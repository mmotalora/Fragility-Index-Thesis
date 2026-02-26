# =====================================================================
# 05_analysis.R
# Negative binomial regression (FI as outcome)
#
# FINAL DECISIONS:
# - NO pval_significance * sample_size interaction
# - rct_aim.factor removed (collinearity with rct_type.factor)
# - Use dichotomous blinding and concealment only
# - Collapse crossover into "Other" ONLY for regression (c_design_reg.factor)
# - Fix LTFU and moved (row-wise)
# - Log complete-case losses due to drop_na()
# - Save datasets + model objects for reproducibility
# =====================================================================

source("00_utils.R")

require_pkgs(c(
  "MASS", "car", "MuMIn", "dplyr", "corrplot", "vcd", "tidyr",
  "psych", "forcats", "readr"
))

library(MASS)
library(car)
library(MuMIn)
library(dplyr)
library(corrplot)
library(vcd)
library(tidyr)
library(psych)
library(forcats)
library(readr)

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

# ---------------------------------------------------------------------
# Load FI dataset (fi_set) created in 04_compute_fragility.R
# ---------------------------------------------------------------------
if (!exists("fi_set")) {
  fi_path <- file.path(DIR_OUTPUTS, "fi_set_2026-02.rds")
  if (!file.exists(fi_path)) stop("fi_set not found. Run 04_compute_fragility.R first.", call. = FALSE)
  fi_set <- readRDS(fi_path)
}

# ---------------------------------------------------------------------
# Prepare dataset for modeling
# ---------------------------------------------------------------------
binomdat <- fi_set %>%
  mutate(across(where(is.factor), droplevels))

# -------------------------
# Dichotomous BLINDING (Yes/No)
# -------------------------
binomdat <- binomdat %>%
  mutate(
    rct_blind_d.factor = factor(
      rct_blind.factor,
      levels = c("Single blind", "Double blind", "Triple blind", "Unblinded"),
      labels = c("Yes", "Yes", "Yes", "No")
    )
  )

# -------------------------
# Dichotomous CONCEALMENT (Yes vs Unclear)
# -------------------------
binomdat <- binomdat %>%
  mutate(
    rct_conceal_d.factor = factor(
      rct_conceal.factor,
      levels = c(
        "Sequentially numbered sealed/opaque envelopes",
        "Sequentially numbered containers",
        "Pharmacy controlled allocation",
        "Central allocation (site remote from trial location)",
        "Other",
        "Unclear"
      ),
      labels = c("Yes", "Yes", "Yes", "Yes", "Yes", "Unclear")
    )
  )

# -------------------------
# P-value significance
# -------------------------
binomdat <- binomdat %>%
  mutate(
    pval_significance = factor(
      ifelse(pval < 0.05, "Significant", "Non-significant"),
      levels = c("Non-significant", "Significant")
    )
  )

# -------------------------
# Correct row-wise LTFU and moved using your helper
# -------------------------
binomdat <- binomdat %>%
  mutate(
    ltfu  = row_sum2(c_missing, i_missing),
    moved = row_sum2(n_movedi, n_movedc)
  )

# -------------------------
# Collapse crossover ONLY for regression
# Keep original c_design.factor unchanged for Table 1
# -------------------------
binomdat <- binomdat %>%
  mutate(
    c_design_reg.factor = forcats::fct_collapse(
      c_design.factor,
      Other = c("Other", "Crossover")
    )
  )

# ---------------------------------------------------------------------
# Build complete-case dataset used for regression + diagnostics
# ---------------------------------------------------------------------
n_before <- nrow(binomdat)

no.na.data <- binomdat %>%
  dplyr::select(
    dplyr::any_of("record_id"),
    fi_each, fq, pval, pval_significance,
    study_year, sample_size, ltfu, moved,
    c_participants.factor,
    c_design_reg.factor,
    rct_type.factor,
    i_type.factor,
    u_allocation.factor,
    rct_blind_d.factor,
    rct_conceal_d.factor,
    rct_centers.factor,
    ethic.factor,
    funding.factor,
    d_share.factor,
    miss_data.factor,
    sample_calc.factor
  ) %>%
  tidyr::drop_na()

n_after <- nrow(no.na.data)

writeLines(
  c(
    "Complete-case selection (05_analysis.R)",
    sprintf("Rows in binomdat before drop_na(): %d", n_before),
    sprintf("Rows in no.na.data after drop_na(): %d", n_after),
    sprintf("Rows excluded due to missingness: %d", n_before - n_after),
    sprintf("Date/time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ),
  con = file.path(DIR_OUTPUTS, "complete_case_flow_2026-02.txt")
)

saveRDS(binomdat,   file.path(DIR_OUTPUTS, "binomdat.rds"))
saveRDS(no.na.data, file.path(DIR_OUTPUTS, "no_na_data.rds"))

# ---------------------------------------------------------------------
# Correlation checks (continuous)
# ---------------------------------------------------------------------
continuous_vars <- no.na.data %>%
  dplyr::select(sample_size, pval, ltfu, moved)

correlation_matrix <- cor(continuous_vars, use = "complete.obs")
png(file.path(DIR_OUTPUTS, "corrplot_continuous_2026-02.png"), width = 1600, height = 1200, res = 200)
corrplot(correlation_matrix, method = "circle")
dev.off()

# ---------------------------------------------------------------------
# Correlation checks (categorical) using Cramér's V
# ---------------------------------------------------------------------
cramers_v_matrix <- function(data) {
  cat_vars <- data[, sapply(data, is.factor), drop = FALSE]
  var_names <- colnames(cat_vars)
  n <- length(var_names)
  
  out <- matrix(NA, nrow = n, ncol = n, dimnames = list(var_names, var_names))
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])
      if (min(dim(tbl)) > 1) {
        out[i, j] <- vcd::assocstats(tbl)$cramer
        out[j, i] <- out[i, j]
      }
    }
  }
  out
}

categorical_vars <- no.na.data %>%
  dplyr::select(where(is.factor))  # <- your preferred approach

cat_correlation_matrix <- cramers_v_matrix(categorical_vars)

png(file.path(DIR_OUTPUTS, "corrplot_categorical_cramersV_2026-02.png"), width = 1800, height = 1600, res = 200)
corrplot(cat_correlation_matrix, method = "circle", na.label = "NA")
dev.off()

# ---------------------------------------------------------------------
# Optional ANOVA checks (descriptive)
# ---------------------------------------------------------------------
anova_checks <- list(
  sample_size_by_participants = summary(aov(sample_size ~ c_participants.factor, data = no.na.data)),
  sample_size_by_rct_type     = summary(aov(sample_size ~ rct_type.factor, data = no.na.data)),
  sample_size_by_i_type       = summary(aov(sample_size ~ i_type.factor, data = no.na.data)),
  sample_size_by_centers      = summary(aov(sample_size ~ rct_centers.factor, data = no.na.data))
)

sink(file.path(DIR_OUTPUTS, "anova_checks_2026-02.txt"))
print(anova_checks)
sink()

# ---------------------------------------------------------------------
# Univariate negative binomial models (updated set)
# ---------------------------------------------------------------------
glm1  <- glm.nb(fi_each ~ study_year, data = no.na.data)
glm2  <- glm.nb(fi_each ~ c_participants.factor, data = no.na.data)
glm3  <- glm.nb(fi_each ~ c_design_reg.factor, data = no.na.data)
glm5  <- glm.nb(fi_each ~ rct_type.factor, data = no.na.data)
glm6  <- glm.nb(fi_each ~ i_type.factor, data = no.na.data)
glm8  <- glm.nb(fi_each ~ rct_blind_d.factor, data = no.na.data)
glm10 <- glm.nb(fi_each ~ rct_conceal_d.factor, data = no.na.data)
glm11 <- glm.nb(fi_each ~ rct_centers.factor, data = no.na.data)
glm12 <- glm.nb(fi_each ~ ethic.factor, data = no.na.data)
glm13 <- glm.nb(fi_each ~ funding.factor, data = no.na.data)
glm14 <- glm.nb(fi_each ~ d_share.factor, data = no.na.data)
glm15 <- glm.nb(fi_each ~ pval, data = no.na.data)
glm16 <- glm.nb(fi_each ~ pval_significance, data = no.na.data)
glm17 <- glm.nb(fi_each ~ ltfu, data = no.na.data)
glm18 <- glm.nb(fi_each ~ moved, data = no.na.data)
glm19 <- glm.nb(fi_each ~ sample_size, data = no.na.data)

univ_summaries <- list(
  glm1  = capture.output(summary(glm1)),
  glm2  = capture.output(summary(glm2)),
  glm3  = capture.output(summary(glm3)),
  glm5  = capture.output(summary(glm5)),
  glm6  = capture.output(summary(glm6)),
  glm8  = capture.output(summary(glm8)),
  glm10 = capture.output(summary(glm10)),
  glm11 = capture.output(summary(glm11)),
  glm12 = capture.output(summary(glm12)),
  glm13 = capture.output(summary(glm13)),
  glm14 = capture.output(summary(glm14)),
  glm15 = capture.output(summary(glm15)),
  glm16 = capture.output(summary(glm16)),
  glm17 = capture.output(summary(glm17)),
  glm18 = capture.output(summary(glm18)),
  glm19 = capture.output(summary(glm19))
)

writeLines(
  unlist(Map(function(nm, x) c(paste0("\n==== ", nm, " ====\n"), x),
             names(univ_summaries), univ_summaries)),
  con = file.path(DIR_OUTPUTS, "univariate_nb_models_2026-02.txt")
)

# ---------------------------------------------------------------------
# Final multivariable model (NO interaction)
# ---------------------------------------------------------------------
final_model <- MASS::glm.nb(
  fi_each ~ c_participants.factor +
    c_design_reg.factor +
    rct_type.factor +
    i_type.factor +
    rct_blind_d.factor +
    rct_conceal_d.factor +
    rct_centers.factor +
    ethic.factor +
    funding.factor +
    d_share.factor +
    pval_significance +
    sample_size +
    ltfu + moved,
  data = no.na.data
)

saveRDS(final_model, file.path(DIR_OUTPUTS, "final_nb_model_2026-02.rds"))
writeLines(capture.output(summary(final_model)),
           file.path(DIR_OUTPUTS, "final_nb_model_summary_2026-02.txt"))

# ---------------------------------------------------------------------
# VIF diagnostic (approximate via lm on same formula)
# ---------------------------------------------------------------------
vif_lm <- lm(
  fi_each ~ c_participants.factor +
    c_design_reg.factor +
    rct_type.factor +
    i_type.factor +
    rct_blind_d.factor +
    rct_conceal_d.factor +
    rct_centers.factor +
    ethic.factor +
    funding.factor +
    d_share.factor +
    pval_significance +
    sample_size +
    ltfu + moved,
  data = no.na.data
)

vif_vals <- car::vif(vif_lm)
writeLines(capture.output(vif_vals),
           file.path(DIR_OUTPUTS, "vif_diagnostic_2026-02.txt"))

message("05_analysis.R complete: no interaction term; complete-case loss logged; model + datasets saved to outputs/.")