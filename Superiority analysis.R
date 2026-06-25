# ============================================================
# Sensitivity analysis: exclude equivalence trials
# ============================================================

library(dplyr)
library(MASS)
library(broom)
library(writexl)

# Load FI dataset
fi_set <- readRDS("outputs/fi_set_2026-02.rds")

table(fi_set$rct_aim.factor,
      fi_set$rct_type.factor)
# rct_type.factor is capturing the actual statistical design, 
# whereas rct_aim.factor may reflect how the authors described the study objective.


# Check trial type variable
table(fi_set$rct_type.factor, useNA = "ifany")

# Confirm equivalence count
fi_set %>%
  count(rct_type.factor)

# Create superiority-only dataset
fi_superiority <- fi_set %>%
  filter(rct_type.factor != "Equivalence trial")

fi_superiority <- fi_set %>%
  filter(rct_type.factor == "Superiority trial")

# Check sample size
n_distinct(fi_set$record_id)
n_distinct(fi_superiority$record_id)


# ------------------------------------------------------------
# Helper function for median [IQR]
# ------------------------------------------------------------
median_iqr <- function(x) {
  x <- x[!is.na(x)]
  paste0(
    median(x), " [",
    quantile(x, 0.25), "–",
    quantile(x, 0.75), "]"
  )
}

# ------------------------------------------------------------
# Compare original vs superiority-only
# ------------------------------------------------------------
comparison <- tibble(
  Analysis = c("Original sample", "Superiority-only"),
  N_trials = c(
    n_distinct(fi_set$record_id),
    n_distinct(fi_superiority$record_id)
  ),
  Median_sample_size = c(
    median_iqr(fi_set$sample_size),
    median_iqr(fi_superiority$sample_size)
  ),
  Median_FI = c(
    median_iqr(fi_set$fi_each),
    median_iqr(fi_superiority$fi_each)
  ),
  Median_FQ = c(
    median_iqr(fi_set$fq),
    median_iqr(fi_superiority$fq)
  )
)

comparison

# ------------------------------------------------------------
# Compare equivalence vs superiority trials
# ------------------------------------------------------------
by_trial_type <- fi_set %>%
  group_by(rct_type.factor) %>%
  summarise(
    n_trials = n_distinct(record_id),
    sample_size = median_iqr(sample_size),
    FI = median_iqr(fi_each),
    FQ = median_iqr(fq),
    .groups = "drop"
  )

by_trial_type

# ------------------------------------------------------------
# Recreate variables for negative binomial model
# ------------------------------------------------------------
make_model_data <- function(df) {
  df %>%
    mutate(
      fi_each = as.numeric(fi_each),
      
      year_group = factor(
        ifelse(study_year < 2008, "Before 2008", "2008 or Later"),
        levels = c("Before 2008", "2008 or Later")
      ),
      
      pval_significance = factor(
        ifelse(pval < 0.05, "Significant", "Non-significant"),
        levels = c("Non-significant", "Significant")
      ),
      
      c_design_collapsed.factor = factor(
        ifelse(
          c_design.factor %in% c("Parallel-arm trial", "Two-by-two factorial trial"),
          as.character(c_design.factor),
          "Other"
        ),
        levels = c("Parallel-arm trial", "Two-by-two factorial trial", "Other")
      ),
      
      rct_blind_d.factor = factor(
        ifelse(
          rct_blind.factor %in% c("Single blind", "Double blind", "Triple blind"),
          "Yes",
          ifelse(rct_blind.factor == "Unblinded", "No", NA_character_)
        )
      ),
      
      rct_conceal_d.factor = factor(
        ifelse(rct_conceal.factor == "Unclear", "Unclear", "Yes")
      )
    )
}

model_superiority <- make_model_data(fi_superiority)

names(model_superiority)

model_superiority <- model_superiority %>%
  mutate(
    ltfu = coalesce(c_missing, 0) + coalesce(i_missing, 0),
    moved = coalesce(n_movedi, 0) + coalesce(n_movedc, 0)
  )
# ------------------------------------------------------------
# Negative binomial model, excluding equivalence trials
# Note: rct_type.factor is removed because only superiority trials remain
# ------------------------------------------------------------
nb_superiority <- glm.nb(
  fi_each ~ year_group +
    c_participants.factor +
    c_design_collapsed.factor +
    i_type.factor +
    rct_blind_d.factor +
    rct_conceal_d.factor +
    rct_centers.factor +
    ethic.factor +
    funding.factor +
    d_share.factor +
    pval_significance +
    sample_size +
    ltfu +
    moved,
  data = model_superiority
)

summary(nb_superiority)

# IRR table
irr_superiority <- tidy(
  nb_superiority,
  conf.int = TRUE,
  exponentiate = TRUE
)

irr_superiority

# ------------------------------------------------------------
# Export results
# ------------------------------------------------------------
write_xlsx(
  list(
    comparison_original_vs_superiority = comparison,
    by_trial_type = by_trial_type,
    nb_superiority_IRR = irr_superiority
  ),
  path = "outputs/sensitivity_excluding_equivalence_trials.xlsx"
)

saveRDS(fi_superiority, "outputs/fi_set_superiority_only.rds")
saveRDS(nb_superiority, "outputs/nb_fit_superiority_only.rds")
