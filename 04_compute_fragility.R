# =====================================================================
# 04_Fragility_Index.R
# Computes per-outcome FI (fi_each), p-value (pval), FQ (fq) using fragility
# Produces overall and subgroup summaries via frag.studies (Fisher, two-sided)
# Locks to frozen cohort IDs
# DOES NOT create per-row reverse FI columns (not needed for your workflow)
# =====================================================================

source("00_utils.R")
require_pkgs(c("dplyr", "tidyr", "fragility", "writexl"))

library(dplyr)
library(tidyr)
library(fragility)
library(writexl)

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

# ---------------------------------------------------------------------
# Load outcome-level dataset created in 01_data_cleaning.R
# ---------------------------------------------------------------------
sample_path <- file.path(DIR_OUTPUTS, "sample_long.rds")
if (!file.exists(sample_path)) {
  stop("Missing outputs/sample_long.rds. Run 01_data_cleaning.R first.", call. = FALSE)
}
sample <- readRDS(sample_path)

# ---------------------------------------------------------------------
# Lock to frozen cohort you already analyzed (NO re-sampling)
# ---------------------------------------------------------------------
frozen_path <- file.path(DIR_OUTPUTS, "frozen_sample_ids_2026-02.rds")
if (file.exists(frozen_path)) {
  frozen_ids <- readRDS(frozen_path)
  if (!("record_id" %in% names(frozen_ids))) stop("Frozen IDs missing record_id.", call. = FALSE)
  ids <- as.character(frozen_ids$record_id)
  sample <- sample %>% filter(record_id %in% ids)
  
  message("FI computation locked to frozen cohort: ",
          n_distinct(sample$record_id), " trials; ", nrow(sample), " outcome rows.")
} else {
  message("No frozen IDs found; FI will run on all rows of sample_long.")
}

# ---------------------------------------------------------------------
# Compute FI per outcome row (fragility::frag.study)
# ---------------------------------------------------------------------
fi_set <- sample %>%
  rowwise() %>%
  mutate(
    FI_obj = list(
      frag.study(
        e0 = i_events, n0 = i_total,
        e1 = c_events, n1 = c_total,
        methods = "Fisher"
      )
    ),
    fi_each = as.numeric(FI_obj[["FI"]]),
    pval    = as.numeric(FI_obj[["pval"]]),
    fq      = as.numeric(FI_obj[["FQ"]])
  ) %>%
  ungroup() %>%
  select(-FI_obj)

# ---------------------------------------------------------------------
# Outlier scan (your IQR method) - optional but kept
# ---------------------------------------------------------------------
Q1 <- quantile(fi_set$fi_each, 0.25, na.rm = TRUE)
Q3 <- quantile(fi_set$fi_each, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

extreme_cases <- fi_set %>%
  filter(fi_each < lower_bound | fi_each > upper_bound)

# ---------------------------------------------------------------------
# Overall FI / reverse/forward reporting using frag.studies (text outputs)
# (frag.studies prints the “significant” vs “non-significant” summaries)
# ---------------------------------------------------------------------
overall_fi <- frag.studies(i_events, i_total, c_events, c_total, fi_set, methods = "Fisher")

sink(file.path(DIR_OUTPUTS, "overall_fi_summary_2026-02.txt"))
print(overall_fi)
sink()

# ---------------------------------------------------------------------
# Subgroup FI reporting (participants)
# ---------------------------------------------------------------------
subgroup_levels <- c("Adults", "Pediatric", "Obstetric", "Cardiovascular", "Regional", "Other")

sink(file.path(DIR_OUTPUTS, "subgroup_fi_reports_2026-02.txt"))
for (g in subgroup_levels) {
  cat("\n====================\nGroup:", g, "\n")
  df <- fi_set %>% filter(c_participants.factor == g)
  if (nrow(df) == 0) {
    cat("No outcomes in this subgroup.\n")
  } else {
    print(frag.studies(i_events, i_total, c_events, c_total, df, methods = "Fisher"))
  }
}
sink()

# ---------------------------------------------------------------------
# Save outputs for downstream scripts (especially regression)
# ---------------------------------------------------------------------
saveRDS(fi_set, file.path(DIR_OUTPUTS, "fi_set_2026-02.rds"))

writexl::write_xlsx(
  list(
    fi_outcomes = fi_set,
    extreme_cases = extreme_cases
  ),
  path = file.path(DIR_OUTPUTS, "FI_outputs_2026-02.xlsx")
)

message("04_Fragility_Index.R complete: saved fi_set_2026-02.rds and FI_outputs_2026-02.xlsx.")