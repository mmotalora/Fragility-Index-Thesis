# ---- Packages ----
# (require_pkgs and ensure_dir come from 00_utils.R)
require_pkgs(c("dplyr", "tidyr", "readxl", "writexl"))

# ---- Directories (from 00_run_all.R or fallback) ----
DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

# ---- Input file (from 00_run_all.R) ----
if (!exists("RAW_DATA_FILE")) {
  stop("RAW_DATA_FILE not found. Define it at the top of 00_run_all.R.", call. = FALSE)
}
if (!file.exists(RAW_DATA_FILE)) {
  stop(paste("Cannot find RAW_DATA_FILE:", RAW_DATA_FILE), call. = FALSE)
}

# ---- Read curated master dataset ----
data <- readxl::read_excel(RAW_DATA_FILE)

# ---- Remove REDCap / non-analytic columns (safe even if some are missing) ----
drop_if_present <- function(df, cols) {
  cols <- intersect(cols, names(df))
  dplyr::select(df, -dplyr::all_of(cols))
}

c_data <- data %>%
  drop_if_present(c(
    "redcap_repeat_instrument", "redcap_repeat_instance",
    "id_extrac", "id_extrac.factor",
    "location11","location12","location13","location14","location15","location16",
    "notes_charc_studies",
    "selection_extraction_form_for_individual_studies_complete",
    "selection_extraction_form_for_individual_studies_complete.factor",
    "location59","location60","location61","location62",
    "out_notes",
    "outcomes_complete", "outcomes_complete.factor",
    "data_notes",
    "data_analysis_complete", "data_analysis_complete.factor",
    "redcap_repeat_instrument.factor"
  ))

# Optional: quick summary for interactive review
summary(c_data)

# =====================================================================
# QA checks for inclusion inconsistencies
# =====================================================================
s_type <- c_data %>% dplyr::filter(c_type == "2", included == "1")
intervention <- c_data %>% dplyr::filter(c_intervention == "2", included == "1")
allocation <- c_data %>% dplyr::filter(c_allocation == "2", included == "1")
type_outcome <- c_data %>% dplyr::filter(type_outcome == "2", included == "1")
clusters <- c_data %>% dplyr::filter(u_allocation == "2", included == "1")
non_inferior <- c_data %>% dplyr::filter(rct_type == "2", included == "1")

# Save QA checks (so issues are visible in outputs)
writexl::write_xlsx(
  list(
    c_type_included_yes_but_no = s_type,
    c_intervention_included_yes_but_no = intervention,
    c_allocation_included_yes_but_no = allocation,
    type_outcome_included_yes_but_no = type_outcome,
    clusters_included_yes = clusters,
    non_inferiority_included_yes = non_inferior
  ),
  path = file.path(DIR_OUTPUTS, "qa_inclusion_checks_2026-02.xlsx")
)

# =====================================================================
# Defining subsets
# =====================================================================

# ---- Studies identified from guidelines (eligibility tracking) ----
eligibility <- c_data %>%
  dplyr::select(
    record_id, study_year, report_id, included,
    study_type.factor, c_type.factor, c_intervention.factor,
    c_participants.factor, c_design.factor,
    c_allocation.factor, type_outcome.factor, included.factor
  ) %>%
  tidyr::drop_na(included)

summary(eligibility)

# Identify incomplete records
missing_rows <- which(is.na(eligibility$type_outcome.factor))
eligibility[missing_rows, ]

# ---- Studies eligible for FI analysis (trial characteristics complete) ----
included <- c_data %>%
  dplyr::select(
    record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor,
    c_intervention.factor, c_participants.factor, c_design.factor,
    c_allocation.factor, type_outcome.factor, included,
    rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor,
    rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
    funding.factor, t_nifunding.factor, d_share.factor
  ) %>%
  dplyr::filter(included.factor == "Yes") %>%
  tidyr::drop_na(
    rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor,
    rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
    funding.factor, d_share.factor
  )

summary(included)

# ---- Sample dataset (long outcome-level dataset) ----
# CRITICAL FIX for reproducibility: fill down WITHIN record_id only
sample <- c_data %>%
  dplyr::select(
    record_id, report_id, study_year, included,
    c_participants.factor, c_design.factor,
    rct_aim.factor, rct_type.factor, i_type.factor,
    u_allocation.factor, rct_blind.factor, rct_conceal.factor,
    rct_centers.factor, ethic.factor, funding.factor, d_share.factor,
    miss_data.factor, sample_calc.factor, sample_size,
    i_events, i_total, c_events, c_total, c_missing, i_missing, n_movedi, n_movedc
  ) %>%
  dplyr::group_by(record_id) %>%
  tidyr::fill(
    report_id, study_year, included,
    c_participants.factor, c_design.factor,
    rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor,
    rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
    funding.factor, d_share.factor, miss_data.factor, sample_calc.factor,
    sample_size,
    .direction = "down"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(included == "1") %>%
  tidyr::drop_na(i_events, i_total, c_events, c_total) %>%
  dplyr::distinct(.keep_all = TRUE)

# ---- List of studies included for the analysis (one row per trial) ----
sample_list <- sample %>%
  dplyr::select(
    record_id, report_id, study_year,
    c_participants.factor, c_design.factor, rct_aim.factor, rct_type.factor,
    i_type.factor, u_allocation.factor, rct_blind.factor, rct_conceal.factor,
    rct_centers.factor, ethic.factor, funding.factor, d_share.factor,
    miss_data.factor, sample_calc.factor, sample_size
  ) %>%
  dplyr::distinct(record_id, .keep_all = TRUE) %>%
  dplyr::mutate(across(where(is.factor), droplevels))

# =====================================================================
# Participant-group lists (remove redundancy)
# =====================================================================
make_group_list <- function(df, group_label) {
  df %>%
    dplyr::select(
      record_id, study_year, report_id, included.factor,
      study_type.factor, c_type.factor, c_intervention.factor,
      c_participants.factor, c_design.factor, c_allocation.factor,
      type_outcome.factor, included, rct_aim.factor, rct_type.factor,
      i_type.factor, u_allocation.factor, rct_blind.factor, rct_conceal.factor,
      rct_centers.factor, ethic.factor, funding.factor, t_nifunding.factor,
      d_share.factor
    ) %>%
    dplyr::filter(included.factor == "Yes", c_participants.factor == group_label) %>%
    tidyr::drop_na(
      rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor,
      rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
      funding.factor, d_share.factor
    )
}

general_list   <- make_group_list(c_data, "Adults")
peds_list      <- make_group_list(c_data, "Pediatric")
obstetric_list <- make_group_list(c_data, "Obstetric")
cv_list        <- make_group_list(c_data, "Cardiovascular")
regional_list  <- make_group_list(c_data, "Regional")
other_list     <- make_group_list(c_data, "Other")

# =====================================================================
# Save key objects for downstream scripts
# =====================================================================
saveRDS(c_data, file.path(DIR_OUTPUTS, "c_data.rds"))
saveRDS(eligibility, file.path(DIR_OUTPUTS, "eligibility.rds"))
saveRDS(included, file.path(DIR_OUTPUTS, "included_trials_characteristics.rds"))
saveRDS(sample, file.path(DIR_OUTPUTS, "sample_long.rds"))
saveRDS(sample_list, file.path(DIR_OUTPUTS, "sample_list.rds"))

# Optional: save a combined xlsx of derivatives (handy for review)
writexl::write_xlsx(
  list(
    c_data = c_data,
    eligibility = eligibility,
    included_trials = included,
    sample_long = sample,
    sample_list = sample_list
  ),
  path = file.path(DIR_OUTPUTS, "cleaned_derivatives_2026-02.xlsx")
)

message("01_data_cleaning.R complete: created c_data, eligibility, included, sample, sample_list and saved to outputs/.")