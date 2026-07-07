# =====================================================================
# SDC3_SDC4_superiority.R
#
# Creates updated Supplemental Digital Content 3 and 4 for the
# superiority-trial analytic cohort.
#
# SDC 3: sampled vs non-sampled eligible superiority trials
# SDC 4: complete list of sampled superiority trials included in analysis
# =====================================================================

source("00_utils.R")
require_pkgs(c("dplyr", "tidyr", "writexl", "forcats"))

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

DIR_TABLES <- file.path(DIR_OUTPUTS, "tables")
ensure_dir(DIR_TABLES)

# ---------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------

sample_list_path <- file.path(DIR_OUTPUTS, "sample_list.rds")
included_path    <- file.path(DIR_OUTPUTS, "included_trials_characteristics.rds")
fi_sup_path      <- file.path(DIR_OUTPUTS, "fi_set_superiority_only.rds")

if (!file.exists(sample_list_path)) stop("Missing sample_list.rds.", call. = FALSE)
if (!file.exists(included_path))    stop("Missing included_trials_characteristics.rds.", call. = FALSE)
if (!file.exists(fi_sup_path))      stop("Missing fi_set_superiority_only.rds.", call. = FALSE)

sample_list     <- readRDS(sample_list_path)
included_trials <- readRDS(included_path)
fi_sup          <- readRDS(fi_sup_path)

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------

median_iqr <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  sprintf(
    "%.1f [%.1f, %.1f]",
    median(x),
    quantile(x, 0.25, names = FALSE),
    quantile(x, 0.75, names = FALSE)
  )
}

n_pct <- function(n, denom) {
  ifelse(
    is.na(n) | is.na(denom) | denom == 0,
    NA_character_,
    sprintf("%d (%.1f%%)", n, 100 * n / denom)
  )
}

safe_rate_pct <- function(e, n) {
  e <- suppressWarnings(as.numeric(e))
  n <- suppressWarnings(as.numeric(n))
  ifelse(is.na(e) | is.na(n) | n <= 0, NA_real_, round(100 * e / n, 1))
}

pick_col <- function(df, choices, default = NA) {
  nm <- intersect(choices, names(df))
  if (length(nm) == 0) return(rep(default, nrow(df)))
  df[[nm[1]]]
}

# ---------------------------------------------------------------------
# Identify eligible and sampled superiority trials
# ---------------------------------------------------------------------

sampled_ids <- unique(as.character(sample_list$record_id))

eligible_sup <- included_trials %>%
  dplyr::mutate(record_id = as.character(record_id)) %>%
  dplyr::filter(as.character(rct_type.factor) == "Superiority trial") %>%
  dplyr::distinct(record_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    sampling_group = ifelse(
      record_id %in% sampled_ids,
      "Sampled superiority trials",
      "Eligible superiority trials not sampled"
    )
  )

sampled_sup_ids <- eligible_sup %>%
  dplyr::filter(sampling_group == "Sampled superiority trials") %>%
  dplyr::pull(record_id)

message("Eligible superiority trials: ", nrow(eligible_sup))
message("Sampled superiority trials: ", length(sampled_sup_ids))
message("Non-sampled eligible superiority trials: ", nrow(eligible_sup) - length(sampled_sup_ids))

if (length(sampled_sup_ids) != 161) {
  warning("Sampled superiority cohort is not 161. Check filtering and record IDs.")
}

# =====================================================================
# SDC 3: sampled vs non-sampled eligible superiority trials
# =====================================================================

sdc3_df <- eligible_sup %>%
  dplyr::mutate(
    study_year = suppressWarnings(as.numeric(study_year)),
    year_group = dplyr::case_when(
      study_year >= 1983 & study_year <= 1992 ~ "1983–1992",
      study_year >= 1993 & study_year <= 2002 ~ "1993–2002",
      study_year >= 2003 & study_year <= 2012 ~ "2003–2012",
      study_year >= 2013 & study_year <= 2022 ~ "2013–2022",
      TRUE ~ NA_character_
    ),
    year_group = factor(
      year_group,
      levels = c("1983–1992", "1993–2002", "2003–2012", "2013–2022")
    )
  )

make_cat_comparison <- function(df, var, label) {
  df %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(sampling_group)) %>%
    dplyr::count(sampling_group, level = as.character(.data[[var]]), name = "n") %>%
    dplyr::group_by(sampling_group) %>%
    dplyr::mutate(
      denom = sum(n),
      stat = n_pct(n, denom)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(sampling_group, level, stat) %>%
    tidyr::pivot_wider(names_from = sampling_group, values_from = stat) %>%
    dplyr::mutate(
      Variable = label,
      Level = level,
      .before = 1
    ) %>%
    dplyr::select(-level)
}

make_cont_comparison <- function(df, var, label) {
  df %>%
    dplyr::group_by(sampling_group) %>%
    dplyr::summarise(stat = median_iqr(.data[[var]]), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = sampling_group, values_from = stat) %>%
    dplyr::mutate(
      Variable = label,
      Level = "Median [IQR]",
      .before = 1
    )
}

sdc3_n <- sdc3_df %>%
  dplyr::count(sampling_group, name = "N") %>%
  tidyr::pivot_wider(names_from = sampling_group, values_from = N) %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      as.character
    )
  ) %>%
  dplyr::mutate(
    Variable = "N",
    Level = "",
    .before = 1
  )

sdc3_cat <- dplyr::bind_rows(
  make_cat_comparison(sdc3_df, "year_group", "Year of publication"),
  make_cat_comparison(sdc3_df, "c_participants.factor", "Perioperative population"),
  make_cat_comparison(sdc3_df, "c_design.factor", "Trial design"),
  make_cat_comparison(sdc3_df, "i_type.factor", "Intervention type"),
  make_cat_comparison(sdc3_df, "rct_blind.factor", "Blinding"),
  make_cat_comparison(sdc3_df, "rct_conceal.factor", "Allocation concealment"),
  make_cat_comparison(sdc3_df, "rct_centers.factor", "Centres")
)


sdc3 <- dplyr::bind_rows(
  sdc3_n,
  sdc3_cat
)

# =====================================================================
# SDC 4: complete list of sampled superiority trials
# =====================================================================

fi_sup <- fi_sup %>%
  dplyr::mutate(record_id = as.character(record_id)) %>%
  dplyr::filter(record_id %in% sampled_sup_ids) %>%
  dplyr::distinct(record_id, .keep_all = TRUE)

if (nrow(fi_sup) != 161) {
  warning("SDC4 source is not 161 after filtering. Check fi_set_superiority_only.rds.")
}

sdc4 <- fi_sup %>%
  dplyr::mutate(
    i_total_num  = suppressWarnings(as.numeric(i_total)),
    c_total_num  = suppressWarnings(as.numeric(c_total)),
    i_events_num = suppressWarnings(as.numeric(i_events)),
    c_events_num = suppressWarnings(as.numeric(c_events)),
    i_rate_pct   = safe_rate_pct(i_events_num, i_total_num),
    c_rate_pct   = safe_rate_pct(c_events_num, c_total_num)
  ) %>%
  dplyr::transmute(
    record_id = record_id,
    Year = study_year,
    Report_ID = report_id,
    Study_reference = pick_col(cur_data(), c("study_reference")),
    Outcome = pick_col(cur_data(), c("out_name")),
    Outcome_definition = pick_col(cur_data(), c("out_definition")),
    Population = as.character(c_participants.factor),
    Trial_design = as.character(c_design.factor),
    Trial_type = as.character(rct_type.factor),
    Intervention_type = as.character(i_type.factor),
    Blinding = as.character(rct_blind.factor),
    Allocation_concealment = as.character(rct_conceal.factor),
    Centres = as.character(rct_centers.factor),
    Funding = as.character(funding.factor),
    Data_sharing_statement = as.character(d_share.factor),
    Total_sample_size = suppressWarnings(as.numeric(sample_size)),
    Intervention_N = i_total_num,
    Intervention_events = i_events_num,
    Intervention_event_rate_pct = i_rate_pct,
    Control_N = c_total_num,
    Control_events = c_events_num,
    Control_event_rate_pct = c_rate_pct
  ) %>%
  dplyr::arrange(dplyr::desc(Year), Report_ID)

if (!all(as.character(sdc4$Trial_type) == "Superiority trial")) {
  warning("SDC4 includes non-superiority trials. Check filtering.")
}

# =====================================================================
# Export
# =====================================================================

sdc3_file <- file.path(
  DIR_TABLES,
  "SDC3_Sampled_vs_Nonsampled_Eligible_Superiority_RCTs.xlsx"
)

sdc4_file <- file.path(
  DIR_TABLES,
  "SDC4_Sampled_Superiority_RCTs_Included_in_Primary_Analysis.xlsx"
)

writexl::write_xlsx(
  list("SDC3_comparison" = sdc3),
  path = sdc3_file
)

writexl::write_xlsx(
  list("SDC4_sampled_superiority_RCTs" = sdc4),
  path = sdc4_file
)

message("SDC 3 written: ", sdc3_file)
message("SDC 4 written: ", sdc4_file)