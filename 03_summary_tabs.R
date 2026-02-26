# =====================================================================
# 03_summary_tabs.R  (FINAL)
#
# Table 1 MUST describe the SAMPLED cohort characteristics (trial-level),
# and should NOT use regression-only datasets (e.g., binomdat).
# Therefore:
#  - Table 1 uses sample_list (trial-level, one row per trial)
#  - LTFU + moved are computed at trial-level (row-wise) from available fields
#  - Crossover remains its own category for reporting (no collapsing here)
#
# Also exports:
#  - SDC 3: Sampled RCT list with group N, events, and event rates (%) (no FI cols)
# =====================================================================

source("00_utils.R")
require_pkgs(c("dplyr", "tidyr", "writexl", "forcats"))

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

DIR_TABLES <- file.path(DIR_OUTPUTS, "tables")
ensure_dir(DIR_TABLES)

# ---------------------------------------------------------------------
# Load inputs created by earlier scripts
# ---------------------------------------------------------------------
sample_list_path <- file.path(DIR_OUTPUTS, "sample_list.rds")
fi_set_path      <- file.path(DIR_OUTPUTS, "fi_set_2026-02.rds")

if (!file.exists(sample_list_path)) stop("Missing sample_list.rds. Run 01_data_cleaning.R.", call. = FALSE)
if (!file.exists(fi_set_path))      stop("Missing fi_set_2026-02.rds. Run 04_Fragility_Index.R.", call. = FALSE)

sample_list <- readRDS(sample_list_path)
fi_set      <- readRDS(fi_set_path)

# ---------------------------------------------------------------------
# Frozen sample IDs (LOCKED cohort)
# ---------------------------------------------------------------------
frozen_ids_path <- file.path(DIR_OUTPUTS, "frozen_sample_ids_2026-02.rds")
has_frozen <- file.exists(frozen_ids_path)

if (has_frozen) {
  frozen_ids <- readRDS(frozen_ids_path)
  if (!("record_id" %in% names(frozen_ids))) stop("Frozen IDs file missing record_id.", call. = FALSE)
  ids <- as.character(frozen_ids$record_id)
  
  if ("record_id" %in% names(sample_list)) sample_list <- dplyr::filter(sample_list, record_id %in% ids)
  if ("record_id" %in% names(fi_set))      fi_set      <- dplyr::filter(fi_set,      record_id %in% ids)
} else {
  warning("Frozen sample IDs not found. Table 1 / SDC3 will reflect current sample_list/fi_set contents.")
}

# ---------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------
median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  med <- stats::median(x)
  q1  <- stats::quantile(x, 0.25, names = FALSE)
  q3  <- stats::quantile(x, 0.75, names = FALSE)
  sprintf("%.1f [%.1f, %.1f]", med, q1, q3)
}

summarise_cont_by <- function(df, group_var, cont_vars) {
  out <- lapply(cont_vars, function(v) {
    df %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarise(stat = median_iqr(.data[[v]]), .groups = "drop") %>%
      dplyr::mutate(variable = v) %>%
      dplyr::select(variable, !!group_var, stat)
  })
  dplyr::bind_rows(out) %>%
    tidyr::pivot_wider(names_from = !!group_var, values_from = stat)
}

summarise_cat_by <- function(df, group_var, cat_vars) {
  out <- lapply(cat_vars, function(v) {
    df %>%
      dplyr::filter(!is.na(.data[[v]]), !is.na(.data[[group_var]])) %>%
      dplyr::count(.data[[group_var]], .data[[v]]) %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::mutate(pct = 100 * n / sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        stat = sprintf("%d (%.1f%%)", n, pct),
        variable = v,
        level = as.character(.data[[v]])
      ) %>%
      dplyr::select(variable, level, !!group_var, stat) %>%
      tidyr::pivot_wider(names_from = !!group_var, values_from = stat)
  })
  dplyr::bind_rows(out)
}

safe_rate_pct <- function(e, n) {
  e <- suppressWarnings(as.numeric(e))
  n <- suppressWarnings(as.numeric(n))
  out <- ifelse(is.na(e) | is.na(n) | n <= 0, NA_real_, 100 * e / n)
  round(out, 1)
}

pick_col <- function(df, choices, default = NA) {
  nm <- intersect(choices, names(df))
  if (length(nm) == 0) return(rep(default, nrow(df)))
  df[[nm[1]]]
}

# ---------------------------------------------------------------------
# Build a TRIAL-LEVEL table for Table 1:
# sample_list is trial-level but may not include missing/moved fields.
# We safely LEFT JOIN trial-level ltfu/moved computed from fi_set.
# (fi_set is outcome-level; we reduce to unique trial record_id.)
# ---------------------------------------------------------------------

ltfu_moved_trial <- fi_set %>%
  dplyr::mutate(
    c_missing = suppressWarnings(as.numeric(c_missing)),
    i_missing = suppressWarnings(as.numeric(i_missing)),
    n_movedi  = suppressWarnings(as.numeric(n_movedi)),
    n_movedc  = suppressWarnings(as.numeric(n_movedc)),
    ltfu  = row_sum2(c_missing, i_missing),
    moved = row_sum2(n_movedi, n_movedc)
  ) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(
    ltfu  = dplyr::first(ltfu),
    moved = dplyr::first(moved),
    .groups = "drop"
  )

table1_df <- sample_list %>%
  dplyr::left_join(ltfu_moved_trial, by = "record_id")

# =====================================================================
# TABLE 1: Sample characteristics (from sample_list; NOT regression data)
# =====================================================================

# Categorical (keep original categories for reporting; crossover stays)
tab1_cat <- summarise_cat_by(
  table1_df,
  group_var = "i_type.factor",
  cat_vars = c(
    "c_participants.factor",
    "c_design.factor",
    "rct_aim.factor",
    "rct_type.factor",
    "u_allocation.factor",
    "rct_blind.factor",
    "rct_conceal.factor",
    "rct_centers.factor",
    "ethic.factor",
    "funding.factor",
    "d_share.factor",
    "miss_data.factor",
    "sample_calc.factor"
  )
)

# Continuous (MEDIAN [IQR]) with reviewer-requested ltfu + moved
tab1_cont <- summarise_cont_by(
  table1_df,
  group_var = "i_type.factor",
  cont_vars = c("study_year", "sample_size", "ltfu", "moved")
)

# N per intervention type
tab1_n <- table1_df %>%
  dplyr::count(i_type.factor, name = "N") %>%
  tidyr::pivot_wider(names_from = i_type.factor, values_from = N) %>%
  dplyr::mutate(variable = "N", .before = 1)

# =====================================================================
# EXPORT Table 1
# =====================================================================

out_file <- file.path(DIR_OUTPUTS, "Summary_Tables_2026-02.xlsx")

writexl::write_xlsx(
  list(
    Table1_sample_counts = tab1_cat,
    Table1_sample_continuous_median_IQR = tab1_cont,
    Table1_sample_N = tab1_n
  ),
  path = out_file
)

# =====================================================================
# SDC 3: Sampled RCTs with event rates (%) and sample sizes (LOCKED; no FI cols)
# =====================================================================

sdc3 <- fi_set %>%
  dplyr::mutate(
    i_total_num  = suppressWarnings(as.numeric(i_total)),
    c_total_num  = suppressWarnings(as.numeric(c_total)),
    i_events_num = suppressWarnings(as.numeric(i_events)),
    c_events_num = suppressWarnings(as.numeric(c_events)),
    i_rate_pct   = safe_rate_pct(i_events_num, i_total_num),
    c_rate_pct   = safe_rate_pct(c_events_num, c_total_num)
  ) %>%
  dplyr::transmute(
    record_id = pick_col(cur_data(), c("record_id")),
    Year      = pick_col(cur_data(), c("study_year")),
    Report_ID = pick_col(cur_data(), c("report_id")),
    Study_reference = pick_col(cur_data(), c("study_reference")),
    Outcome   = pick_col(cur_data(), c("out_name")),
    Outcome_definition = pick_col(cur_data(), c("out_definition")),
    
    Participants = dplyr::coalesce(
      as.character(pick_col(cur_data(), c("c_participants.factor"), default = NA)),
      as.character(pick_col(cur_data(), c("c_participants"), default = NA))
    ),
    
    Design = dplyr::coalesce(
      as.character(pick_col(cur_data(), c("c_design.factor"), default = NA)),
      as.character(pick_col(cur_data(), c("c_design"), default = NA))
    ),
    
    Trial_type = dplyr::coalesce(
      as.character(pick_col(cur_data(), c("rct_type.factor"), default = NA)),
      as.character(pick_col(cur_data(), c("rct_type"), default = NA))
    ),
    
    Intervention_type = dplyr::coalesce(
      as.character(pick_col(cur_data(), c("i_type.factor"), default = NA)),
      as.character(pick_col(cur_data(), c("i_type"), default = NA))
    ),
    
    Intervention_N               = i_total_num,
    Intervention_events          = i_events_num,
    Intervention_event_rate_pct  = i_rate_pct,
    
    Control_N                    = c_total_num,
    Control_events               = c_events_num,
    Control_event_rate_pct       = c_rate_pct
  ) %>%
  dplyr::arrange(dplyr::desc(Year), Report_ID)

# QC check: POISE-3 (record_id = 253)
poise3_check <- sdc3 %>% dplyr::filter(record_id == 253)
if (nrow(poise3_check) == 0) {
  warning("POISE-3 (record_id = 253) not found in SDC3 export.")
} else {
  message("POISE-3 found in SDC3 export. Check Design column is 'Two-by-two factorial trial'.")
  print(poise3_check[, c("record_id","Year","Study_reference","Design")])
}

sdc3_file <- file.path(DIR_TABLES, "SDC3_Sampled_RCTs_EventRates_2026-02.xlsx")
writexl::write_xlsx(sdc3, path = sdc3_file)

message("03_summary_tabs.R complete. Wrote: ", out_file)
message("SDC 3 written: ", sdc3_file)
if (has_frozen) message("Table 1 and SDC 3 were locked to frozen cohort IDs.")