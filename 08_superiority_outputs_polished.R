# =====================================================================
# 03_summary_tabs.R  (SUPERIORITY-ONLY REVISION)
#
# Table 1 describes the PRIMARY ANALYTIC COHORT:
# superiority trials only, using trial-level sample_list.
#
# Reviewer/editor revisions:
# - Primary cohort excludes equivalence trials
# - Table 1 reports rct_type.factor, not rct_aim.factor
# - LTFU + moved computed trial-level
# - SDC 3 includes event rates (%) and group sample sizes
# =====================================================================

source("00_utils.R")
require_pkgs(c("dplyr", "tidyr", "writexl", "forcats"))

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)

DIR_TABLES <- file.path(DIR_OUTPUTS, "tables")
ensure_dir(DIR_TABLES)

# ---------------------------------------------------------------------
# Load inputs
# ---------------------------------------------------------------------

sample_list_path <- file.path(DIR_OUTPUTS, "sample_list.rds")
fi_set_path      <- file.path(DIR_OUTPUTS, "fi_set_2026-02.rds")
fi_sup_path      <- file.path(DIR_OUTPUTS, "fi_set_superiority_only.rds")

if (!file.exists(sample_list_path)) stop("Missing sample_list.rds. Run 01_import_clean.R.", call. = FALSE)
if (!file.exists(fi_set_path)) stop("Missing fi_set_2026-02.rds. Run 04_compute_fragility.R.", call. = FALSE)

sample_list <- readRDS(sample_list_path)
fi_set_full <- readRDS(fi_set_path)

# Prefer already-created superiority-only file
if (file.exists(fi_sup_path)) {
  fi_set <- readRDS(fi_sup_path)
} else {
  fi_set <- fi_set_full %>%
    dplyr::filter(rct_type.factor == "Superiority trial")
}

# Restrict sample_list to superiority-only record IDs
superiority_ids <- unique(as.character(fi_set$record_id))

sample_list <- sample_list %>%
  dplyr::filter(as.character(record_id) %in% superiority_ids)

# QC
message("Primary cohort for Table 1/SDC3:")
message("Trials in fi_set: ", dplyr::n_distinct(fi_set$record_id))
print(table(fi_set$rct_type.factor, useNA = "ifany"))

# ---------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------

if (!exists("row_sum2")) {
  row_sum2 <- function(a, b) {
    a <- suppressWarnings(as.numeric(a))
    b <- suppressWarnings(as.numeric(b))
    a[is.na(a)] <- 0
    b[is.na(b)] <- 0
    a + b
  }
}

median_iqr_or_range <- function(x, var_name) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  
  if (var_name == "moved") {
    return(sprintf("%s - %s", min(x), max(x)))
  }
  
  med <- stats::median(x)
  q1  <- stats::quantile(x, 0.25, names = FALSE)
  q3  <- stats::quantile(x, 0.75, names = FALSE)
  
  sprintf("%.1f [%.1f, %.1f]", med, q1, q3)
}

summarise_cont_by <- function(df, group_var, cont_vars) {
  out <- lapply(cont_vars, function(v) {
    df %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarise(
        stat = median_iqr_or_range(.data[[v]], v),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        variable = v,
        level = ifelse(v == "moved", "[Min, Max]", "Median [IQR]")
      ) %>%
      dplyr::select(variable, level, !!group_var, stat)
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
# Trial-level LTFU and protocol deviations
# ---------------------------------------------------------------------

ltfu_moved_trial <- fi_set %>%
  dplyr::mutate(
    c_missing = suppressWarnings(as.numeric(c_missing)),
    i_missing = suppressWarnings(as.numeric(i_missing)),
    n_movedi  = suppressWarnings(as.numeric(n_movedi)),
    n_movedc  = suppressWarnings(as.numeric(n_movedc)),
    pval      = suppressWarnings(as.numeric(pval)),
    ltfu      = row_sum2(c_missing, i_missing),
    moved     = row_sum2(n_movedi, n_movedc)
  ) %>%
  dplyr::group_by(record_id) %>%
  dplyr::summarise(
    ltfu  = dplyr::first(ltfu),
    moved = dplyr::first(moved),
    pval  = dplyr::first(pval),
    .groups = "drop"
  )

table1_df <- sample_list %>%
  dplyr::left_join(ltfu_moved_trial, by = "record_id")

table1_df <- table1_df %>%
  dplyr::mutate(
    study_year_group = dplyr::case_when(
      study_year >= 1983 & study_year <= 1992 ~ "1983-1992",
      study_year >= 1993 & study_year <= 2002 ~ "1993-2002",
      study_year >= 2003 & study_year <= 2012 ~ "2003-2012",
      study_year >= 2013 & study_year <= 2022 ~ "2013 - 2022",
      TRUE ~ NA_character_
    ),
    study_year_group = factor(
      study_year_group,
      levels = c("1983-1992", "1993-2002", "2003-2012", "2013 - 2022")
    )
  )

table1_df <- table1_df %>%
  dplyr::mutate(
    rct_blind_d.factor =
      dplyr::case_when(
        rct_blind.factor %in% c(
          "Single blind",
          "Double blind",
          "Triple blind"
        ) ~ "Yes",
        rct_blind.factor == "Unblinded" ~ "No",
        TRUE ~ NA_character_
      ),
    
    rct_conceal_d.factor =
      dplyr::case_when(
        rct_conceal.factor == "Unclear" ~ "No",
        TRUE ~ "Yes"
      )
  )

table1_df <- table1_df %>%
  dplyr::mutate(
    pval = suppressWarnings(as.numeric(pval)),
    pval_significance = dplyr::case_when(
      pval < 0.05 ~ "Significant",
      pval >= 0.05 ~ "Non-significant",
      TRUE ~ NA_character_
    ),
    pval_significance = factor(
      pval_significance,
      levels = c("Non-significant", "Significant")
    )
  )
# =====================================================================
# TABLE 1: Superiority-only sample characteristics
# =====================================================================

tab1_cat <- summarise_cat_by(
  table1_df,
  group_var = "i_type.factor",
  cat_vars = c(
    "study_year_group",
    "c_participants.factor",
    "c_design.factor",
    "rct_type.factor",
    "u_allocation.factor",
    "rct_blind_d.factor",
    "rct_conceal_d.factor",
    "rct_centers.factor",
    "ethic.factor",
    "funding.factor",
    "d_share.factor",
    "miss_data.factor",
    "sample_calc.factor",
    "pval_significance"
  )
)

tab1_cont <- summarise_cont_by(
  table1_df,
  group_var = "i_type.factor",
  cont_vars = c("sample_size", "ltfu", "moved")
)

tab1_n <- table1_df %>%
  dplyr::count(i_type.factor, name = "N") %>%
  tidyr::pivot_wider(names_from = i_type.factor, values_from = N) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
  dplyr::mutate(variable = "N", .before = 1)

# =====================================================================
# Export Table 1
# =====================================================================

out_file <- file.path(DIR_OUTPUTS, "Summary_Tables_Superiority_2026-02.xlsx")

writexl::write_xlsx(
  list(
    Table1_superiority_counts = tab1_cat,
    Table1_superiority_continuous_median_IQR = tab1_cont,
    Table1_superiority_N = tab1_n
  ),
  path = out_file
)

# =====================================================================
# SDC 3: Superiority-only sampled RCTs with event rates (%)
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
    record_id = pick_col(dplyr::cur_data(), c("record_id")),
    Year      = pick_col(dplyr::cur_data(), c("study_year")),
    Report_ID = pick_col(dplyr::cur_data(), c("report_id")),
    Study_reference = pick_col(dplyr::cur_data(), c("study_reference")),
    Outcome = pick_col(dplyr::cur_data(), c("out_name")),
    Outcome_definition = pick_col(dplyr::cur_data(), c("out_definition")),
    
    Participants = dplyr::coalesce(
      as.character(pick_col(dplyr::cur_data(), c("c_participants.factor"), default = NA)),
      as.character(pick_col(dplyr::cur_data(), c("c_participants"), default = NA))
    ),
    
    Design = dplyr::coalesce(
      as.character(pick_col(dplyr::cur_data(), c("c_design.factor"), default = NA)),
      as.character(pick_col(dplyr::cur_data(), c("c_design"), default = NA))
    ),
    
    Trial_type = dplyr::coalesce(
      as.character(pick_col(dplyr::cur_data(), c("rct_type.factor"), default = NA)),
      as.character(pick_col(dplyr::cur_data(), c("rct_type"), default = NA))
    ),
    
    Intervention_type = dplyr::coalesce(
      as.character(pick_col(dplyr::cur_data(), c("i_type.factor"), default = NA)),
      as.character(pick_col(dplyr::cur_data(), c("i_type"), default = NA))
    ),
    
    Intervention_N              = i_total_num,
    Intervention_events         = i_events_num,
    Intervention_event_rate_pct = i_rate_pct,
    
    Control_N                   = c_total_num,
    Control_events              = c_events_num,
    Control_event_rate_pct      = c_rate_pct
  ) %>%
  dplyr::arrange(dplyr::desc(Year), Report_ID)

sdc3_file <- file.path(DIR_TABLES, "SDC3_Superiority_RCTs_EventRates_2026-02.xlsx")
writexl::write_xlsx(sdc3, path = sdc3_file)

message("03_summary_tabs.R superiority-only update complete.")
message("Table 1 written: ", out_file)
message("SDC 3 written: ", sdc3_file)
message("Primary cohort n = ", dplyr::n_distinct(fi_set$record_id))