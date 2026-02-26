require_pkgs(c("readxl", "dplyr", "writexl"))

DIR_OUTPUTS <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"
ensure_dir(DIR_OUTPUTS)


SAMPLE_COHORT_FILE <- "sample_list.xlsx"  # <-- put this file in your project folder

if (!file.exists(SAMPLE_COHORT_FILE)) {
  stop(
    paste(
      "Cannot find:", SAMPLE_COHORT_FILE,
      "\nPlace it in your project directory (same folder you run 00_run_all.R from),",
      "\nor change SAMPLE_COHORT_FILE to the correct filename."
    ),
    call. = FALSE
  )
}

sampled_trials <- readxl::read_excel(SAMPLE_COHORT_FILE)

if (!("record_id" %in% names(sampled_trials))) {
  stop(
    paste(
      "The cohort file does not contain a 'record_id' column:",
      SAMPLE_COHORT_FILE,
      "\nUse a cohort file that includes record_id (e.g., sample_list.xlsx)."
    ),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------
# Freeze IDs (unique trial IDs)
# ---------------------------------------------------------------------
frozen_ids <- sampled_trials %>%
  mutate(record_id = as.character(record_id)) %>%
  filter(!is.na(record_id), record_id != "") %>%
  distinct(record_id)

n_ids <- nrow(frozen_ids)
message("Frozen cohort loaded from: ", SAMPLE_COHORT_FILE)
message("Number of unique record_id frozen: ", n_ids)

# ---------------------------------------------------------------------
# Save frozen IDs to outputs/ for reproducibility
# ---------------------------------------------------------------------
write.csv(
  frozen_ids,
  file.path(DIR_OUTPUTS, "frozen_sample_ids_2026-02.csv"),
  row.names = FALSE
)

saveRDS(
  frozen_ids,
  file.path(DIR_OUTPUTS, "frozen_sample_ids_2026-02.rds")
)

# Also save a small receipt for audit trail
receipt <- data.frame(
  cohort_file = SAMPLE_COHORT_FILE,
  n_unique_record_id = n_ids,
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

writexl::write_xlsx(
  list(
    receipt = receipt,
    frozen_ids = frozen_ids
  ),
  path = file.path(DIR_OUTPUTS, "frozen_sample_receipt_2026-02.xlsx")
)

# Make available to downstream scripts in this run
assign("frozen_ids", frozen_ids, envir = .GlobalEnv)

message("02_sample_size.R complete: frozen IDs saved to outputs/ and loaded as object 'frozen_ids'.")
