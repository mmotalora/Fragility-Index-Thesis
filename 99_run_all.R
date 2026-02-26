# =====================================================================
# 00_run_all.R
# Master runner script for FI thesis/manuscript pipeline
# - Sets canonical dataset filename
# - Sources all project scripts in the correct order
# - Creates output folders
# - Logs session info for reproducibility
# =====================================================================

# ----------------------------
# Housekeeping
# ----------------------------
options(stringsAsFactors = FALSE)
set.seed(1)  # pipeline-level seed (sampling script may override deterministically)

# ----------------------------
# Canonical dataset (update here only)
# ----------------------------
RAW_DATA_FILE <- "fi_guideline_trials_master_2026-02.xlsx"

if (!file.exists(RAW_DATA_FILE)) {
  stop(paste("Dataset not found:", RAW_DATA_FILE,
             "\nCheck your working directory with getwd()."))
}
cat("Using dataset:", RAW_DATA_FILE, "\n")

# ----------------------------
# Project directories
# ----------------------------
DIR_OUTPUTS <- "outputs"
DIR_FIGURES <- "figures"

dir.create(DIR_OUTPUTS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)

# Optional: log file for the run
LOG_FILE <- file.path(DIR_OUTPUTS, "run_log.txt")
log_con <- file(LOG_FILE, open = "wt")
sink(log_con, type = "output")
sink(log_con, type = "message")

cat("=====================================================\n")
cat("FI pipeline run started:\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")
cat("Dataset:", RAW_DATA_FILE, "\n")
cat("=====================================================\n\n")

# ----------------------------
# Source scripts (order matters)
# ----------------------------
# NOTE: these scripts should not hardcode file names;
#       they should reference RAW_DATA_FILE where needed.

# Shared utilities (paths, helper functions, plotting helpers)
source("00_utils.R")

# Data cleaning & recoding (creates cleaned dataset objects / saves outputs)
source("01_data_cleaning.R")

# Sampling / sample size logic (writes sampling outputs, deterministic seed)
source("02_sample_size.R")

# Summary tables for manuscript
source("03_summary_tabs.R")

# FI / reverse FI / FQ computations
source("04_compute_fragility.R")

# Regression analysis / model outputs
source("05_analysis.R")

# Figures (forest plot, distributions, etc.)
source("06_figures.R")

# ----------------------------
# Reproducibility: session info
# ----------------------------
SESSION_FILE <- file.path(DIR_OUTPUTS, "session_info_2026-02.txt")
writeLines(capture.output(sessionInfo()), SESSION_FILE)

cat("\n=====================================================\n")
cat("FI pipeline run finished successfully.\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Outputs folder:", normalizePath(DIR_OUTPUTS), "\n")
cat("Figures folder:", normalizePath(DIR_FIGURES), "\n")
cat("Session info saved to:", SESSION_FILE, "\n")
cat("=====================================================\n")

# ----------------------------
# Close sinks
# ----------------------------
sink(type = "message")
sink(type = "output")
close(log_con)

message("Done. See outputs/run_log.txt for full console log.")
