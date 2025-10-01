# scripts/01_data_ingest_and_clean.R
# Read extracted summary-level RCT data and harmonize variable names/types.

library(readr); library(dplyr); library(stringr)

# === USER ACTION ===
# Point to your main CSV with trial-level summary data.
# Example: "data/raw/fi_dataset.csv"
input_file <- "data/raw/fi_dataset.csv"

if (!file.exists(input_file)) {
  stop("Missing input file: ", input_file, call. = FALSE)
}

raw <- readr::read_csv(input_file, show_col_types = FALSE)

# Minimal column set expected (rename here if needed):
# You can adapt these mappings to your actual column names.
mapped <- raw %>%
  rename(
    fi_single              = fi_single,
    pval_significance      = pval_significance,
    sample_size            = sample_size,
    ltfu                   = ltfu,
    moved                  = moved,
    c_participants.factor  = c_participants.factor,
    c_design.factor        = c_design.factor,
    rct_aim.factor         = rct_aim.factor,
    i_type.factor          = i_type.factor,
    rct_conceal_d.factor   = rct_conceal_d.factor,
    rct_blind_d.factor     = rct_blind_d.factor,
    rct_centers.factor     = rct_centers.factor,
    funding.factor         = funding.factor
  )

# Basic type coercions
mapped <- mapped %>%
  mutate(
    across(ends_with(".factor"), as.factor),
    pval_significance = as.factor(pval_significance),
    sample_size = as.integer(sample_size),
    ltfu = as.integer(ltfu),
    moved = as.integer(moved)
  )

readr::write_csv(mapped, "data/processed/fi_dataset_clean.csv")
message("Ingest & clean complete -> data/processed/fi_dataset_clean.csv")