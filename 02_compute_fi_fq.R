# scripts/02_compute_fi_fq.R
# Compute FI/FQ (if not already present) using the fragilityindex package.
# Assumes 2x2 summary counts per trial are available or precomputed.
# If your dataset already contains FI/FQ, you can skip or adapt this script.

library(readr); library(dplyr)
# library(fragilityindex) # Uncomment when using actual computation

dat <- readr::read_csv("data/processed/fi_dataset_clean.csv", show_col_types = FALSE)

# Placeholder: if you already have FI and FQ, ensure they're named fi_single and fq
# Otherwise, this is where you'd iterate over trials to compute FI/rFI/FQ
# using fragilityindex::fragility.index or similar functions given a 2x2 table.

if (!"fq" %in% names(dat)) {
  dat <- dat %>% mutate(fq = fi_single / sample_size)
}

readr::write_csv(dat, "data/processed/fi_dataset_fi_fq.csv")
message("FI/FQ ensured -> data/processed/fi_dataset_fi_fq.csv")