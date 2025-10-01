# scripts/00_setup.R
# Install & load required packages; set options and seed

pkgs <- c(
  "fragilityindex", "MASS", "dplyr", "tidyr", "ggplot2",
  "readr", "purrr", "stringr", "broom", "performance", "car", "sampler"
)

install_if_missing <- function(pk){
  if (!requireNamespace(pk, quietly = TRUE)) {
    install.packages(pk, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(pkgs, install_if_missing))
invisible(lapply(pkgs, require, character.only = TRUE))

options(stringsAsFactors = FALSE)
set.seed(2025)

# Create output dirs if missing
dirs <- c("figures", "outputs/tables", "outputs/models", "data/processed")
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

message("Setup complete.")