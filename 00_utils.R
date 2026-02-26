# =====================================================================
# 00_utils.R  (FINAL)
# =====================================================================

DIR_OUTPUTS <- "outputs"
DIR_FIGURES <- "figures"

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

require_pkgs <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Missing package: ", p,
           ". Please install it once with install.packages('", p, "').",
           call. = FALSE)
    }
  }
  # attach AFTER checks
  suppressPackageStartupMessages({
    for (p in pkgs) library(p, character.only = TRUE)
  })
}

# Provide %>% if user code still uses it
# (magrittr is imported by dplyr but this guarantees it)
`%>%` <- magrittr::`%>%`

# NA-safe row sum for two columns
row_sum2 <- function(a, b) {
  a2 <- ifelse(is.na(a), 0, as.numeric(a))
  b2 <- ifelse(is.na(b), 0, as.numeric(b))
  a2 + b2
}

ensure_dir(DIR_OUTPUTS)
ensure_dir(DIR_FIGURES)