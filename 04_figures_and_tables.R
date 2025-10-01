# scripts/04_figures_and_tables.R
# Create key figures (FI/FQ distributions, subspecialty comparisons, diagnostics)

library(readr); library(dplyr); library(ggplot2)

dat <- readr::read_csv("data/processed/fi_dataset_fi_fq.csv", show_col_types = FALSE)

# Example: FI distribution
p1 <- ggplot(dat, aes(x = fi_single)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Fragility Index", x = "FI", y = "Count")
ggsave("figures/fi_distribution.png", p1, width = 7, height = 5, dpi = 300)

# Example: FQ vs sample size
p2 <- ggplot(dat, aes(x = sample_size, y = fq)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Fragility Quotient vs Sample Size", x = "Sample size", y = "FQ")
ggsave("figures/fq_vs_n.png", p2, width = 7, height = 5, dpi = 300)

message("Figures saved to figures/")