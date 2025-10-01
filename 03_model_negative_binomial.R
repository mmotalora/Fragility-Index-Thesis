# scripts/03_model_negative_binomial.R
# Fit negative binomial model for FI ~ trial characteristics

library(readr); library(dplyr); library(MASS); library(broom); library(performance); library(car)

dat <- readr::read_csv("data/processed/fi_dataset_fi_fq.csv", show_col_types = FALSE)

# Example formula — adapt to match your final variable names:
form <- as.formula("fi_single ~ c_participants.factor + c_design.factor + rct_aim.factor + i_type.factor +
                   rct_conceal_d.factor + rct_blind_d.factor + rct_centers.factor + funding.factor +
                   pval_significance + sample_size + ltfu + moved + pval_significance:sample_size")

fit <- tryCatch(MASS::glm.nb(form, data = dat), error = function(e) e)

if (inherits(fit, "error")) {
  stop("Model failed to fit: ", fit$message)
}

# Save model
saveRDS(fit, file = "outputs/models/neg_binom_fi.rds")

# Export tidy IRRs
tidy_res <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
readr::write_csv(tidy_res, "outputs/tables/neg_binom_fi_irrs.csv")

# Diagnostics
check_overdispersion(fit)
check_collinearity(fit)

message("Modeling complete. Outputs in outputs/models and outputs/tables.")