# scripts/05_reproducibility_checks.R
# Save session info and package versions used for the analysis.

sink("outputs/sessionInfo.txt")
cat("==== sessionInfo() ====
")
print(sessionInfo())
sink()
message("Session info saved to outputs/sessionInfo.txt")