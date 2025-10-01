library(dplyr)
library(tidyr)
library(fragility)
library(table1)

## Table 1. Characteristics of the studies identified
tab1 <- table1(~ study_year + c_participants.factor + 
                 c_design.factor | included.factor, data=eligibility)
as.data.frame(tab1)

summary(eligibility$study_year)


## Table 2. Characteristics of the studies eligible for FI analysis
tab2 <- table1(~ c_participants.factor + rct_aim.factor + rct_type.factor + u_allocation.factor + 
                 rct_blind.factor + rct_conceal.factor + rct_centers.factor + 
                 ethic.factor + funding.factor + t_nifunding.factor + 
                 d_share.factor| i_type.factor, data=included)
as.data.frame(tab2)

eligiblestudies <- as.data.frame(tab2) 

included$rct_blind_d.factor <- factor(included$rct_blind.factor, 
                                      levels = c("Single blind", "Double blind", "Triple blind", "Unblinded"),
                                      labels = c("Yes", "Yes", "Yes", "No"))

included$rct_conceal_d.factor <- factor(included$rct_conceal.factor, 
                                        levels = c("Sequentially numbered sealed/opaque envelopes", 
                                                   "Sequentially numbered containers", 
                                                   "Pharmacy controlled allocation", 
                                                   "Central allocation (site remote from trial location)", 
                                                   "Other", 
                                                   "Unclear"),
                                        labels = c("Yes", "Yes", "Yes", "Yes", "Yes", "Unclear"))


## Table 6. New variables table 2
tab6 <- table1(~ rct_aim.factor + rct_blind_d.factor + rct_conceal_d.factor + 
                 rct_centers.factor + ethic.factor + funding.factor + 
                 t_nifunding.factor + d_share.factor| i_type.factor, data=included)
as.data.frame(tab6)


## Table 3. Characteristics of the sample 

sample_list$rct_blind_d.factor <- factor(sample_list$rct_blind.factor, 
                                      levels = c("Single blind", "Double blind", "Triple blind", "Unblinded"),
                                      labels = c("Yes", "Yes", "Yes", "No"))

sample_list$rct_conceal_d.factor <- factor(sample_list$rct_conceal.factor, 
                                        levels = c("Sequentially numbered sealed/opaque envelopes", 
                                                   "Sequentially numbered containers", 
                                                   "Pharmacy controlled allocation", 
                                                   "Central allocation (site remote from trial location)", 
                                                   "Other", 
                                                   "Unclear"),
                                        labels = c("Yes", "Yes", "Yes", "Yes", "Yes", "Unclear"))


tab3 <- table1(~ study_year + c_participants.factor + c_design.factor + 
                 rct_aim.factor + rct_type.factor + u_allocation.factor + 
                 rct_blind_d.factor + rct_conceal_d.factor + rct_centers.factor + 
                 ethic.factor + funding.factor + d_share.factor + 
                 sample_calc.factor + sample_size| i_type.factor, 
               data=sample_list)

samplesummary <- as.data.frame(tab3)

samplesummary

## Table 4 Distribution of RCTs identified from guidelines 
tab4 <- table1(~ study_year + c_participants.factor + c_design.factor + 
                 rct_aim.factor + rct_type.factor + u_allocation.factor + 
                 rct_blind.factor + rct_conceal.factor + rct_centers.factor + 
                 ethic.factor + funding.factor + d_share.factor + i_type.factor, 
               data=eligibility)
as.data.frame(tab4)

## Table 5. Characteristics of CPGs

cleaned_data <- cleaned_data[order(cleaned_data$cpg_year, decreasing = TRUE),]

selected_columns <- c(
  "cpg_year", "cpg_title", "guideline_type.factor",
  "cpg_target.factor", "report_quality.factor", "class_type.factor",
  "n_recom", "studies_identified")

result_table <- cleaned_data[, selected_columns]


tab5 <- table1(~ cpg_year + guideline_type.factor + 
                 cpg_target.factor + report_quality.factor + class_type.factor, data=cleaned_data)
as.data.frame(tab5)

summary_median <- binomdat %>%
  group_by(i_type.factor) %>%
  summarise(
    sample_median = median(sample_size, na.rm = TRUE),
    sample_Q1 = quantile(sample_size, 0.25, na.rm = TRUE),
    sample_Q3 = quantile(sample_size, 0.75, na.rm = TRUE),
    sample_range = paste0(min(sample_size, na.rm = TRUE), " - ", max(sample_size, na.rm = TRUE)),
    ltfu_median = median(ltfu, na.rm = TRUE),
    ltfu_Q1 = quantile(ltfu, 0.25, na.rm = TRUE),
    ltfu_Q3 = quantile(ltfu, 0.75, na.rm = TRUE),
    ltfu_range = paste0(min(ltfu, na.rm = TRUE), " - ", max(ltfu, na.rm = TRUE)),
    moved_median = median(moved, na.rm = TRUE),
    moved_Q1 = quantile(moved, 0.25, na.rm = TRUE),
    moved_Q3 = quantile(moved, 0.75, na.rm = TRUE),
    moved_range = paste0(min(moved, na.rm = TRUE), " - ", max(moved, na.rm = TRUE)),
    n = n()
  )

print(summary_median)

## Study year ##

# Function to calculate the mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}


mode_study_year <- get_mode(eligibility$study_year)

print(mode_study_year)

study_year_freq <- table(eligibility$study_year)

study_year_freq_sorted <- sort(study_year_freq, decreasing = TRUE)

print(study_year_freq_sorted)

eligibility$study_year_range <- cut(eligibility$study_year, 
                                    breaks = seq(1970, 2022, by = 10), 
                                    right = FALSE, 
                                    labels = paste(seq(1970, 2010, by = 10), "-", seq(1979, 2019, by = 10)))

study_year_freq_ranges <- table(eligibility$study_year_range)

print(study_year_freq_ranges)


# Cross-tabulation of 'study_year_range' and 'included'
study_year_included_freq <- table(eligibility$study_year_range, eligibility$included)

print(study_year_included_freq)

#######################

mode_cpg_year <- get_mode(cleaned_data$cpg_year)
mode_cpg_year

summary(cleaned_data$cpg_year)

cpg_year_freq <- table(cleaned_data$cpg_year)

cpg_year_freq

cpg_year_freq_sorted <- sort(cpg_year_freq, decreasing = TRUE)

print(cpg_year_freq_sorted)

eligibility$cpg_year_range <- cut(cleaned_data$cpg_year, 
                                  breaks = seq(2012, 2022, by = 5), 
                                  right = FALSE, 
                                  labels = paste(seq(2012, 2022, by = 5), "-", seq(2012, 2022, by = 5)))

cpg_year_freq_ranges <- table(cleaned_data$cpg_year_range)

print(cpg_year_freq_ranges)


summary(sample_list$study_year)

# Define intervals for 'study_year'
sample_list$study_year_range <- cut(sample_list$study_year, 
                                    breaks = seq(1983, 2023, by = 10), 
                                    right = FALSE, 
                                    labels = c("1983-1992", "1993-2002", "2003-2012", "2013-2022"))

# Cross-tabulation of 'study_year_range' and 'i_type.factor'
study_year_intervention_freq <- table(sample_list$study_year_range, sample_list$i_type.factor)

print(study_year_intervention_freq)s


######## File exports #########
library(writexl)
write_xlsx(result_table, path = "cpg_summary.xlsx")

write_xlsx(eligiblestudies, path = "eligiblestudies.xlsx")

write_xlsx(samplesummary, path = "samplesummary.xlsx")

write_xlsx(binomdat, path = "binomdat.xlsx")
