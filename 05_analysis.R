### Ngeative Binomial regression 

library(MASS)
library(car)
library(MuMIn)
library(COUNT)
library(dplyr)
library(corrplot)
library(vcd)
library(tidyr)
install.packages("psych")
library(psych)

#Preparing data set
binomdat <- fi_set %>% 
  mutate(across(where(is.factor), droplevels))

## Modifying the variable type of blinding to dichotomous (y/n)
binomdat$rct_blind_d.factor <- factor(binomdat$rct_blind.factor, 
                                      levels = c("Single blind", "Double blind", "Triple blind", "Unblinded"),
                                      labels = c("Yes", "Yes", "Yes", "No"))

## Modifying the variable type of concealment 
binomdat$rct_conceal_d.factor <- factor(binomdat$rct_conceal.factor, 
                                        levels = c("Sequentially numbered sealed/opaque envelopes", 
                                                   "Sequentially numbered containers", 
                                                   "Pharmacy controlled allocation", 
                                                   "Central allocation (site remote from trial location)", 
                                                   "Other", 
                                                   "Unclear"),
                                        labels = c("Yes", "Yes", "Yes", "Yes", "Yes", "Unclear"))

## Modifying the variable p value 
binomdat$pval_significance <- ifelse(binomdat$pval < 0.05, "Significant", 
                                     "Non-significant")
binomdat$pval_significance <- as.factor(binomdat$pval_significance)


## Creating the variable lost-to-follow (ltfu) up and patients moved from group (moved)
binomdat <- binomdat %>% mutate(ltfu = sum(c_missing, i_missing),
                                moved = sum(n_movedi, n_movedc))

no.na.data <- binomdat %>% 
  select(-report_id, -study_year, -included, -i_events, -c_events, 
       -i_total, -c_total, -c_missing, -i_missing, -n_movedi, -n_movedc, 
       -FI)


summary(binomdat)


tab7 <- table1(~ study_year + c_participants.factor + c_design.factor + 
                 rct_aim.factor + rct_type.factor + u_allocation.factor + 
                 rct_blind_d.factor + rct_conceal_d.factor + rct_centers.factor + 
                 ethic.factor + funding.factor + d_share.factor + sample_size + ltfu + moved +
                 pval_significance| i_type.factor, 
               data=binomdat)
as.data.frame(tab7)


## Checking for correlation for continuous variables

continuous_vars <- no.na.data[, c("sample_size", "pval", "ltfu", "moved")]

correlation_matrix <- cor(continuous_vars, use = "complete.obs")

corrplot(correlation_matrix, method = "circle")

#Moderate correlation between sample size, ltfu, and moved.
#Low correlation between pval, ltfu, and moved


# Checking for correlation for categorical variables
#Cramér's V for all pairs of categorical variables
cramers_v_matrix <- function(data) {
  cat_vars <- data[, sapply(data, is.factor)] 
  var_names <- colnames(cat_vars)
  n <- length(var_names)
  

  cramers_v_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(var_names, var_names))
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])  
      if (min(dim(tbl)) > 1) {  
        cramers_v_matrix[i, j] <- assocstats(tbl)$cramer
        cramers_v_matrix[j, i] <- cramers_v_matrix[i, j]
      }
    }
  }
  
  return(cramers_v_matrix)
}

categorical_vars <- no.na.data %>% 
  select(-record_id, -sample_size, -ltfu, -moved, -pval, -fi_each, -fq)

cat_correlation_matrix <- cramers_v_matrix(categorical_vars)

corrplot(cat_correlation_matrix, method = "circle", na.label = "NA")

###Strong correlation between: rct_aim and rct_type (0.84)
## Moderate correlation between: 
# c_participants and rct_type 
# c_participants and rct_aim
# c_participants and funding
# rct_type and d_share 
# rct_aim and d_share
# rct_type and funding
# i_type and rct_blind_d
# rct_conceal and rct_centers


# Correlation between categorical and continuous variables
anova_result <- aov(sample_size ~ c_participants.factor, data = no.na.data)
summary(anova_result)

anova_result <- aov(sample_size ~ rct_type.factor, data = no.na.data)
summary(anova_result)

anova_result <- aov(sample_size ~ i_type.factor, data = no.na.data)
summary(anova_result)

anova_result <- aov(sample_size ~ rct_centers.factor, data = no.na.data)
summary(anova_result) #### as expected there is a correlation between the sample size and the number of centers

## The variable rct_aim was removed due to strong correlation with rct_type. For the purposes of the analysis rct_type provided more information



# Descriptive statistics

summary(no.na.data)

descriptive <- no.na.data %>%
  select(-record_id, -rct_type.factor, -u_allocation.factor) %>% 
  mutate(fi_each_percentile = cut(fi_each, 
                                  breaks = quantile(fi_each, probs = seq(0, 1, 0.25), na.rm = TRUE), 
                                  include.lowest = TRUE, 
                                  labels = c("Q1", "Q2", "Q3", "Q4")))

numeric_stats <- descriptive %>%
  group_by(fi_each_percentile) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        median = ~median(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE)),
                   .names = "{col}_{fn}")) 

factor_stats <- descriptive %>%
  group_by(fi_each_percentile) %>%
  summarise(across(where(is.factor), 
                   ~list(table(.) %>% prop.table() %>% round(2)),
                   .names = "{col}_freq"))

# Step 1: Evaluating each variable

glm1 <- glm.nb(fi_each ~ study_year, 
               data = binomdat)

summary(glm1) ## variable study_year no significant p value 0.291

glm2 <- glm.nb(fi_each ~ c_participants.factor, 
               data = binomdat)

summary(glm2) ## Only the category regional is significantly associated. Lower FI compared to reference category (obstetric)

glm3 <- glm.nb(fi_each ~ c_design.factor, 
               data = binomdat)

summary(glm3) ## Only the category 2x2 factorial is significantly associated. Higher FI 

glm4 <- glm.nb(fi_each ~ rct_aim.factor, 
               data = binomdat)

summary(glm4) ## Only the category Equivalence is significantly associated with lower FI

glm5 <- glm.nb(fi_each ~ rct_type.factor, 
               data = binomdat)

summary(glm5) ### Only the category Equivalence is significantly associated with lower FI

glm6 <- glm.nb(fi_each ~ i_type.factor, 
               data = binomdat)

summary(glm6) ## The variable type of intervention is significantly associated with lower FI for non-drug related interventions

glm7 <- glm.nb(fi_each ~ rct_blind.factor, 
               data = binomdat)

summary(glm7) ## Only the category double_blind  is significantly associated with higher FI


glm8 <- glm.nb(fi_each ~ rct_blind_d.factor, 
               data = binomdat)

summary(glm8) # non-blinded studies have a significantly lower expected number of events compared to blinded studies


glm9 <- glm.nb(fi_each ~ rct_conceal.factor, 
               data = binomdat)

summary(glm9) # only the categories other type of concealment, unclear concealment and central allocation concealment are associated


glm10 <- glm.nb(fi_each ~ rct_conceal_d.factor, 
               data = binomdat)

summary(glm10) #studies with unclear/no concealment have a significantly lower 
#expected number of events compared to those with proper concealment

glm11 <- glm.nb(fi_each ~ rct_centers.factor, 
               data = binomdat) 
summary(glm11) #Multicenter studies are associated with a higher count of FI 

glm12 <- glm.nb(fi_each ~ ethic.factor, 
                data = binomdat)
summary(glm12) #No association

glm13 <- glm.nb(fi_each ~ funding.factor, 
                data = binomdat)
summary(glm13) #Non-industry funded studies are associated with a higher count of FI

glm14 <- glm.nb(fi_each ~ d_share.factor, 
                data = binomdat)
summary(glm14) #No association

glm15 <- glm.nb(fi_each ~ pval, 
                data = binomdat)
summary(glm15) #No association

glm16 <- glm.nb(fi_each ~ pval_significance, 
                data = binomdat)
summary(glm16)# Studies with a significant pvalue are associated with a higher count of FI 

glm17 <- glm.nb(fi_each ~ ltfu, 
                data = binomdat)
summary(glm17) # associated with a higher count of FI 

glm18 <- glm.nb(fi_each ~ moved, 
                data = binomdat)
summary(glm18) # associated with a higher count of FI 

glm19 <- glm.nb(fi_each ~ sample_size, 
                data = binomdat)
summary(glm19) # associated with a higher count of FI 

glm19 <- glm.nb(fi_each ~ sample_size, 
                data = binomdat)
summary(glm19)
