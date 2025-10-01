library(dplyr)
library(tidyr)
library(FragilityTools)
library(fragility)

fi_set <- sample %>%
  rowwise() %>%
  mutate(FI = list(frag.study(e0 = i_events, n0 = i_total, e1 = c_events, 
                              n1 = c_total, methods = "Fisher")))

fi <- sapply(1:nrow(fi_set), function(i) as.numeric(fi_set[[28]][[i]][["FI"]]))

fi_set$fi_each <- fi

pval <- sapply(1:nrow(fi_set), function(i) as.numeric(fi_set[[28]][[i]][["pval"]]))

fi_set$pval <- pval

fq <-  sapply(1:nrow(fi_set), function(i) as.numeric(fi_set[[28]][[i]][["FQ"]]))

fi_set$fq <- fq

summary(fi_set$fi_each)

Q1 <- quantile(fi_set$fi_each, 0.25, na.rm = TRUE)
Q3 <- quantile(fi_set$fi_each, 0.75, na.rm = TRUE)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

extreme_values <- fi_set$fi_each[fi_set$fi_each < lower_bound | fi_set$fi_each > upper_bound]

extreme_values

extreme_cases <- fi_set[fi_set$fi_each < lower_bound | fi_set$fi_each > upper_bound, ]

#### Overall FI ####
overall_fi <- frag.studies(i_events, i_total, c_events, c_total, sample, 
                     methods = "Fisher")

overall_fi

#The input dataset contains 219 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#93 studies yield significance with
#median FI = 4, range 1-189, IQR 1-9 and
#median FQ = 3.0%, range 0.2%-27.6%, IQR 1.3%-5.3%;
#126 studies yield non-significance with
#median FI = 4, range 1-62, IQR 2-6 and
#median FQ = 4.0%, range 0.0%-19.0%, IQR 1.6%-6.7%;
#overall, among all studies,
#median FI = 4, range 1-189, IQR 2-7 and
#median FQ = 3.6%, range 0.0%-27.6%, IQR 1.4%-6.2%

#### FI by subgroups ####
## General population trials 
general <- fi_set %>% 
  filter(c_participants.factor == "Adults")

adults_fi <- frag.studies(i_events, i_total, c_events, c_total, general, 
                           methods = "Fisher")
adults_fi

#The input dataset contains 129 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#57 studies yield significance with
#median FI = 3, range 1-189, IQR 2-9 and
#median FQ = 2.5%, range 0.2%-27.6%, IQR 1.0%-5.0%;
#72 studies yield non-significance with
#median FI = 4, range 1-46, IQR 2-6 and
#median FQ = 3.7%, range 0.0%-10.2%, IQR 1.3%-5.9%;
#overall, among all studies,
#median FI = 4, range 1-189, IQR 2-7 and
#median FQ = 3.3%, range 0.0%-27.6%, IQR 1.2%-5.0%

## Pediatric population trials 
pediatric <- fi_set %>% 
  filter(c_participants.factor == "Pediatric")

pediatric_fi <- frag.studies(i_events, i_total, c_events, c_total, pediatric, 
                          methods = "Fisher")
pediatric_fi

#The input dataset contains 15 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#8 studies yield significance with
#median FI = 1, range 1-33, IQR 1-12 and
#median FQ = 1.4%, range 0.8%-11.4%, IQR 1.3%-4.9%;
#7 studies yield non-significance with
#median FI = 3, range 1-6, IQR 2-4 and
#median FQ = 3.8%, range 0.3%-17.1%, IQR 1.8%-5.0%;
#overall, among all studies,
#median FI = 3, range 1-33, IQR 1-5 and
#median FQ = 2.5%, range 0.3%-17.1%, IQR 1.4%-5.0%

## Obstetric population trials 
ob <- fi_set %>% 
  filter(c_participants.factor == "Obstetric")

ob_fi <- frag.studies(i_events, i_total, c_events, c_total, ob, 
                             methods = "Fisher")
ob_fi

#The input dataset contains 30 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#15 studies yield significance with
#median FI = 4, range 1-61, IQR 2-6 and
#median FQ = 3.6%, range 1.0%-15.3%, IQR 1.7%-5.7%;
#15 studies yield non-significance with
#median FI = 4, range 1-62, IQR 2-5 and
#median FQ = 8.0%, range 0.5%-16.7%, IQR 1.7%-10.0%;
#overall, among all studies,
#median FI = 4, range 1-62, IQR 2-6 and
#median FQ = 4.3%, range 0.5%-16.7%, IQR 1.7%-9.5%

## Cardiovascular population trials
cv <- fi_set %>% 
  filter(c_participants.factor == "Cardiovascular")

cv_fi <- frag.studies(i_events, i_total, c_events, c_total, cv, 
                      methods = "Fisher")
cv_fi

#The input dataset contains 17 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#4 studies yield significance with
#FI = 1, 8, 1, 7 and
#FQ = 0.3%, 8.0%, 0.8%, 2.5%;
#13 studies yield non-significance with
#median FI = 5, range 1-27, IQR 1-13 and
#median FQ = 2.3%, range 0.4%-14.3%, IQR 1.3%-5.0%;
#overall, among all studies,
#median FI = 5, range 1-27, IQR 1-12 and
#median FQ = 2.3%, range 0.3%-14.3%, IQR 0.9%-5.0%

## Regional population trials
regional <- fi_set %>% 
  filter(c_participants.factor == "Regional")

regional_fi <- frag.studies(i_events, i_total, c_events, c_total, regional, 
                      methods = "Fisher")
regional_fi

#The input dataset contains 28 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#9 studies yield significance with
#median FI = 3, range 1-12, IQR 1-9 and
#median FQ = 4.3%, range 1.4%-21.1%, IQR 2.9%-7.5%;
#19 studies yield non-significance with
#median FI = 4, range 1-8, IQR 2-5 and
#median FQ = 4.5%, range 0.6%-19.0%, IQR 1.6%-7.5%;
#overall, among all studies,
#median FI = 4, range 1-12, IQR 2-5 and
#median FQ = 4.5%, range 0.6%-21.1%, IQR 2.3%-7.5%

wilcox_test_result <- wilcox.test(fi_each ~ pval_significance, data = binomdat, exact = FALSE)
wilcox_test_result

conceal <- binomdat %>% 
  filter(rct_conceal_d.factor == "Yes")

conceal_fi <- frag.studies(i_events, i_total, c_events, c_total, conceal, 
                            methods = "Fisher")
conceal_fi

# Summary Table: FI and FQ by Type of Intervention
summary_intervention <- binomdat %>%
  group_by(i_type.factor) %>%
  summarise(
    FI_mean = mean(fi_each, na.rm = TRUE),
    FI_median = median(fi_each, na.rm = TRUE),
    FI_sd = sd(fi_each, na.rm = TRUE),
    FQ_mean = mean(fq, na.rm = TRUE),
    FQ_median = median(fq, na.rm = TRUE),
    FQ_sd = sd(fq, na.rm = TRUE),
    n = n()
  )

print(summary_intervention)

# Boxplot: FI by Type of Intervention
ggplot(binomdat, aes(x = i_type.factor, y = fi_each, fill = i_type.factor)) +
  geom_boxplot() +
  labs(title = "Fragility Index (FI) by Type of Intervention",
       x = "Type of Intervention", y = "Fragility Index (FI)") +
  theme_minimal() +
  theme(legend.position = "none")

# Violin plot: FI by Type of Intervention
ggplot(binomdat, aes(x = i_type.factor, y = fi_each, fill = i_type.factor)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  labs(title = "Fragility Index (FI) by Type of Intervention",
       x = "Type of Intervention", y = "Fragility Index (FI)") +
  theme_minimal() +
  theme(legend.position = "none")

# Summary Table: FI and FQ by Allocation Concealment
summary_concealment <- binomdat %>%
  group_by(rct_conceal_d.factor) %>%
  summarise(
    FI_mean = mean(fi_each, na.rm = TRUE),
    FI_median = median(fi_each, na.rm = TRUE),
    FI_sd = sd(fi_each, na.rm = TRUE),
    FQ_mean = mean(fq, na.rm = TRUE),
    FQ_median = median(fq, na.rm = TRUE),
    FQ_sd = sd(fq, na.rm = TRUE),
    n = n()
  )

print(summary_concealment)

summary_funding <- binomdat %>%
  group_by(funding.factor) %>%
  summarise(
    FI_mean = mean(fi_each, na.rm = TRUE),
    FI_median = median(fi_each, na.rm = TRUE),
    FI_sd = sd(fi_each, na.rm = TRUE),
    FQ_mean = mean(fq, na.rm = TRUE),
    FQ_median = median(fq, na.rm = TRUE),
    FQ_sd = sd(fq, na.rm = TRUE),
    n = n()
  )


summary(binomdat$ltfu)

summary_population <- binomdat %>%
  group_by(c_participants.factor) %>%
  summarise(
    FI_mean = mean(fi_each, na.rm = TRUE),
    FI_median = median(fi_each, na.rm = TRUE),
    FI_sd = sd(fi_each, na.rm = TRUE),
    FQ_mean = mean(fq, na.rm = TRUE),
    FQ_median = median(fq, na.rm = TRUE),
    FQ_sd = sd(fq, na.rm = TRUE),
    n = n()
  )