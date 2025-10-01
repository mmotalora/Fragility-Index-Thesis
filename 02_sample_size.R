install.packages("sampler")
library(sampler)

library(tidyverse)

library(readxl)

library(dplyr)


extraction_log <- read_excel("extraction_log.xlsx")
View(extraction_log)

citation('sampler')

extraction_log <- extraction_log %>% 
  mutate(group.f = factor(group, levels = c("1", "2", "3", "4", "5", "6"), 
                          labels = c("general", "pediatric", "obstetric", 
                                     "regional", "cardiovascular", "neuroanesthesia")))

summary(extraction_log)  

ssampcalc(df=extraction_log, n=220, strata = group.f)

sample <- ssamp(df=extraction_log, n=220, strata=group.f)

install.packages("writexl")

library(writexl)

write_xlsx(sample, "~/Documents/Fragility Index project/Fragility/sample.xlsx")

#########
##Eligible trials distribution
# Adults 421/639 65.9%.  // 58.9%
# Pediatric 54/639 8.5% // 6.8%
# Obstetric 68/639 10.6% // 13.7%
# CV 41/639 6.4% // 7.8%
# Regional 55/639 8.6% //12.8%
# Other 0 


