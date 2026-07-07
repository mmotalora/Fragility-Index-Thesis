
library(dplyr)
library(tidyr)
library(FragilityTools)
library(fragility)


#### FI by subgroups ####
## General population trials 
general <- fi_sup %>% 
  filter(c_participants.factor == "Adults")

adults_fi <- frag.studies(i_events, i_total, c_events, c_total, general, 
                          methods = "Fisher")
adults_fi

#The input dataset contains 110 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#49 studies yield significance with
#median FI = 3, range 1-189, IQR 2-10 and
#median FQ = 2.5%, range 0.2%-27.6%, IQR 1.0%-5.0%;
#61 studies yield non-significance with
#median FI = 4, range 1-46, IQR 2-7 and
#median FQ = 3.3%, range 0.0%-10.0%, IQR 1.2%-5.0%;
#overall, among all studies,
#median FI = 4, range 1-189, IQR 2-8 and
#median FQ = 3.0%, range 0.0%-27.6%, IQR 1.2%-5.0%

## Pediatric population trials 
pediatric <- fi_sup %>% 
  filter(c_participants.factor == "Pediatric")

pediatric_fi <- frag.studies(i_events, i_total, c_events, c_total, pediatric, 
                             methods = "Fisher")
pediatric_fi

#The input dataset contains 10 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#5 studies yield significance with
#median FI = 1, range 1-33, IQR 1-1 and
#median FQ = 1.3%, range 0.8%-4.9%, IQR 0.8%-1.4%;
#5 studies yield non-significance with
#median FI = 3, range 1-6, IQR 1-5 and
#median FQ = 2.5%, range 0.3%-17.1%, IQR 0.3%-5.0%;
#overall, among all studies,
#median FI = 1, range 1-33, IQR 1-5 and
#median FQ = 1.4%, range 0.3%-17.1%, IQR 0.8%-4.9%

## Obstetric population trials 
ob <- fi_sup %>% 
  filter(c_participants.factor == "Obstetric")

ob_fi <- frag.studies(i_events, i_total, c_events, c_total, ob, 
                      methods = "Fisher")
ob_fi

#The input dataset contains 21 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#11 studies yield significance with
#median FI = 6, range 1-61, IQR 2-8 and
#median FQ = 4.3%, range 1.0%-15.3%, IQR 3.3%-7.5%;
#10 studies yield non-significance with
#median FI = 4, range 1-12, IQR 1-5 and
#median FQ = 8.0%, range 1.0%-16.7%, IQR 1.7%-12.0%;
#overall, among all studies,
#median FI = 4, range 1-61, IQR 1-6 and
#median FQ = 4.3%, range 1.0%-16.7%, IQR 1.7%-10.2%

## Cardiovascular population trials
cv <- fi_sup %>% 
  filter(c_participants.factor == "Cardiovascular")

cv_fi <- frag.studies(i_events, i_total, c_events, c_total, cv, 
                      methods = "Fisher")
cv_fi

#The input dataset contains 13 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#3 studies yield significance with
#FI = 8, 1, 7 and
#FQ = 8.0%, 0.8%, 2.5%;
#10 studies yield non-significance with
#median FI = 6, range 1-27, IQR 1-14 and
#median FQ = 2.3%, range 0.4%-14.3%, IQR 0.9%-5.0%;
#overall, among all studies,
#median FI = 6, range 1-27, IQR 1-13 and
#median FQ = 2.3%, range 0.4%-14.3%, IQR 0.9%-5.0%

## Regional population trials
regional <- fi_sup %>% 
  filter(c_participants.factor == "Regional")

regional_fi <- frag.studies(i_events, i_total, c_events, c_total, regional, 
                            methods = "Fisher")
regional_fi

#The input dataset contains 7 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#2 studies yield significance with
#FI = 3, 12 and
#FQ = 5.0%, 21.1%;
#5 studies yield non-significance with
#median FI = 3, range 1-8, IQR 1-5 and
#median FQ = 3.0%, range 0.8%-13.9%, IQR 0.8%-10.0%;
#overall, among all studies,
#median FI = 3, range 1-12, IQR 3-5 and
#median FQ = 7.5%, range 0.8%-21.1%, IQR 3.0%-10.0%

overall_fi <- frag.studies(i_events, i_total, c_events, c_total, fi_sup, 
                           methods = "Fisher")

overall_fi

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#  70 studies yield significance with
#    median FI = 4, range 1-189, IQR 2-9 and
#    median FQ = 3.0%, range 0.2%-27.6%, IQR 1.3%-5.0%;
#  91 studies yield non-significance with
#    median FI = 4, range 1-46, IQR 2-6 and
#    median FQ = 3.7%, range 0.0%-17.1%, IQR 1.4%-6.0%;
#  overall, among all studies,
#    median FI = 4, range 1-189, IQR 2-8 and
#   median FQ = 3.3%, range 0.0%-27.6%, IQR 1.3%-5.3%

############################################################
# Table 3
############################################################

table(fi_sup$i_type.factor)

fi_sup <- fi_sup %>%
  mutate(
    concealment_group =
      ifelse(
        rct_conceal.factor == "Unclear",
        "No / unclear",
        "Yes"
      )
  )

drug_yes <- fi_sup %>%
  filter(
    i_type.factor == "Drug-related",
    concealment_group == "Yes"
  )

drug_no <- fi_sup %>%
  filter(
    i_type.factor == "Drug-related",
    concealment_group == "No / unclear"
  )

drug_yes_fi <- frag.studies(
  i_events, i_total,
  c_events, c_total,
  drug_yes,
  methods = "Fisher"
)

drug_no_fi <- frag.studies(
  i_events, i_total,
  c_events, c_total,
  drug_no,
  methods = "Fisher"
)

nondrug_yes <- fi_sup %>%
  filter(
    i_type.factor == "Non-drug related",
    concealment_group == "Yes"
  )

nondrug_no <- fi_sup %>%
  filter(
    i_type.factor == "Non-drug related",
    concealment_group == "No / unclear"
  )

nondrug_yes_fi <- frag.studies(
  i_events, i_total,
  c_events, c_total,
  nondrug_yes,
  methods = "Fisher"
)

nondrug_no_fi <- frag.studies(
  i_events, i_total,
  c_events, c_total,
  nondrug_no,
  methods = "Fisher"
)

drug_yes_fi
drug_no_fi
nondrug_yes_fi
nondrug_no_fi

#> drug_yes_fi
#The input dataset contains 76 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#30 studies yield significance with
#median FI = 5, range 1-189, IQR 1-10 and
#median FQ = 2.7%, range 0.2%-27.6%, IQR 1.3%-4.9%;
#46 studies yield non-significance with
#median FI = 4, range 1-46, IQR 2-10 and
#median FQ = 2.5%, range 0.0%-17.1%, IQR 1.3%-4.0%;
#overall, among all studies,
#median FI = 5, range 1-189, IQR 2-12 and
#median FQ = 2.7%, range 0.0%-27.6%, IQR 1.3%-4.3%
#> drug_no_fi
#The input dataset contains 18 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#9 studies yield significance with
#median FI = 3, range 1-61, IQR 1-6 and
#median FQ = 1.4%, range 0.2%-15.3%, IQR 1.1%-5.0%;
#9 studies yield non-significance with
#median FI = 4, range 2-8, IQR 3-5 and
#median FQ = 4.9%, range 0.6%-16.7%, IQR 2.5%-13.9%;
#overall, among all studies,
#median FI = 4, range 1-61, IQR 2-6 and
#median FQ = 3.7%, range 0.2%-16.7%, IQR 1.2%-10.9%
#> nondrug_yes_fi
#The input dataset contains 33 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#14 studies yield significance with
#median FI = 3, range 1-25, IQR 2-5 and
#median FQ = 2.3%, range 0.2%-10.6%, IQR 0.8%-3.8%;
#19 studies yield non-significance with
#median FI = 5, range 1-24, IQR 3-5 and
#median FQ = 5.0%, range 0.3%-10.0%, IQR 1.7%-6.2%;
#overall, among all studies,
#median FI = 4, range 1-25, IQR 2-5 and
#median FQ = 3.6%, range 0.2%-10.6%, IQR 1.2%-5.0%
#> nondrug_no_fi
#The input dataset contains 34 studies
#Significance level = 0.05
#p-value (two-sided) is based on:
#  Fisher's exact test  

#Fragility index (FI) and fragility quotient (FQ):
#Based on Fisher's exact test,
#17 studies yield significance with
#median FI = 4, range 1-35, IQR 1-10 and
#median FQ = 3.3%, range 0.5%-21.1%, IQR 1.0%-9.4%;
#17 studies yield non-significance with
#median FI = 4, range 1-6, IQR 2-5 and
#median FQ = 4.0%, range 0.0%-12.0%, IQR 0.8%-6.0%;
#overall, among all studies,
#median FI = 4, range 1-35, IQR 2-6 and
#median FQ = 4.0%, range 0.0%-21.1%, IQR 1.0%-7.9%