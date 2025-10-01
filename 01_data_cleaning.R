library(remotes)
library(dplyr)
library(tidyr)

summary(data)


library(writexl)
write_xlsx(data, path = "full_dataset.xlsx")

names(data)

c_data  <- data %>% 
  select(-redcap_repeat_instrument, -redcap_repeat_instance, -id_extrac, 
         -id_extrac.factor, -location11, -location12, -location13, -location14,
         -location15, -location16, -notes_charc_studies, 
         -selection_extraction_form_for_individual_studies_complete, 
         -selection_extraction_form_for_individual_studies_complete.factor, 
         -location59, -location60, -location61, -location62, -out_notes, 
         -outcomes_complete, -outcomes_complete.factor, -data_notes, 
         -data_analysis_complete, -data_analysis_complete.factor, 
         -redcap_repeat_instrument.factor) 

summary(c_data)

## Checking for errors based on inclusion criteria
s_type <- c_data %>% 
  filter(c_type == "2", included == "1")
s_type

intervention <- c_data %>% 
  filter(c_intervention == "2", included == "1")
intervention

allocation <- c_data %>% 
  filter(c_allocation == "2", included == "1") 
allocation

type_outcome <- c_data %>% 
  filter(type_outcome == "2", included == "1") 
type_outcome

clusters <- c_data %>% 
  filter(u_allocation == "2", included == "1") 
clusters

non_inferior <- c_data %>% 
  filter(rct_type == "2", included == "1") 


### Defining subsets #####
### Studies identified from guidelines 
eligibility <- c_data %>% 
  select(record_id, study_year, report_id, included, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included.factor) %>% 
  drop_na(included)

summary(eligibility)

# ----> Identifying incomplete records
missing_rows <- which(is.na(eligibility$type_outcome.factor))

eligibility[missing_rows, ] ## 0 record identified as incomplete. 


### Studies eligible for FI analysis 

included <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes") %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

summary(included)

### Sample

sample <- c_data %>%
  select(record_id, report_id, study_year, included, c_participants.factor, 
         c_design.factor, rct_aim.factor, rct_type.factor, i_type.factor, 
         u_allocation.factor, rct_blind.factor, rct_conceal.factor, 
         rct_centers.factor, ethic.factor, funding.factor, d_share.factor, 
         miss_data.factor, sample_calc.factor, sample_size, i_events, i_total, 
         c_events, c_total, c_missing, i_missing, n_movedi, n_movedc) %>% 
  fill(included, .direction = "down") %>% 
  filter(included == "1") %>% 
  fill(report_id, study_year, c_participants.factor, c_design.factor, 
       rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor, 
       rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor, 
       funding.factor, d_share.factor, miss_data.factor, sample_calc.factor, 
       sample_size, .direction = "down") %>% 
  drop_na(i_events, i_total, c_events, c_total) %>% 
  distinct(.keep_all = TRUE)

#### The list of studies included for the analysis###
sample_list <- sample %>% 
  select(record_id, report_id, study_year, c_participants.factor, c_design.factor, 
         rct_aim.factor, rct_type.factor, i_type.factor, u_allocation.factor, 
         rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor, 
         funding.factor, d_share.factor, miss_data.factor, sample_calc.factor, 
         sample_size) %>% 
  mutate(across(where(is.factor), droplevels))


### List of studies by group
general_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Adults" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

peds_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Pediatric" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

obstetric_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Obstetric" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

cv_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Cardiovascular" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

regional_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Regional" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)

other_list <- c_data %>% 
  select(record_id, study_year, report_id, included.factor, study_type.factor, c_type.factor, 
         c_intervention.factor, c_participants.factor, c_design.factor, 
         c_allocation.factor, type_outcome.factor, included, rct_aim.factor, 
         rct_type.factor, i_type.factor, u_allocation.factor, rct_blind.factor, 
         rct_conceal.factor, rct_centers.factor, ethic.factor, funding.factor, 
         t_nifunding.factor, d_share.factor) %>% 
  filter(included.factor == "Yes", c_participants.factor == "Other" ) %>% 
  drop_na(rct_aim.factor,rct_type.factor, i_type.factor, u_allocation.factor, 
          rct_blind.factor, rct_conceal.factor, rct_centers.factor, ethic.factor,
          funding.factor, d_share.factor)
