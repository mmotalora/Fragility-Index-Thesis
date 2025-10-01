#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('FragilityIndexRandom_DATA_2024-10-24_1049.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_repeat_instrument)="Repeat Instrument"
label(data$redcap_repeat_instance)="Repeat Instance"
label(data$date_completed)="Date form completed"
label(data$id_extrac)="Name of person extracting data "
label(data$study_year)="Year of publication"
label(data$report_id)="Report ID"
label(data$study_reference)="Reference citation"
label(data$study_type)="Study type"
label(data$c_type)="Is it a randomized controlled trial made in humans?"
label(data$c_intervention)="Is an interventional study?"
label(data$c_participants)="Participants"
label(data$participants_other)="Other"
label(data$c_design)="Type of design"
label(data$c_allocation)="Employing 1:1 allocation?"
label(data$type_outcome)="Is it a binary outcome?"
label(data$included)="Does the study meet the inclusion criteria?"
label(data$reason_exclusion)="Reason for exclusion"
label(data$notes_eleg)="Notes"
label(data$location11)="Location"
label(data$location12)="Location"
label(data$location13)="Location"
label(data$location14)="Location"
label(data$location15)="Location"
label(data$location16)="Location"
label(data$rct_aim)="Study aim"
label(data$o_aim)="Other study aim "
label(data$rct_type)="Type of Study"
label(data$o_type)="Other types of study"
label(data$i_type)="Type of intervention"
label(data$u_allocation)="Unit of allocation"
label(data$rct_blind)="Type of blinding"
label(data$rct_conceal)="Method of allocation concealment"
label(data$o_conceal)="Other method of allocation concealment"
label(data$rct_centers)="Number of participating centers"
label(data$ethic)="Ethical approval obtained?"
label(data$funding)="Source of funding"
label(data$t_nifunding)="Type of Non-industry funding"
label(data$d_share)="Trial data-sharing"
label(data$notes_charc_studies)="Notes"
label(data$selection_extraction_form_for_individual_studies_complete)="Complete?"
label(data$out_name)="Outcome name"
label(data$out_definition)="Outcome definition"
label(data$miss_data)="Type of imputation analysis"
label(data$o_imputation)="Other type of imputation method"
label(data$sample_calc)="Sample size calculation"
label(data$sample_size)="Total sample size"
label(data$location59)="location59"
label(data$location60)="location60"
label(data$location61)="location61"
label(data$location62)="location62"
label(data$out_notes)="out_notes"
label(data$outcomes_complete)="Complete?"
label(data$comparison)="Comparison "
label(data$intervention)="Intervention"
label(data$i_events)="Number of patients with the event in the intervention group"
label(data$i_total)="Total of patients in the intervention group"
label(data$c_events)="Number of patients with the event in the comparison group"
label(data$c_total)="Total of patients in the comparison group"
label(data$p_value_i)="p-value"
label(data$c_missing)="Number of missing participants in the comparison group"
label(data$i_missing)="Number of missing participants in the intervention group"
label(data$n_movedi)="Number of patients moved from intervention"
label(data$n_movedc)="Number of patients moved from comparison"
label(data$data_notes)="Notes"
label(data$data_analysis_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("outcomes","data_analysis"))
data$id_extrac.factor = factor(data$id_extrac,levels=c("1","2","3","4","5","6","7"))
data$study_type.factor = factor(data$study_type,levels=c("1","2","3","77"))
data$c_type.factor = factor(data$c_type,levels=c("1","2","3"))
data$c_intervention.factor = factor(data$c_intervention,levels=c("1","2","3"))
data$c_participants.factor = factor(data$c_participants,levels=c("1","2","3","4","5","77"))
data$c_design.factor = factor(data$c_design,levels=c("1","2","3","4"))
data$c_allocation.factor = factor(data$c_allocation,levels=c("1","2","3"))
data$type_outcome.factor = factor(data$type_outcome,levels=c("1","2","3"))
data$included.factor = factor(data$included,levels=c("1","0"))
data$rct_aim.factor = factor(data$rct_aim,levels=c("1","2","3","4"))
data$rct_type.factor = factor(data$rct_type,levels=c("1","2","3","4"))
data$i_type.factor = factor(data$i_type,levels=c("1","2"))
data$u_allocation.factor = factor(data$u_allocation,levels=c("1","2","3"))
data$rct_blind.factor = factor(data$rct_blind,levels=c("1","2","3","4"))
data$rct_conceal.factor = factor(data$rct_conceal,levels=c("1","2","3","4","5","6"))
data$rct_centers.factor = factor(data$rct_centers,levels=c("1","2","3"))
data$ethic.factor = factor(data$ethic,levels=c("1","2","3"))
data$funding.factor = factor(data$funding,levels=c("1","2","3"))
data$t_nifunding.factor = factor(data$t_nifunding,levels=c("1","2","3","4"))
data$d_share.factor = factor(data$d_share,levels=c("1","2","3"))
data$selection_extraction_form_for_individual_studies_complete.factor = factor(data$selection_extraction_form_for_individual_studies_complete,levels=c("0","1","2"))
data$miss_data.factor = factor(data$miss_data,levels=c("1","2","3","4","5","6","7"))
data$sample_calc.factor = factor(data$sample_calc,levels=c("1","2","3"))
data$outcomes_complete.factor = factor(data$outcomes_complete,levels=c("0","1","2"))
data$data_analysis_complete.factor = factor(data$data_analysis_complete,levels=c("0","1","2"))

levels(data$redcap_repeat_instrument.factor)=c("Outcomes","Data & Analysis")
levels(data$id_extrac.factor)=c("Juan Diego Aristizabal","Lorena Díaz","Verónica Echeverry","Felipe Muñoz","Margarita Otálora","Juan Camilo Segura","Amir Zabida")
levels(data$study_type.factor)=c("Randomized Clinical Trial","Systematic review","Meta analysis","Other")
levels(data$c_type.factor)=c("Yes","No","Unclear")
levels(data$c_intervention.factor)=c("Yes","No","Unclear")
levels(data$c_participants.factor)=c("Adults","Pediatric","Obstetric","Cardiovascular","Regional","Other")
levels(data$c_design.factor)=c("Parallel-arm trial","Two-by-two factorial trial","Crossover","Other")
levels(data$c_allocation.factor)=c("Yes","No","Unclear")
levels(data$type_outcome.factor)=c("Yes","No","Unclear")
levels(data$included.factor)=c("Yes","No")
levels(data$rct_aim.factor)=c("Efficacy","Equivalence","Pragmatic","Other")
levels(data$rct_type.factor)=c("Superiority trial","Non-inferiority trial","Equivalence trial","Other")
levels(data$i_type.factor)=c("Drug-related","Non-drug related")
levels(data$u_allocation.factor)=c("Individuals","Cluster/groups","Body parts")
levels(data$rct_blind.factor)=c("Single blind","Double blind","Triple blind","Unblinded")
levels(data$rct_conceal.factor)=c("Sequentially numbered sealed/opaque envelopes","Sequentially numbered containers","Pharmacy controlled allocation","Central allocation (site remote from trial location)","Other","Unclear")
levels(data$rct_centers.factor)=c("Single-center","Multicenter","Unclear")
levels(data$ethic.factor)=c("Yes","No","Unclear")
levels(data$funding.factor)=c("Industry-funded","Non-industry funded","Not reported")
levels(data$t_nifunding.factor)=c("Governmental","Institutional","Non-profit organization","Unclear")
levels(data$d_share.factor)=c("Yes","No","Unclear")
levels(data$selection_extraction_form_for_individual_studies_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$miss_data.factor)=c("Single","Multiple","Model-based","Complete analysis","Other","Unclear","Not used")
levels(data$sample_calc.factor)=c("Yes","No","Unclear")
levels(data$outcomes_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$data_analysis_complete.factor)=c("Incomplete","Unverified","Complete")
