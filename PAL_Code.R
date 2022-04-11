# Libraries ---------
library(readr)
library(cmprsk)
library(basecamb)
library(readxl) 
library(survminer)
library(dplyr)
library(ggplot2)
library(effects)
library(survival)
library(table1)
library(car)
library(geepack)
library(multgee)
library(doBy)
library(ClusterBootstrap)
library(tidyverse)
library(caret)
library(boot)
library(ResourceSelection)
library(generalhoslem)
library(readxl)
library(lme4)
library(gtsummary)
library(broom)
library(oddsratio)

# create a CSV file with all column names)
# x <- as.data.frame(names(airleak_final))
# write_csv(x, '~/Desktop/airleak_data_dictionary.csv')

# Read in Data ---------
airleak_final <-readxl::read_excel("/Users/mayakashani/Dropbox/Maya_Dessi_Florian_shared/Deidentified_data_2.xlsx")

# Apply Data Dictionary -------
airleak_data_dictionary <- readr::read_csv('/Users/mayakashani/Dropbox/Maya_Dessi_Florian_shared/airleak_data_dictionary.csv')
airleak_cleaned <- basecamb::apply_data_dictionary(airleak_final, airleak_data_dictionary, print_coerced_NA = F)

# Create Subset for included sessions
airleak_cleaned_included <-subset(airleak_cleaned, airleak_cleaned$included_sessions==1)

# Name levels for Tables
airleak_cleaned_included$sex = factor(airleak_cleaned_included$sex, levels = c(1:2), labels = c('Male', 'Female'))
airleak_cleaned_included$modality = factor(airleak_cleaned_included$modality, levels = c(1:0), labels = c('Cryoablation', 'Microwave Ablation'))
airleak_cleaned_included$hospital = factor(airleak_cleaned_included$hospital, levels = c(1:2), labels = c('MGH', 'BWH'))
airleak_cleaned_included$resection = factor(airleak_cleaned_included$resection, levels = c(0:2), labels = c('No prior ipsilateral thoracic surgery', 'Oesophageal Surgery', 'Prior Ipsilateral Resection or Pleurodesis'))
airleak_cleaned_included$ventilation = factor(airleak_cleaned_included$ventilation, levels = c(0:2), labels = c('Spontaneous breathing', 'Jet ventilation', 'Positive pressure ventilation, other than jet'))
airleak_cleaned_included$positive_pressure = factor(airleak_cleaned_included$positive_pressure, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$jet = factor(airleak_cleaned_included$jet, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$lesion_location_lobe = factor(airleak_cleaned_included$lesion_location_lobe, levels = c(1:5), labels = c('RUL', 'RML', 'RLL', 'LUL', 'LLL'))
airleak_cleaned_included$intentional_ptx = factor(airleak_cleaned_included$intentional_ptx, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$costal = factor(airleak_cleaned_included$costal, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$mediastinal = factor(airleak_cleaned_included$mediastinal, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$diaphragmatic = factor(airleak_cleaned_included$diaphragmatic, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$cervical = factor(airleak_cleaned_included$cervical, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$fissure = factor(airleak_cleaned_included$fissure, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$puncture_site = factor(airleak_cleaned_included$puncture_site, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$puncture_site_table1 = factor(airleak_cleaned_included$puncture_site_table1, levels = c(1:3), labels = c('0', '1-2', '≥3'))
airleak_cleaned_included$pleural_punctures_table1 = factor(airleak_cleaned_included$pleural_punctures_table1, levels = c(1:3), labels = c('1', '2-3', '>3'))
airleak_cleaned_included$track_ablation = factor(airleak_cleaned_included$track_ablation, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$freeze_protocol_dual = factor(airleak_cleaned_included$freeze_protocol_dual, levels = c(1:2), labels = c('Triple Freeze', 'Dual Freeze'))
airleak_cleaned_included$active_thaw = factor(airleak_cleaned_included$active_thaw, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$procedural_ptx = factor(airleak_cleaned_included$procedural_ptx, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$ptx_end_of_ablation = factor(airleak_cleaned_included$ptx_end_of_ablation, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$ptx_hospitalization = factor(airleak_cleaned_included$ptx_hospitalization, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$delayed_ptx = factor(airleak_cleaned_included$delayed_ptx, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$chest_tube = factor(airleak_cleaned_included$chest_tube, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$air_leak_24h = factor(airleak_cleaned_included$air_leak_24h, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$ct_enlarging_ptx = factor(airleak_cleaned_included$ct_enlarging_ptx, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$bpf = factor(airleak_cleaned_included$bpf, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$composite_outcome = factor(airleak_cleaned_included$composite_outcome, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$smoking_status = factor(airleak_cleaned_included$smoking_status, levels = c(0:2), labels = c('Never Smoker', 'Former Smoker', 'Active Smoker'))
airleak_cleaned_included$systemic_treatment = factor(airleak_cleaned_included$systemic_treatment, levels = c(0:2), labels = c('No','Chemotherapy', 'Other (i.e. Immunotherapy'))
airleak_cleaned_included$chemo = factor(airleak_cleaned_included$chemo, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$ipsilateral_lung_resection = factor(airleak_cleaned_included$ipsilateral_lung_resection, levels = c(1:0), labels = c('Yes', 'No'))
airleak_cleaned_included$histology = factor(airleak_cleaned_included$histology, levels = c(1:2), labels = c('Primary lung cancer', 'Lung metastases from extrathoracic primary cancer'))
airleak_cleaned_included$number_of_targeted_tumors_table1 = factor(airleak_cleaned_included$number_of_targeted_tumors_table1, levels = c(1:3), labels = c('1', '2-3', '>3'))
airleak_cleaned_included$probes_session_table1 = factor(airleak_cleaned_included$probes_session_table1, levels = c(1:3), labels = c('1', '2-3', '>3'))
airleak_cleaned_included$gauge_14_table1 = factor(airleak_cleaned_included$gauge_14_table1, levels = c(1:3), labels = c('1', '2-3', '>3'))
airleak_cleaned_included$gauge_17_table1 = factor(airleak_cleaned_included$gauge_17_table1, levels = c(1:3), labels = c('1', '2-3', '>3'))
airleak_cleaned_included$ecog_table1 = factor(airleak_cleaned_included$ecog_table1, levels = c(1:0), labels = c('0', '≥1'))
airleak_cleaned_included$diagnosis = factor(airleak_cleaned_included$diagnosis, levels = c(1:2), labels = c('Primary Lung Cancer', 'Lung Metastases from Extrathoracic Primary Cancer'))
airleak_cleaned_included$stage_at_treatment_table1 = factor(airleak_cleaned_included$stage_at_treatment_table1, levels = c(1:2), labels = c('Early-stage Cancer', 'Advanced-stage Cancer'))

#Table 1
table1_paper<-table1(~sex+age+pack_years+number_of_lesions_per_session+procedure_time+procedure_time_per_lesion+ecog_table1+smoking_status+pack_years+diagnosis+stage_at_treatment_table1+chemo+ipsilateral_lung_resection+number_of_targeted_tumors_table1+probes_session+chiba+chiba_table1+probes_session_table1+gauge_17_table1+gauge_16_table1+gauge_14_table1+pleural_punctures_session+pleural_punctures_table1+puncture_site_number_total+puncture_site_table1+total_distance_traversed+costal+diaphragmatic+mediastinal+cervical+total_pleural_area+positive_pressure+jet+ventilation+intentional_ptx+procedure_time| modality, data= airleak_cleaned_included, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table1_paper

#Exlusion criteria
airleak_cleaned_included <-subset(airleak_cleaned, airleak_cleaned$included_sessions==1)
airleak_cleaned_exclusion_criteria<-subset(airleak_cleaned, airleak_cleaned$local_control_inclusion==1)
table(airleak_cleaned$local_control_inclusion)

# Create Subset for stage of lung cancer
airleak_cleaned_lungcancer<-subset(airleak_cleaned_included, airleak_cleaned_included$diagnosis==1)
table1.1_paper<-table1(~stage_at_treatment_table1| modality, data= airleak_cleaned_lungcancer, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))

#GEEGLM Table 1 
airleak_cleaned_included$modality = as.numeric(as.character(airleak_cleaned_included$modality))
summary(geeglm(modality~chest_tube_dwell_time, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~sex, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~age, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~ecog_table1, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
anova(geeglm(modality~factor(smoking_status), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~pack_years, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(diagnosis), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(chemo), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(ipsilateral_lung_resection), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~number_of_lesions_per_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~number_of_targeted_tumors_table1, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~probes_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~probes_session_table1, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~puncture_site_number_total, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~puncture_site_table1, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~total_distance_traversed, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
anova(geeglm(modality~ventilation, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~total_pleural_area, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~jet, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~positive_pressure, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~spontaneous_breathing, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(intentional_ptx), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(costal), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(diaphragmatic), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(mediastinal), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~factor(cervical), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~pleural_punctures_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~pleural_punctures_table1, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~procedure_time, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~primary_technical_success, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

airleak_cleaned_included$stage_at_treatment_table1[airleak_cleaned_included$stage_at_treatment_table1=='NA'] = NA
airleak_cleaned_included = airleak_cleaned_included %>% drop_na(stage_at_treatment_table1)
summary(geeglm(modality~factor(stage_at_treatment_table1), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

# Read in Data ---------
airleak_final <-readxl::read_excel("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/Deidentified_data_2.xlsx")
airleak_data_dictionary <- readr::read_csv('/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/airleak_data_dictionary.csv')
airleak_cleaned <- basecamb::apply_data_dictionary(airleak_final, airleak_data_dictionary, print_coerced_NA = F)
airleak_cleaned_included <-subset(airleak_cleaned, airleak_cleaned$included_sessions==1)

#Table 2
# Create Subset for included lung lesions
airleak_cleaned_lesions<-subset(airleak_cleaned, airleak_cleaned$included==1)
airleak_cleaned_lesions$histology = factor(airleak_cleaned_lesions$histology, levels = c(0:41), labels = c('Multifocal lung cancer', 'Primary Lung Cancer', 'Recurrent Lung Cancer', 'Sarcoma', 'Prostate CA', 'Colorectal CA', 'Metastatic Lung Cancer', 'Uterine CA', 'Melanoma', 'SCC', 'TCC', 'Parotid CA', 'Breast Cancer', 'Esophageal Cancer', 'Submandibular CA','Sublingual CA', 'Mesothelioma', 'Neck and Salivary Gland Cancer', 'Soft Palate CA', 'Nasal Cavity CA', 'HCC', 'Testicular CA', 'Thymoma', 'Solitary Fibrous Tumor', 'Myoepithelioma', 'Mediastinal Germ Cell Tumor', 'Cholangiocarcinoma', 'Laryngeal ACC', 'Hemangiopericytoma', 'Anal Cancer', 'Vaginal Cancer','ACC of the Parathyroid', 'ACC of the Tongue', 'Ovarian CA', 'Lacrimal Gland CA', 'Paraganglioma', 'Esophageal CA', 'Giant Cell CA', 'Adrenal CA', 'Cervical CA', 'Leiomyosarcoma CA', 'Pleiomorphic adenoma'))
table2_paper<-table1(~pleural_punctures_lesion+pleural_punctures_lesion_table1+maximum_diameter+maximum_diameter_table2+distance_to_pleura+lesion_location_lobe+histology+histology_table2+probes_lesion+probes_lesion_table2+chiba+chiba_table1+primary_technical_success| modality, data= airleak_cleaned_lesions, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table2_paper

#Follow-up time by modality
airleak_cleaned_lesions_local_control<-subset(airleak_cleaned_included, airleak_cleaned_included$local_control_inclusion==1)
airleak_cleaned_lesions_local_control$modality = as.numeric(as.character(airleak_cleaned_lesions_local_control$modality))
summary(geeglm(modality~fu, id=as.factor(mrn), data=airleak_cleaned_lesions_local_control, family="binomial"))
fu_paper<-table1(~fu| modality, data=airleak_cleaned_lesions_local_control, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
fu_paper

# Create Subset for included lung lesions | CA
airleak_cleaned_lesions_CA<-subset(airleak_cleaned_lesions, airleak_cleaned_lesions$modality==1)
table2_paper_CA<-table1(~freeze_protocol_dual+mode+freezing_time| modality, data= airleak_cleaned_lesions_CA, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table2_paper_CA

# Create Subset for included lung lesions | MWA
airleak_cleaned_lesions_MWA<-subset(airleak_cleaned_lesions, airleak_cleaned_lesions$modality==0)
table2_paper_MWA<-table1(~watt+ablation_time| modality, data= airleak_cleaned_lesions_MWA, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table2_paper_MWA

#GEEGLM Table 2
airleak_cleaned_lesions$modality = as.numeric(as.character(airleak_cleaned_lesions$modality))
summary(geeglm(modality~maximum_diameter, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~factor(maximum_diameter_table2), id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~distance_to_pleura, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
anova(geeglm(modality~factor(lesion_location_lobe), id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~factor(histology_table2), id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~probes_lesion, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~probes_lesion_table2, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~primary_technical_success, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))
summary(geeglm(modality~pleural_punctures_lesion, id=as.factor(mrn), data=airleak_cleaned_lesions, family="binomial"))

#Table 3
table3_paper<-table1(~ct_unintentional_procedural+intraprocedural_ptx+ptx_within_24h+ptx_more_than_24h+chest_tube+chest_tube_unintentional_ptx+chest_tube_dwell_time+ct_enlarging_ptx+air_leak_24h+air_leak_dwell_time+composite_outcome+bpf+hlos| modality, data= airleak_cleaned_included, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table3_paper
table4.1_paper<-table1(~composite_outcome|hospital, data= airleak_cleaned_included, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))

#Table Exclusion criteria
airleak_cleaned_excluded <-subset(airleak_cleaned, airleak_cleaned$included==0)
table(airleak_cleaned_excluded$exclusion)

#GEEGLM Table 3
airleak_cleaned_included$modality = as.numeric(as.character(airleak_cleaned_included$modality))
summary(geeglm(modality~intraprocedural_ptx, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~ptx_within_24h, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~ptx_more_than_24h, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~chest_tube_unintentional_ptx, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~ct_enlarging_ptx, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~air_leak_24h, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~composite_outcome, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~ct_unintentional_procedural, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
summary(geeglm(modality~hlos, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

airleak_cleaned_included$chest_tube_dwell_time[airleak_cleaned_included$chest_tube_dwell_time=='NA'] = NA
airleak_cleaned_included = airleak_cleaned_included %>% drop_na(chest_tube_dwell_time)
summary(geeglm(modality~chest_tube_dwell_time, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

airleak_cleaned_included$air_leak_dwell_time[airleak_cleaned_included$air_leak_dwell_time=='NA'] = NA
airleak_cleaned_included = airleak_cleaned_included %>% drop_na(air_leak_dwell_time)
summary(geeglm(modality~air_leak_dwell_time, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Table4
table4_paper<-table1(~modality+pleural_punctures_session+puncture_site_number_total+pack_years+chemo+ipsilateral_lung_resection+total_pleural_area+positive_pressure+total_distance_traversed|composite_outcome, data= airleak_cleaned_included, digits.pct = 0, render.continuous=c(.="Median [Q1,Q3]",.="Median [Min,Max]",.="Median [IQR]",.="Mean (SD)"))
table4_paper

#GEEGLM | Table 4
airleak_cleaned_included$composite_outcome = as.numeric(as.character(airleak_cleaned_included$composite_outcome))
airleak_cleaned_cryo$composite_outcome = as.numeric(as.character(airleak_cleaned_cryo$composite_outcome))

# Create Subset for Cryoablation cases
airleak_cleaned_cryo<-subset(airleak_cleaned_included, airleak_cleaned_included$modality==1)
airleak_cleaned_cryo$composite_outcome = as.numeric(as.character(airleak_cleaned_cryo$composite_outcome))
summary(geeglm(composite_outcome~puncture_site_number_total, id=as.factor(mrn), data=airleak_cleaned_cryo, family="binomial"))

# Create Subset for Microwave ablation cases
airleak_cleaned_microwave<-subset(airleak_cleaned_included, airleak_cleaned_included$modality==0)
airleak_cleaned_microwave$composite_outcome = as.numeric(as.character(airleak_cleaned_microwave$composite_outcome))
summary(geeglm(composite_outcome~puncture_site_number_total, id=as.factor(mrn), data=airleak_cleaned_microwave, family="binomial"))

#p=0.63    
airleak_cleaned_cryo$composite_outcome = as.numeric(as.character(airleak_cleaned_cryo$composite_outcome))
summary(geeglm(composite_outcome~mode_subgroup, id=factor(mrn), data=airleak_cleaned_cryo, family="binomial"))

#p=0.0036 
anova(geeglm(composite_outcome~number_of_lesions_per_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.0061
summary(geeglm(composite_outcome~modality, id=factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.84  
summary(geeglm(composite_outcome~puncture_site_number_total, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.097
#summary(geeglm(composite_outcome~active_smoker, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.12
summary(geeglm(composite_outcome~no_ipsilateral_lung_resection, id=factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.15  
summary(geeglm(composite_outcome~(sex), id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#ventilation1    0.265   0.649  0.17     0.68    
#ventilation2    0.547   0.525  1.09     0.30  
summary(geeglm(composite_outcome~ventilation, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
anova(geeglm(composite_outcome~ventilation, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.2    
summary(geeglm(composite_outcome~total_distance_traversed, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.7    
summary(geeglm(composite_outcome~total_pleural_area, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

summary(geeglm(composite_outcome~distance_to_pleura, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

##########
#p=0.47 
summary(geeglm(composite_outcome~chemo, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.42 
summary(geeglm(composite_outcome~pleural_punctures_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p= 0.958
summary(geeglm(composite_outcome~age, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#p=0.53
summary(geeglm(composite_outcome~hospital, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))



#Multivariable Analysis

#Multivariable model of all variables with p<0.4
#modality1                      -1.10e+00  5.31e-01 4.25   0.0392 * 
#number_of_lesions_per_session   2.76e-01  2.12e-01 1.69   0.1936   
#no_ipsilateral_lung_resection1  4.38e-01  5.36e-01 0.67   0.4138   
#total_distance_traversed       -4.55e-06  3.51e-03 0.00   0.9990   
#sex2                           -7.67e-01  5.16e-01 2.21   0.1372     
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+no_ipsilateral_lung_resection+total_distance_traversed+sex, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

##Purposeful selection

#remove "total distance traversed" => coefficients do not change by more than 15%
#modality1                        -1.095   0.480 5.20   0.0226 * 
#number_of_lesions_per_session     0.276   0.138 3.99   0.0456 * 
#no_ipsilateral_lung_resection1    0.438   0.508 0.74   0.3890   
#sex2                             -0.767   0.514 2.23   0.1355  
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+no_ipsilateral_lung_resection+sex, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#remove "no_ipsilateral_lung_resection" => coefficients do not change by more than 15%
#modality1                       -1.136   0.464 6.00   0.0143 * 
#number_of_lesions_per_session    0.303   0.141 4.65   0.0311 * 
#sex2                            -0.761   0.515 2.18   0.1397 
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+sex, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#remove "sex" => coefficients do not change by more than 15%
#modality1                       -1.098   0.461  5.66  0.01737 *  
#number_of_lesions_per_session    0.295   0.134  4.82  0.02811
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Add "pleural_punctures_session"
#modality1                      -1.0529  0.4666  5.09  0.02405 *  
#number_of_lesions_per_session   0.3359  0.1890  3.16  0.07549 .  
#pleural_punctures_session      -0.0436  0.1400  0.10  0.75547 
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+pleural_punctures_session, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Remove "pleural_punctures_session", add "ventilation"
#modality1                       -1.297   0.517  6.30   0.0121 *  
#number_of_lesions_per_session    0.260   0.148  3.06   0.0805 .  
#ventilation1                     0.805   0.708  1.29   0.2558    
#ventilation2                     0.342   0.583  0.34   0.5575     
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+ventilation, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Remove "ventilation", add "age"
#modality1                     -1.09038  0.47192 5.34    0.021 *
#number_of_lesions_per_session  0.29772  0.13170 5.11    0.024 *
#age                            0.00144  0.01906 0.01    0.940 
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+age, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Remove "age", add "puncture_site_number_total"
#modality1                       -1.097   0.452  5.89  0.01519 *  
#number_of_lesions_per_session    0.367   0.132  7.69  0.00555 ** 
#puncture_site_number_total      -0.248   0.209  1.41  0.23525 
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+puncture_site_number_total, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Remove "puncture_site_number_total", add "chemo"
#modality1                       -1.131   0.445  6.47  0.01096 *  
#number_of_lesions_per_session    0.299   0.130  5.25  0.02197 *  
#chemo1                           0.780   0.843  0.86  0.35461 
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+chemo, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))

#Remove "chemo", add "total_pleural_area"
#modality1                     -1.118066  0.454852  6.04  0.01397 *  
#number_of_lesions_per_session  0.531863  0.183827  8.37  0.00381 ** 
#total_pleural_area            -0.000488  0.000306  2.54  0.11106  ###significant at the 0.15 level
summary(geeglm(composite_outcome~modality+number_of_lesions_per_session+total_pleural_area, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))


#STEPWISE REGRESSION
glm_all <- glm(composite_outcome~modality+number_of_lesions_per_session+no_ipsilateral_lung_resection+total_distance_traversed+sex+ventilation, data=airleak_cleaned_included, family="binomial")

backward <- step(glm_all, direction="backward") #returns composite_outcome ~ modality + positive_pressure
backward$coefficients

glm_null <-glm(composite_outcome ~1, data=airleak_cleaned_included, family="binomial")
forward<-step(glm_null, direction="forward", scope=formula(glm_all))  #finds composite_outcome ~ modality + positive_pressure
forward$coefficients

both<-step(glm_all, direction="both") #finds composite_outcome ~ modality + positive_pressure
both$coefficients


#Confidence intervals
y = geeglm(composite_outcome~total_pleural_area, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial")
tidy(y, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE)

#ODDS RATIOS

glmgee_all<-summary(geeglm(composite_outcome~total_pleural_area, id=as.factor(mrn), data=airleak_cleaned_included, family="binomial"))
coef(glmgee_all)
exp(coef(glmgee_all))

#Intrareader
#install.packages("irr")
library("irr")
intrareader <-readxl::read_excel("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Florian_PHI/Air leak project/intrareader.xlsx")
icc(intrareader, model = "twoway", type = "agreement", unit = "single")

#Interreader
interreader <-readxl::read_excel("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Florian_PHI/Air leak project/interreader.xlsx")
icc(interreader, model = "twoway", type = "agreement", unit = "single")

#Fine_Gray Analysis
library(cmprsk)
library(riskRegression)
library(prodlim)
library(survival)
library(survminer)
library(coxme)
library(readxl)
library(table1)
library(frailtyHL)
library(timereg) 
library(crrSC)
library(eventglm)

PAL=read.csv("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/mgus_final_5years.csv")

#variables
PAL$acc <- factor(PAL$acc) 
PAL$modality<-factor(PAL$modality, labels = c("MWA","CA"))#(modality - 1=Microwave ablation; 2=Cryoablation)

etime <- with(PAL, ifelse(pstat==0, futime, ptime))
event <- with(PAL, ifelse(pstat==0, 2*death, 1))
event <- factor(event, 0:2, labels=c("censor", "progression", "death"))
pcmdat <- finegray(Surv(etime, event) ~ ., data=PAL, etype="progression")
deathdat <- finegray(Surv(etime, event) ~ ., data=PAL, etype="death")
dim(pcmdat)
dim(deathdat)
dim(PAL)

# The PCM curves of the multi-state model
pfit2 <- survfit(Surv(fgstart, fgstop, fgstatus) ~ modality,
                 data=pcmdat, weight=fgwt)
# The death curves of the multi-state model
dfit2 <- survfit(Surv(fgstart, fgstop, fgstatus) ~ modality,
                 data=deathdat, weight=fgwt)

#coxph !!!Cluster - ACC# / MRN
fgfit1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ modality + cluster(mrn),
                data=pcmdat,weight= fgwt, id=acc)
fgfit1
summary(fgfit1)
fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ modality + cluster(mrn),
                data=deathdat,weight= fgwt, id=acc)
fgfit2
summary(fgfit2)

ndata <- data.frame(modality=c("MWA", "CA"))
fgsurv1 <- survfit(fgfit1, ndata)

#Plot gaph
plot(fgsurv1, fun="event", lty=2:1, lwd=2, col=c("red", "blue"),
     xscale=12, conf.int=TRUE, conf.times=c(6,30,54),
     xlab="Time to Local Tumor Progression (years)", ylab="Cumulative Incidence Function Estimate")
legend("bottomright", c("Microwave Ablation", "Cryoablation"), col=c("red", "blue"), lty=c(2:1), bty='n')


#Competing Risks Analysis/ Cumulative Incidence
bmt=read.csv("/Users/mayakashani/Dropbox (Partners HealthCare)/Maya_Dessi_Florian_shared/bmt.csv")
dis=factor(dis, levels=c(0:1), labels= c("Microwave Ablation", "Cryoablation"))
ftime=as.numeric(ftime)
tapply(ftime, list(dis, status), mean)
table(dis,status)
status

"CumIncidence" <- function(ftime, fstatus, group, t, strata, rho = 0, 
                           cencode = 0, subset, na.action = na.omit, level,
                           xlab = "Time", ylab = "Probability", 
                           col, lty, lwd, digits = 4)
{
  # check for the required package
  if(!require("cmprsk"))
  { stop("Package `cmprsk' is required and must be installed.\n 
           See help(install.packages) or write the following command at prompt
           and then follow the instructions:\n
           > install.packages(\"cmprsk\")") } 
  # collect data
  mf  <- match.call(expand.dots = FALSE)
  mf[[1]] <- as.name("list")
  mf$t <- mf$digits <- mf$col <- mf$lty <- mf$lwd <- mf$level <- 
    mf$xlab <- mf$ylab <- NULL
  mf <- eval(mf, parent.frame())
  g <- max(1, length(unique(mf$group)))
  s <- length(unique(mf$fstatus))
  if(missing(t)) 
  { time <- pretty(c(0, max(mf$ftime)), 6)
  ttime <- time <- time[time < max(mf$ftime)] }
  else { ttime <- time <- t }
  # fit model and estimates at time points
  fit   <- do.call("cuminc", mf)
  tfit <- timepoints(fit, time)
  # print result
  cat("\n+", paste(rep("-", 67), collapse=""), "+", sep ="")
  cat("\n| Cumulative incidence function estimates from competing risks data |")
  cat("\n+", paste(rep("-", 67), collapse=""), "+\n", sep ="")
  tests <- NULL
  if(g > 1)
  { 
    tests <- data.frame(fit$Tests[,c(1,3,2)], check.names = FALSE)
    colnames(tests) <- c("Statistic", "df", "p-value")
    tests$`p-value` <- format.pval(tests$`p-value`)
    cat("Test equality across groups:\n")
    print(tests, digits = digits) 
  }
  cat("\nEstimates at time points:\n")
  print(tfit$est, digits = digits)
  cat("\nStandard errors:\n")
  print(sqrt(tfit$var), digits = digits)
  #
  if(missing(level))
  { # plot cumulative incidence functions
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) else col
    lty <- if(missing(lty)) rep(1:g, s-1) else lty
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) else lwd 
    matplot(time, base::t(x$est), type="s", ylim = c(0,1), 
            xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
            col = col, lty = lty, lwd = lwd)
    legend("topleft", legend =  rownames(x$est), x.intersp = 2, 
           bty = "n", xjust = 1, col = col, lty = lty, lwd = lwd)
    out <- list(test = tests, est = tfit$est, se = sqrt(tfit$var))
  }
  else
  { if(level < 0 | level > 1) 
    error("level must be a value in the range [0,1]")
    # compute pointwise confidence intervals
    oldpar <- par(ask=TRUE)
    on.exit(par(oldpar))
    if(missing(t))
    { time <- sort(unique(c(ftime, time)))
    x <- timepoints(fit, time) }
    else x <- tfit
    z <- qnorm(1-(1-level)/2)
    lower <- x$est ^ exp(-z*sqrt(x$var)/(x$est*log(x$est)))
    upper <- x$est ^ exp(z*sqrt(x$var)/(x$est*log(x$est)))
    col <- if(missing(col)) rep(1:(s-1), rep(g,(s-1))) 
    else             rep(col, g*(s-1))
    lwd <- if(missing(lwd)) rep(1, g*(s-1)) 
    else             rep(lwd, g*(s-1))      
    # plot pointwise confidence intervals
    for(j in 1:nrow(x$est))
    { matplot(time, cbind(x$est[j,], lower[j,], upper[j,]), type="s", 
              xlab = xlab, ylab = ylab, xaxs="i", yaxs="i", 
              ylim = c(0,1), col = col[j], lwd = lwd[j], lty = c(1,3,3))
      legend("topleft", legend =  rownames(x$est)[j], bty = "n", xjust = 1) }
    # print pointwise confidence intervals
    i <- match(ttime, time)
    ci <- array(NA, c(2, length(i), nrow(lower)))
    ci[1,,] <- base::t(lower[,i])
    ci[2,,] <- base::t(upper[,i])
    dimnames(ci) <- list(c("lower", "upper"), ttime, rownames(lower))
    cat(paste("\n", level*100, "% pointwise confidence intervals:\n\n", sep=""))
    print(ci, digits = digits)
    out <- list(test = tests, est = x$est, se = sqrt(tfit$var), ci = ci)
  }
  # return results
  invisible(out)
}

fit=CumIncidence (ftime, status, dis, cencode = 0, xlab="Months")

