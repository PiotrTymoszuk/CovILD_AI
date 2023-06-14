# Import and formatting of the Radio CovILD study data
#
# I'm fetching them from the Warehouse Package CovILD

# tools ------

  library(tidyverse)
  library(trafo)
  library(stringi)
  
  library(covILD)
  
  library(soucer)

  insert_head()

  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# container ------
  
  insert_msg('Data container')
  
  covild <- list()
  
# Data import -------
  
  insert_msg('Data import')
  
  covild[c('baseline', 
           'baseline_lexicon', 
           'ct', 
           'ct_lexicon', 
           'lft', 
           'lft_lexicon')] <- list(covILD::baseline, 
                                   covILD::baseline_lexicon, 
                                   covILD::ct, 
                                   covILD::ct_lexicon, 
                                   covILD::lft, 
                                   covILD::lft_lexicon)
  
# Restricting to the patients present in all datasets ------
  
  insert_msg('Resticting to the patients present in all datasets')
  
  covild$common_ids <- covild[c("baseline", "ct", "lft")] %>% 
    map(~.x$ID) %>% 
    map(unique) %>% 
    reduce(intersect)
  
  covild[c("baseline", "ct", "lft")] <- 
    covild[c("baseline", "ct", "lft")] %>% 
    map(filter, ID %in% covild$common_ids)
  
# Baseline lexicon -----
  
  insert_msg('Baseline lexicon')
  
  base_lex <- 
    c('sex' = 'sex', 
      'age' = 'age at admission', 
      'age_class' = 'age class', 
      'smoking' = 'smoking history', 
      'smoking_pack_years' = 'smoking', 
      'BMI' = 'BMI', 
      'body_mass_class' = 'body mass class', 
      'cardiovascular_comorbidity' = 'cardiovascular illness', 
      'hypertension_comorbidity' = 'hypertension', 
      'pulmonary_comorbidity' = 'pulmonary illness', 
      'COPD_comorbidity' = 'COPD', 
      'asthma_comorbidity' = 'asthma', 
      'interstitial_lung_comorbidity' = 'interstitial lung disease', 
      'endocrine_metabolic_comorbidity' = 'endocrine/metabolic disease', 
      'hypercholesterolemia_comorbidity' = 'hypercholesterolemia', 
      'diabetes_comorbidity' = 'type II diabetes mellitus', 
      'CKD_comorb' = 'CKD', 
      'gastrointestinal_comorb' = 'gastrointestinal disease', 
      'chronic_lung_disease_comorbidity' = 'chronic lung disease', 
      'malingnant_comorbidity' = 'malignancy', 
      'immdeficiency_comorbidity' = 'immune deficiency', 
      'severity_class' = 'COVID-19 severity', 
      'severity_WHO' = 'WHO ordinal severity scale', 
      'hospital_stay_days' = 'hospital stay', 
      'ICU_stay_days' = 'ICU stay', 
      'steroid_treatment' = 'steroids during COVID-19', 
      'antiinfective_treatment' = 'antiinfectives during COVID-19', 
      'macrolide_treatment' = 'macrolides during COVID-19', 
      'antiplatelet_treatment' = 'anti-platelet drugs during COVID-19', 
      'anticoagulant_treatment' = 'anti-coagulants during COVID-19', 
      'immunosuppressive_treatment' = 'immunosuppressive drugs during COVID-19', 
      'weight_change_kg' = 'weight change during COVID-19')
  
  base_units <- 
    c('age' = 'years', 
      'smoking_pack_years' = 'pack-years', 
      'BMI' = 'kg/m\u00B2', 
      'hospital_stay_days' = 'days', 
      'ICU_stay_days' = 'days', 
      'weight_change_kg' = 'kg')

# CT lexicon ------
  
  insert_msg('CT lexicon')
  
  ct_lex <- 
    c('follow-up' = 'follow-up', 
      'CT_findings' = 'any CT findings', 
      'GGO_finding' = 'GGO', 
      'reticulation_finding' = 'reticulation', 
      'consolidation_finding' = 'consolidation', 
      'bronchiectasis_finding' = 'bronchiectasis', 
      'crazy_paving_finding' = 'crazy paving', 
      'ARDS_pattern_finding' = 'ARDS pattern', 
      'opacitiy_finding' = 'opacity', 
      'CTSS' = 'CTSS', 
      'opacity_percent' = 'opactity, AI', 
      'high_opacity_percent' = 'high opacity, AI')
  
  ct_units <- 
    c('opacity_percent' = '% of lung', 
      'high_opacity_percent' = '% of lung', 
      'CTSS' = 'points')
  
# Lung function lexicon ------
  
  insert_msg('Lung function lexicon')
  
  lft_lex <- 
    c('follow-up' = 'follow-up', 
      'LFT_findings' = 'any LFT findings', 
      'FVC_volume' = 'FVC', 
      'FVC_percent' = 'FVC',
      'FVC_reduced' = 'FVC < 80%', 
      'FEV1_volume' = 'FEV1', 
      'FEV1_percent' = 'FEV1', 
      'FEV1_reduced' = 'FEV1 < 80%', 
      'FEV1_FVC_percent' = 'FEV1:FVC', 
      'FEV1_FVC_reduced' = 'FEV1:FVC < 70%', 
      'DLCO' = 'DLCO', 
      'DLCO_percent' = 'DLCO', 
      'DLCO_reduced' = 'DLCO < 80%', 
      'TLC_volume' = 'TLC', 
      'TLC_percent' = 'TLC', 
      'RV_volume' = 'RV', 
      'RV_percent' = 'RV')
  
  lft_units <- 
    c('FVC_volume' = 'L', 
      'FVC_percent' = '% of reference', 
      'FEV1_volume' = 'L', 
      'FEV1_percent' = '% of reference', 
      'FEV1_FVC_percent' = '% of reference', 
      'DLCO' = 'CO/min/kPa', 
      'DLCO_percent' = '% of reference', 
      'TLC_volume' = 'L', 
      'TLC_percent' = '% of reference', 
      'RV_volume' = 'L', 
      'RV_percent' = '% of reference')
  
# Information abuut the variable format, table label ------
  
  insert_msg('Information about the variable format, table label')
  
  covild[c("baseline_lexicon", "ct_lexicon", "lft_lexicon")] <- 
    list(label_vector = list(base_lex, ct_lex, lft_lex), 
         unit_vector = list(base_units, ct_units, lft_units), 
         lexicon = covild[c("baseline_lexicon", "ct_lexicon", "lft_lexicon")], 
         data = covild[c("baseline", "ct", "lft")]) %>% 
    pmap(make_lexicon)
  
# END ------
  
  rm(base_lex, base_units, ct_lex, ct_units, lft_lex, lft_units)
  
  insert_tail()