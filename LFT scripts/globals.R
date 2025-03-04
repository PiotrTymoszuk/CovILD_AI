## Setup of the modeling globals

  insert_head()

# globals container -------

  lft_globals <- list()

# analysis variables ------

  insert_msg('Analysis variables')
  
  lft_globals$responses <- 
    c('LFT_findings', 
      'DLCO_reduced', 'DLCO_percent', 
      'FVC_reduced', 'FVC_percent', 
      'FEV1_reduced', 'FEV1_percent')
  
  lft_globals$ct_variables <- 
    c('follow_up', 'CT_findings', 'GGO_finding', 'reticulation_finding', 
      'consolidation_finding', 'bronchiectasis_finding', 
      'opacity_percent', 'high_opacity_percent', 'CTSS')
  
  lft_globals$symptom_variables <- covild$symptoms_lexicon$variable

  ## clinical baseline variables and severity of acute COVID-19
  ## 'pulmonary_comorbidity' is defined as presence of any pulmonary, COPD, 
  # asthma, ILD, or chronic lung disease prior to COVID-19
  ## 'anticoagulant_treatment' subsumes anti-coagulant and ant-platelet therapy
  ## 'antiinfective_treatment' subsumes macrolide and other antibiotic treatment
  ## 'smoking': ex- and active smokers are grouped together
  
  lft_globals$baseline_variables <- 
    c('sex', 'age', 'age_class', 
      'smoking', 'smoking_pack_years', 
      'BMI', 'body_mass_class', 
      'cardiovascular_comorbidity', 
      'hypertension_comorbidity', 
      'pulmonary_comorbidity', 
      'endocrine_metabolic_comorbidity', 
      'hypercholesterolemia_comorbidity', 
      'diabetes_comorbidity', 
      'gastrointestinal_comorb', 
      'malingnant_comorbidity', 
      'hospital_stay_days', 
      'ICU_stay_days', 
      'steroid_treatment', 
      'antiinfective_treatment',
      'anticoagulant_treatment', 
      'weight_change_kg', 
      'severity_WHO', 
      'severity_class')

  ## modeling formulas
  
  lft_globals$formulas <- lft_globals$responses %>% 
    map(~paste(.x, 
               paste(c(lft_globals$ct_variables, 
                       lft_globals$symptom_variables, 
                       lft_globals$baseline_variables), 
                     collapse = ' + '), 
               sep = ' ~ ')) %>% 
    map(formula) %>% 
    set_names(lft_globals$responses)

# global analysis table -------

  insert_msg('Analysis tables')
  
  ## recoding variables as specified above
  ## the table should contain complete cases only.
  
  lft_globals$analysis_tbl <- covild[c("baseline", "ct", "symptoms", "lft")]
  
  lft_globals$analysis_tbl$baseline <- 
    lft_globals$analysis_tbl$baseline %>% 
    mutate(pulmonary_comorbidity = ifelse(pulmonary_comorbidity == 'yes' | 
                                            COPD_comorbidity == 'yes' | 
                                            asthma_comorbidity == 'yes' | 
                                            interstitial_lung_comorbidity == 'yes' | 
                                            chronic_lung_disease_comorbidity == 'yes', 
                                          'yes', 'no'), 
           pulmonary_comorbidity = factor(pulmonary_comorbidity, c('no', 'yes')), 
           anticoagulant_treatment = ifelse(anticoagulant_treatment == 'yes' | 
                                              antiplatelet_treatment == 'yes', 
                                            'yes', 'no'),
           anticoagulant_treatment = factor(anticoagulant_treatment, c('no', 'yes')), 
           antiinfective_treatment = ifelse(antiinfective_treatment == 'yes' | 
                                              macrolide_treatment == 'yes', 
                                            'yes', 'no'), 
           antiinfective_treatment = factor(antiinfective_treatment, c('no', 'yes')), 
           severity_WHO = factor(severity_WHO), 
           smoking = car::recode(as.character(smoking), 
                                 "'never smoker' = 'never'; 
                                 'ex-smoker' = 'ex or active'; 
                                 'active smoker' = 'ex or active'"), 
           smoking = factor(smoking, c('never', 'ex or active')))
  
  lft_globals$analysis_tbl <- 
    map2(lft_globals$analysis_tbl, 
         list(c('ID', lft_globals$baseline_variables), 
              c('ID', lft_globals$ct_variables), 
              c('ID', lft_globals$symptom_variables), 
              c('ID', 'follow_up', lft_globals$responses)), 
         ~.x[.y])
  
  lft_globals$analysis_tbl <- 
    lft_globals$analysis_tbl[c("lft", "ct", "symptoms")] %>% 
    map(filter, follow_up != 'acute COVID-19') %>% 
    map(mutate, follow_up = droplevels(follow_up)) %>% 
    reduce(left_join, by = c('ID', 'follow_up')) %>% 
    left_join(lft_globals$analysis_tbl$baseline, by = 'ID') %>% 
    filter(complete.cases(.))
  
  ## setting unique 'observation_id' containing the patient ID and timepoint
  ## defining the observation index, which will be later used for definition 
  ## of the participant-matched CV folds
  
  lft_globals$analysis_tbl <- lft_globals$analysis_tbl %>% 
    mutate(observation_id = paste(ID, 
                                  stri_extract(follow_up, regex = '\\d+'), 
                                  sep = '_'), 
           index = 1:nrow(.)) %>% 
    column_to_rownames('observation_id')

# Variable lexicon --------

  insert_msg('Variable lexicon')
  
  lft_globals$lexicon <- 
    covild[c("baseline_lexicon", 
             "symptoms_lexicon", 
             "ct_lexicon", 
             "lft_lexicon")] %>% 
    reduce(rbind) %>% 
    filter(variable %in% c(lft_globals$responses, 
                           lft_globals$ct_variables, 
                           lft_globals$symptom_variables, 
                           lft_globals$baseline_variables)) %>%
    rbind(tibble(variable = 'follow_up', 
                 label = 'follow-up', 
                 unit = NA, 
                 description = 'Follow-up after COVID-19', 
                 format = 'factor', 
                 table_label = 'follow-up'))
  
  lft_globals$lexicon[lft_globals$lexicon$variable == 'severity_WHO', 'format'] <- 
    'factor'

  ## adding classification of the explanatory variables
  
  lft_globals$lexicon <- lft_globals$lexicon %>% 
    mutate(class = ifelse(variable %in% lft_globals$responses, 
                          'response', 
                          ifelse(variable %in% lft_globals$ct_variables, 
                                 'CT readouts and findings', 
                                 ifelse(variable %in% lft_globals$symptom_variables, 
                                        'symptoms', 
                                        ifelse(variable %in% lft_globals$baseline_variables, 
                                               'risk factors and CoV severity', 
                                               'time after CoV')))), 
           class = factor(class, 
                          c('risk factors and CoV severity', 
                            'symptoms', 
                            'time after CoV', 
                            'CT readouts and findings')))
  
  ## colors of the variable classes
  
  lft_globals$class_colors <- 
    c('risk factors and CoV severity' = 'steelblue', 
      'symptoms' = 'plum4', 
      'time after CoV' = 'aquamarine3', 
      'CT readouts and findings' = 'orangered3')
  
  
# CV folds -------

  insert_msg('CV folds')
  
  ## The dataset observations are partially participant-matched
  ## In order to preserve this structure I'll define folds by participants
  ## i.e. the entire series per participant is used either for training
  ## or for testing and never split between the subsets. 
  ## Such CV fold are used to tune ML models to predict individuals trajectories
  ## of LFT responses
  
  ## selecting the participants
  
  lft_globals$unique_id <- lft_globals$analysis_tbl$ID %>% 
    unique
  
  set.seed(12345)
  
  lft_globals$folds_participants <- 
    createMultiFolds(lft_globals$unique_id, 
                     k = 10, 
                     times = 10)
  
  lft_globals$folds_participants <- 
    lft_globals$folds_participants %>% 
    map(~lft_globals$unique_id[.x])
  
  ## obtaining the observation indexes
  
  lft_globals$indexes <- lft_globals$folds_participants %>% 
    map(~filter(lft_globals$analysis_tbl, 
                ID %in% .x)) %>% 
    map(~.x$index)
  
  ## train control object
  
  lft_globals$train_control <- 
    trainControl(method = 'repeatedcv', 
                 repeats = 10, 
                 number = 10, 
                 savePredictions = 'final', 
                 returnData = TRUE, 
                 returnResamp = 'final', 
                 classProbs = TRUE, 
                 summaryFunction = youden_tuner, 
                 index = lft_globals$indexes)

# END ------
  
  insert_tail()