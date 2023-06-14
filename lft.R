# Modeling of the key LFT outcomes by CT findings and parameters 
# (manually identified findings, CTSS, AI opacity and AI high opacity).
# The responses are 
# 1) presence of any LFT findings defined as parameters <80% or <70%
# 2) presence of impaired DLCO (< 80% reference)
# 3) DLCO expressed as percentage of the reference
#
# In addition , a 'univariate' analysis is done, i.e. comparison and 
# correlation of the CT variables with the outcomes

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(exda)
  library(rstatix)
  library(caret)
  library(caretExtra)
  library(effectsize)
  library(ranger)

  library(soucer)
  library(furrr)
  library(doParallel)

  library(OptimalCutpoints)
  library(plotROC)

  library(ggrepel)

  explore <- exda::explore

  c('./tools/tools.R', 
    './tools/globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
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
  
  ## clinical baseline variables and severity of acute COVID-19
  ## 'pulmonary_comorbidity' is defined as presence of any pulmonary, COPD, 
  # asthma, ILD, or chronic lung disease prior to COVID-19
  ## 'anticoagulant_treatment' subsumes anti-coagulant and ant-platelet therapy
  ## 'antiinfective_treatment' subsumes macrolide and othe antibiotic treatment
  
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
                       lft_globals$baseline_variables), 
                     collapse = ' + '), 
               sep = ' ~ ')) %>% 
    map(formula) %>% 
    set_names(lft_globals$responses)
  
# global analysis table -------
  
  insert_msg('Analysis tables')
  
  ## recoding variables as specified above
  ## the table should contain complete cases only.
  
  lft_globals$analysis_tbl <- covild[c("baseline", "ct", "lft")]

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
           severity_WHO = factor(severity_WHO))
  
  lft_globals$analysis_tbl <- 
    map2(lft_globals$analysis_tbl, 
         list(c('ID', lft_globals$baseline_variables), 
              c('ID', lft_globals$ct_variables), 
              c('ID', 'follow_up', lft_globals$responses)), 
         ~.x[.y])
  
  lft_globals$analysis_tbl <- lft_globals$analysis_tbl[c("lft", "ct")] %>% 
    map(filter, follow_up != 'acute COVID-19') %>% 
    map(mutate, follow_up = droplevels(follow_up)) %>% 
    reduce(left_join, by = c('ID', 'follow_up')) %>% 
    left_join(lft_globals$analysis_tbl$baseline, by = 'ID') %>% 
    filter(complete.cases(.))
  
  ## setting unique 'observation_id' containing the patient ID and timepoint
  
  lft_globals$analysis_tbl <- lft_globals$analysis_tbl %>% 
    mutate(observation_id = paste(ID, 
                                  stri_extract(follow_up, regex = '\\d+'), 
                                  sep = '_')) %>% 
    column_to_rownames('observation_id')
  
# Variable lexicon --------
  
  insert_msg('Variable lexicon')
  
  lft_globals$lexicon <- 
    covild[c("baseline_lexicon", "ct_lexicon", "lft_lexicon")] %>% 
    reduce(rbind) %>% 
    filter(variable %in% c(lft_globals$responses, 
                           lft_globals$ct_variables, 
                           lft_globals$baseline_variables)) %>%
    rbind(tibble(variable = 'follow_up', 
                 label = 'follow-up', 
                 unit = NA, 
                 description = 'Follow-up after COVID-19', 
                 format = 'factor', 
                 table_label = 'follow-up'))
  
  lft_globals$lexicon[lft_globals$lexicon$variable == 'severity_WHO', 'format'] <- 
    'factor'
  
# CV folds -------
  
  insert_msg('CV folds')
  
  ## The dataset observations are partially participant-matched
  ## In order to preserve this structure I'll define folds by participants
  ## i.e. the entire series per participant is used either for training
  ## or for testing and never split between the subsets. 
  ## Such CV fold are used to tune RF models to predict individuals trajectories
  ## of LFT responses
  ##
  ## Additionally, I'm also providing CV folds without participant matching. 
  ## Such CV structure will be used to train models that simply 
  ## detect LFT abnormalities
  
  ## selecting the participants
  
  lft_globals$unique_id <- lft_globals$analysis_tbl$ID %>% 
    unique
  
  set.seed(12345)
  
  lft_globals$folds_participants <- 
    createMultiFolds(lft_globals$unique_id, 
                     k = 10, 
                     times = 5)
  
  lft_globals$folds_participants <- 
    lft_globals$folds_participants %>% 
    map(~lft_globals$unique_id[.x])
  
  ## obtaining the observation indexes
  
  lft_globals$indexes$matched <- lft_globals$folds_participants %>% 
    map(function(x) lft_globals$analysis_tbl %>% 
          mutate(index = 1:nrow(.)) %>% 
          filter(ID %in% x)) %>% 
    map(~.x$index)
  
  set.seed(12345)
  
  lft_globals$indexes$unmatched <- lft_globals$analysis_tbl %>% 
    rownames %>% 
    createMultiFolds(k = 10, 
                     times = 5)
  
  ## train control objects
  
  lft_globals$train_control <- lft_globals$indexes %>% 
    map(~trainControl(method = 'repeatedcv', 
                      repeats = 5, 
                      number = 10, 
                      savePredictions = 'final', 
                      returnData = TRUE, 
                      returnResamp = 'final', 
                      classProbs = TRUE, 
                      index = .x))

# Analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  ## univariable analysis
  
  c('./LFT scripts/univariable_LFT_findings.R', 
    './LFT scripts/univariable_DLCO_findings.R', 
    './LFT scripts/univariable_DLCO.R', 
    './LFT scripts/univariable_FVC_findings.R', 
    './LFT scripts/univariable_FVC.R',
    './LFT scripts/univariable_FEV1_findings.R', 
    './LFT scripts/univariable_FEV1.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## tuning the models, resorting to cached results

  list(cache_path = c('./cache/lft_rf.RData', 
                      './cache/dlco_red_rf.RData', 
                      './cache/dlco_rf.RData', 
                      './cache/fvc_red_rf.RData', 
                      './cache/fvc_rf.RData', 
                      './cache/fev1_red_rf.RData', 
                      './cache/fev1_rf.RData'), 
       script_path = c('./LFT scripts/rf_LFT_findings.R', 
                       './LFT scripts/rf_DLCO_findings.R', 
                       './LFT scripts/rf_DLCO.R', 
                       './LFT scripts/rf_FVC_findings.R', 
                       './LFT scripts/rf_FVC.R', 
                       './LFT scripts/rf_FEV1_findings.R', 
                       './LFT scripts/rf_FEV1.R'), 
       message = c('Loading cached tuning results for LFT findings', 
                   'Loading cached tuning results for reduced DLCO', 
                   'Loading cached tuning results for DLCO', 
                   'Loading cached tuning results for reduced FVC', 
                   'Loading cached tuning results for FVC', 
                   'Loading cached tuning results for reduced FEV1', 
                   'Loading cached tuning results for FEV1')) %>% 
    pwalk(access_cache)
  
  ## checking model performance: binary classification and regression
  ## analyzed separately
  
  c('./LFT scripts/binary_performance.R', 
    './LFT scripts/regression_performance.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## variable importance: binary and regression classifiers are analyzed
  ## separately
  
  c('./LFT scripts/binary_importance.R', 
    './LFT scripts/regression_importance.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## ROC analysis for the top most important explanatory CT factors
  ## opacity, high opacity and CTSS
  
  c('./LFT scripts/roc_ct.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END -----
  
  insert_tail()