# Modeling of the key LFT outcomes by CT findings and parameters 
# (manually identified findings, CTSS, AI opacity and AI high opacity).

# The responses are 
# 1) presence of any LFT findings defined as parameters <80% or <70%
# 2) presence of impaired DLCO (< 80% reference)
# 3) DLCO expressed as percentage of the reference
#

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(exda)
  library(rstatix)
  library(effectsize)
  library(sciplot)
  library(rcompanion)
  library(clustTools)
  library(zoo)

  library(graphExtra)
  library(igraph)

  library(caret)
  library(caretExtra)

  library(ranger)
  library(nnet)
  library(kernlab)
  library(gbm)

  library(kernelshap)
  library(shapviz)

  library(soucer)
  library(furrr)
  library(doParallel)

  library(ggrepel)
  library(ggtext)

  library(OptimalCutpoints)
  library(plotROC)

  library(figur)

  explore <- exda::explore
  train <- caret::train
  components <- generics::components

  c('./tools/tools.R', 
    './tools/globals.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Setup of the modeling globals -------
  
  insert_msg('Analysis globals')
  
  source_all('./LFT scripts/globals.R', 
             message = TRUE, crash = TRUE)
  
# Univariate analysis -------
  
  insert_msg('Univariate analysis')
  
  ## This is a simple comparison of explanatory variables
  ## between observations with and without particular LFT findings

  c('./LFT scripts/univariable_LFT_findings.R', 
    './LFT scripts/univariable_DLCO_findings.R', 
    './LFT scripts/univariable_DLCO.R', 
    './LFT scripts/univariable_FVC_findings.R', 
    './LFT scripts/univariable_FVC.R',
    './LFT scripts/univariable_FEV1_findings.R', 
    './LFT scripts/univariable_FEV1.R', 
    './LFT scripts/bootstrap_tests.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Tuning of the machine learining models --------
  
  insert_msg('Tuning of the machine learning models')
  
  ## for binary outcomes
  
  list(script_path = c('./LFT scripts/LFT_findings_tuning.R', 
                       './LFT scripts/DLCO_findings_tuning.R', 
                       './LFT scripts/FVC_findings_tuning.R', 
                       './LFT scripts/FEV1_findings_tuning.R'), 
       cache_path = c('./cache/lft_tune.RData', 
                      './cache/dlco_red_tune.RData', 
                      './cache/fvc_red_tune.RData', 
                      './cache/fev1_red_tune.RData'), 
       message = c('Loading cached tuning results for LFT findings', 
                   'Loading cached tuning results for DLCO findings', 
                   'Loading cached tuning results for FVC findings', 
                   'Loading cached tuning results for FEV1 findings')) %>% 
    pwalk(soucer::access_cache)
  
  ## for regression
  
  list(script_path = c('./LFT scripts/DLCO_tuning.R', 
                       './LFT scripts/FVC_tuning.R', 
                       './LFT scripts/FEV1_tuning.R'), 
       cache_path = c('./cache/dlco_tune.RData', 
                      './cache/fvc_tune.RData', 
                      './cache/fev1_tune.RData'), 
       message = c('Loading cached tuning results for DLCO percentage',
                   'Loading cached tuning results for FVC percentage', 
                   'Loading cached tuning results for FEV1 percentage')) %>% 
    pwalk(soucer::access_cache)
  
# Assessment of the model performance ------
  
  insert_msg('Assessment of the model performance')
  
  ## assessment of predictions in the training and CV data
  
  c('./LFT scripts/binary_performance.R', 
    './LFT scripts/regression_performance.R', 
    './LFT scripts/residuals.R', 
    './LFT scripts/ct_predictions.R', 
    './LFT scripts/dlco_classification.R') %>% 
    source_all(message = TRUE, crash = TRUE)

  ## assessment of over-fitting by learning curves
  
 access_cache(cache_path = './cache/lft_learn.RData',
              script_path = './LFT scripts/learning_curves.R', 
              message = 'Assessment of over-fitting by learning curves')
 
 c('./LFT scripts/learning_plots.R') %>% 
   source_all(message = TRUE, crash = TRUE)
 
# Variable importance ------
  
  insert_msg('Variable importance')
  
  ## importance determined by SHAP and algorithm-specific algorithms 
  ## by caret.
  ## For the SHAP measures, we're analyzing the co-linearity by PCA
  ## and investigation of correlation graphs
  ##
  ## Co-regulation of the top explanatory variables: 
  ## PCA and correlation graphs

  access_cache(cache_path = './cache/shap_devel.RData', 
               script_path = './LFT scripts/shap_development.R', 
               message = 'Loading cached SHAP importance data')
  
  c('./LFT scripts/shap_importance.R', 
    './LFT scripts/shap_pca.R', 
    './LFT scripts/caret_importance.R', 
    './LFT scripts/variable_pca.R', 
    './LFT scripts/variable_networks.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# ROC analysis for the CT parameters -------
  
  insert_msg('ROC for CT parameters')
  
  c('./LFT scripts/roc_ct.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END -----
  
  insert_tail()