# Inter-rater reliability of the AI-detected opacity and high opacity 
# as markers of manually-detected, clinically relevant CT abnormalities
#
# 1) Definition of the optimal cutoff of AI-opacity and AI-high opacity
# by ROC and Youden criterion. The entire cohort and timepoints are used
# for the cutoff search
#
# 2) Computation of the sensitivity, specificity, kappa and accuracy for the 
# optimal cutoffs in the COVID-19 severity strata and for particular timepoints

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(OptimalCutpoints)
  library(plotROC)
  library(caret)
  library(bootStat)

  library(soucer)
  library(furrr)

  insert_head()
  
  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# analysis globals ------
  
  insert_msg('Analysis globals')

  cut_globals <- list()
  
  ## responses
  
  cut_globals$responses <- 
    c('CT_findings', 
      'GGO_finding', 
      'reticulation_finding', 
      'consolidation_finding', 
      'bronchiectasis_finding') 
  
  
  cut_globals$responses <- 
    set_names(cut_globals$responses, 
              cut_globals$responses)
  
  ## their dictionary and colors
  
  cut_globals$lexicon <- covild$ct_lexicon %>% 
    filter(variable %in% cut_globals$responses)
  
  cut_globals$response_colors <- 
    c('CT_findings' = 'coral3', 
      'GGO_finding' = 'firebrick3', 
      'reticulation_finding' = 'coral4', 
      'consolidation_finding' = 'gray40', 
      'bronchiectasis_finding' = 'steelblue4')
    
# analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./cutpoint scripts/cutoff_opacity.R', 
    './cutpoint scripts/cutoff_high.R', 
    './cutpoint scripts/opacity_strata.R', 
    './cutpoint scripts/high_strata.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()