# Analysis of kinetic of CT and LFT parameters, and symptoms.
#
# The analysis is done for the entire cohort and the severity subsets
# Only participants with the complete one-year follow-up are included in 
# the analysis
# Statistical tools are Cochran Q test (binary responses) and Friedman test

# tools ------

  library(tidyverse)
  library(rlang)
  library(trafo)
  library(stringi)
  
  library(exda)
  library(rstatix)
  library(rcompanion)
  library(effectsize)

  library(soucer)
  library(furrr)
  
  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# analysis globals -------
  
  insert_msg('Analysis globals')
  
  kin_globals <- list()
  
  ## study variables
  ## 'crazy_paving_finding', 'TLC' and 'RV' (imcomplete!), 
  ## and volume LFT parameters are excluded
  
  kin_globals$lexicon <- 
    covild[c("ct_lexicon", "lft_lexicon", "symptoms_lexicon")] %>% 
    reduce(rbind) %>%
    filter(!variable %in% c('crazy_paving_finding', 
                            'DLCO', 
                            'TLC_percent', 
                            'RV_percent')) %>%
    filter(!stri_detect(variable, regex = '_volume$')) %>% 
    mutate(class = ifelse(variable %in% covild$ct_lexicon$variable, 
                          'CT', 
                          ifelse(variable %in% covild$lft_lexicon$variable, 
                                 'LFT', 'symptoms')), 
           class = factor(class, c('CT', 'LFT', 'symptoms')))
  
  kin_globals$variables <- kin_globals$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable) %>% 
    map(~.x[.x != 'follow_up'])
  
  ## analysis tables for the entire cohort and severity groups

  kin_globals$analysis_tbl <- covild[c("ct", "lft", "symptoms")] %>% 
    reduce(left_join, by = c('ID', 'follow_up')) %>% 
    left_join(covild$baseline[c('ID', 'severity_class')], 
              by = 'ID') %>% 
    select(ID, severity_class, follow_up, 
           all_of(kin_globals$lexicon$variable))
  
  kin_globals$analysis_tbl <- 
    kin_globals$analysis_tbl %>% 
    mutate(severity_class = 'cohort') %>%
    rbind(kin_globals$analysis_tbl) %>% 
    mutate(severity_class = factor(severity_class, 
                                   c('cohort', 
                                     levels(kin_globals$analysis_tbl$severity_class)))) %>% 
    filter(follow_up != 'acute COVID-19') %>% 
    filter(!is.na(severity_class)) %>% 
    mutate(follow_up = droplevels(follow_up))
  
  kin_globals$analysis_tbl <- kin_globals$analysis_tbl %>% 
    blast(severity_class) %>% 
    map(select, -severity_class)
  
  ## restricting the analysis tables to the participants with the complete
  ## one-year follow-up
  
  kin_globals$analysis_tbl <- kin_globals$analysis_tbl %>% 
    map(complete_longitudinal)
  
# Analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./kinetic scripts/numeric.R', 
    './kinetic scripts/factor.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()