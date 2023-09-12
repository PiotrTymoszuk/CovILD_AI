# Analysis of kinetic of CT and LFT parameters, done in participants with 
# complete longitudinal observation sets.
#
# The analysis is done for the entire cohort and the severity subsets
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
  ## 'crazy_paving_finding', and volume LFT parameters are excluded
  
  kin_globals$lexicon <- 
    covild[c("ct_lexicon", "lft_lexicon")] %>% 
    reduce(rbind) %>%
    filter(!variable %in% c('crazy_paving_finding', 'DLCO')) %>%
    filter(!stri_detect(variable, regex = '_volume$'))
  
  kin_globals$variables <- kin_globals$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)
  
  ## analysis tables for the entire cohort and severity groups

  kin_globals$analysis_tbl <- covild[c("ct", "lft")] %>% 
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
  
# Analysis scripts ------
  
  insert_msg('Analysis scripts')
  
  c('./kinetic scripts/numeric.R', 
    './kinetic scripts/factor.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END -----
  
  insert_tail()