# Concordance of CTSS, AI-determined opacity and high opacity
#
# A general problem with the CTSS and opacity relationship: 
# integration of these two variables.
# An extreme case: let's assume, one lap of the lung is completely destroyed, 
# this gives CTSS = 5, and opacity of approx. 20%. 
# Another extreme case: single subtle GGOs in each of the laps, 
# this gives CTSS = 5, and opacity of approx. 5%.
# So those two parameters measure clearly different outcomes. 
# Integration of CTSS by sum over all 5 lung laps may lead to problems, 
# since it does not discriminate between locally severe alteratons and globally
# mild abnormalities!

# tools -------

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  
  library(caret)
  library(exda)
  library(rstatix)
  library(bootStat)

  library(plotROC)
  library(OptimalCutpoints)
  
  library(soucer)
  library(furrr)
  library(MASS)
  
  library(lmqc)

  insert_head()
  
  select <- dplyr::select
  
  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ctss_globals <- list()
  
  ## variables
  
  ctss_globals$variables <- 
    c('CTSS', 'CTSS_class', 
      'opacity_percent', 
      'high_opacity_percent', 
      'opacity_class')
  
  ctss_globals$lexicon <- covild$ct_lexicon %>% 
    filter(variable %in% ctss_globals$variables)
  
  ## analysis tables
  
  ctss_globals$analysis_tbl <- 
    left_join(covild$ct,
              covild$baseline[c('ID', 'severity_class')], 
              by = 'ID') %>% 
    select(ID, severity_class, follow_up, any_of(ctss_globals$variables))

  ctss_globals$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    mutate(severity_class = 'cohort') %>%
    rbind(ctss_globals$analysis_tbl) %>% 
    mutate(severity_class = factor(severity_class, 
                                   c('cohort', 
                                     levels(ctss_globals$analysis_tbl$severity_class)))) %>% 
    filter(follow_up != 'acute COVID-19') %>% 
    filter(!is.na(severity_class)) %>% 
    mutate(follow_up = droplevels(follow_up), 
           CTSS_class = cut(CTSS, 
                            c(-Inf, 0, 4, 9, Inf), 
                            c('0', '1-4', '5-9', '10+')),
           opacity_class = cut(opacity_percent, 
                               c(-Inf, 5, Inf), 
                               c('0-5%', '>5%')))
  
# analysis scripts -------
  
  insert_msg('Analysis scripts')
  
  c('./CTSS scripts/ild_frequency.R', 
    './CTSS scripts/ild_cutoff.R', 
    './CTSS scripts/ild_strata.R', 
    './CTSS scripts/ctss_classes.R', 
    './CTSS scripts/ctss_relationship.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()