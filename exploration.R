# Exploratory data analysis: characteristic of the study variables
# and the study cohort

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  
  library(exda)
  library(rstatix)
  library(rcompanion)
  library(fastTest)

  library(clustTools)
  library(ggvenn)

  library(soucer)
  library(furrr)

  library(ggrepel)

  explore <- exda::explore
  
  c('./tools/globals.R', 
    './tools/tools.R') %>%
    source_all(message = TRUE, crash = TRUE)
  
# analysis scripts -----

  insert_msg('Analysis scripts')
  
  c('./exploration scripts/baseline_cohort.R', 
    './exploration scripts/ct.R',
    './exploration scripts/lft.R', 
    './exploration scripts/symptoms.R', 
    './exploration scripts/distribution_longitudinal.R', 
    './exploration scripts/distribution_baseline.R', 
    './exploration scripts/correspondence.R', 
    './exploration scripts/correlation.R', 
    './exploration scripts/comparison.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()