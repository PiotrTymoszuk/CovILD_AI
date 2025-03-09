# Parts of the manuscript

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  
  library(flextable)
  library(figur)
  library(cowplot)
  
  library(caretExtra)
  library(ggtext)
  
  library(rmarkdown)
  library(bookdown)
  library(knitr)
  library(writexl)

  library(soucer)
  
  insert_head()
  
  width <- flextable::width
  nobs <- stats::nobs
  
  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# Scripts generating paper parts -------
  
  insert_msg('Scripts for paper parts')
  
  c('./paper scripts/tables.R', 
    './paper scripts/supplementary_tables.R', 
    './paper scripts/figures.R', 
    './paper scripts/supplementary_figures.R', 
    './paper scripts/reviewer_figures.R', 
    './paper scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()
