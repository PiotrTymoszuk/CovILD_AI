# Analysis report

# tools -----

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)

  library(flextable)
  library(figur)
  library(cowplot)
  library(writexl)

  library(rmarkdown)
  library(bookdown)
  library(knitr)
  library(rmdformats)

  library(soucer)

  insert_head()

  width <- flextable::width

  c('./tools/globals.R', 
    './tools/tools.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# report scripts -------
  
  insert_msg('Report scripts')
  
  c('./report scripts/links.R', 
    './report scripts/html.R', 
    './report scripts/tables.R', 
    './report scripts/figures.R', 
    './report scripts/render.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()