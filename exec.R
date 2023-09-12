# Launches the entire analysis pipeline

  library(soucer)

  print(source_all(c('import.R', 
                     'exploration.R', 
                     'kinetic.R', 
                     'cutpoint.R', 
                     'ctss.R', 
                     'lft.R', 
                     'report.R'), 
                   message = TRUE, crash = TRUE))
