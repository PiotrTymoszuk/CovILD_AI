# This script contains project globals

# data container ------

  globals <- list()

# graphics -----
  
  globals$common_text <- element_text(size = 8,
                                      face = 'plain',
                                      color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')
  
  globals$common_theme <- theme_classic() + 
    theme(axis.text = globals$common_text,
          axis.title = globals$common_text,
          plot.title = element_text(size = 8,
                                    face = 'bold'),
          plot.subtitle = globals$common_text,
          plot.tag = element_text(size = 8,
                                  face = 'plain',
                                  color = 'black',
                                  hjust = 0,
                                  vjust = 1),
          plot.tag.position = 'bottom',
          legend.text = globals$common_text,
          legend.title = globals$common_text,
          strip.text = globals$common_text,
          strip.background = element_rect(fill = 'gray95',
                                          color = 'gray80'),
          plot.margin = globals$common_margin,
          panel.grid.major = element_line(color = 'gray90'))
  
# a theme for presentation figures --------
  
  globals$presentation_text <- element_text(size = 12,
                                            face = 'plain',
                                            color = 'black')
  
  globals$presentation_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')
  
  globals$presentation_theme <- theme_classic() + 
    theme(axis.text = globals$presentation_text,
          axis.title = globals$presentation_text,
          plot.title = element_text(size = 12,
                                    face = 'bold'),
          plot.subtitle = globals$presentation_text,
          plot.tag = element_text(size = 12,
                                  face = 'plain',
                                  color = 'black',
                                  hjust = 0,
                                  vjust = 1),
          plot.tag.position = 'bottom',
          legend.text = globals$presentation_text,
          legend.title = globals$presentation_text,
          strip.text = globals$presentation_text,
          strip.background = element_rect(fill = 'gray95',
                                          color = 'gray80'),
          plot.margin = globals$common_margin,
          panel.grid.major = element_line(color = 'gray90'))
  
  
# strata colors and labels ------

  globals$sev_colors <- c('ambulatory mild' = 'steelblue', 
                          'hospitalized moderate' = 'darkolivegreen3', 
                          'hospitalized severe' = 'coral3', 
                          'cohort' = 'plum4')
  
  globals$sev_labels <- c('ambulatory mild' = 'ambulatory', 
                          'hospitalized moderate' = 'moderate', 
                          'hospitalized severe' = 'severe', 
                          'cohort' = 'CovILD cohort')
  
# follow-up colors and labels -----
  
  globals$fup_colors = c('acute COVID-19' = 'black', 
                         '2-month follow-up' = 'coral4', 
                         '3-month follow-up' = 'coral3', 
                         '6-month follow-up' = 'steelblue2', 
                         '12-month follow-up' = 'steelblue4', 
                         'cohort' = 'plum4')
  
  globals$fup_labels = c('acute COVID-19' = 'acute CoV', 
                         '2-month follow-up' = '2 mo', 
                         '3-month follow-up' = '3 mo', 
                         '6-month follow-up' = '6 mo', 
                         '12-month follow-up' = '12 mo', 
                         'cohort' = 'CovILD cohort')
  
# Algorithm labels and colors ------
  
  globals$algo_labs <- c(ranger = 'Random Forest', 
                         nnet = 'Neural network', 
                         svmRadial = 'SVM radial', 
                         gbm = 'GBM')
  
  globals$algo_colors <- c(ranger = 'darkolivegreen', 
                           nnet = 'steelblue4', 
                           svmRadial = 'orangered4', 
                           gbm = 'plum4')
  
# Dataset labels and symbols ------
  
  globals$dataset_lab <- c(train = 'training', 
                           cv = 'cross-validation', 
                           outcome = 'observed')
  
  globals$dataset_colors <- c(train = 'steelblue', 
                              cv = 'orangered3', 
                              outcome = 'gray50')
  
# LFT response lexicon -------
  
  globals$lft_lexicon <- 
    c('LFT_findings' = 'LFT findings', 
      'DLCO_reduced' = 'DLCO < 80%', 
      'FVC_reduced' = 'FVC < 80%', 
      'FEV1_reduced' = 'FEV1 < 80%', 
      'DLCO_percent' = 'DLCO', 
      'FVC_percent' = 'FVC', 
      'FEV1_percent' = 'FEV1') %>% 
    compress(names_to = 'variable', 
             values_to = 'label')

# END -----
