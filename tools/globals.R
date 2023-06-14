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

# END -----
