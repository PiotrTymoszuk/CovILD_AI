# Frequency of ILD in the entire data set, at the follow-ups 
# and COVID-19 severity classes

  insert_head()
  
# container ------
  
  ild_freq <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## analysis table
  
  ild_freq$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    select(ID, severity_class, follow_up, opacity_class) %>% 
    mutate(analysis_split = interaction(follow_up, severity_class))

# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  ild_freq$stats <- ild_freq$analysis_tbl %>% 
    explore(split_factor = 'analysis_split', 
            variables = 'opacity_class', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    compress(names_to = 'analysis_split') %>% 
    mutate(follow_up = stri_split_fixed(analysis_split, pattern = '.', simplify = TRUE)[, 1], 
           severity_class = stri_split_fixed(analysis_split, pattern = '.', simplify = TRUE)[, 2])
  
# Plotting ------
  
  insert_msg('Plotting')
  
  ild_freq$plot <- 
    plot_explo_stack(data = ild_freq$analysis_tbl, 
                     outcome = 'opacity_class', 
                     time_variable = 'follow_up', 
                     severity_variable = 'severity_class', 
                     plot_title = 'ILD frequency', 
                     plot_subtitle = 'ILD: opacity > 5% of the lungs', 
                     y_lab = '% of FUP strata', 
                     x_lab = 'Follow-up, months after COVID-19', 
                     txt_size = 2.5, 
                     txt_form = 'text', 
                     txt_color = 'white', 
                     show_n = TRUE) + 
    scale_fill_manual(values = c('steelblue', 'coral3')) + 
    scale_x_discrete(labels = cohort_sympt$short_fup_labs)

# END -----

  insert_tail()