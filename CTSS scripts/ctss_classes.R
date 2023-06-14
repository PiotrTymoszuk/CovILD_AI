# Opacity and high opacity in the CTSS severity classes

  insert_head()
  
# container ------
  
  ctss_classes <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  ctss_classes$variables <- c('opacity_percent', 'high_opacity_percent')
  
  ctss_classes$variables <- 
    set_names(ctss_classes$variables, 
              ctss_classes$variables)
  
  ## analysis table
  
  ctss_classes$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    filter(severity_class == 'cohort') %>% 
    select(ID, CTSS_class, all_of(ctss_classes$variables)) %>%
    filter(complete.cases(.))

# descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  ## n numbers 
  
  ctss_classes$n_numbers <- ctss_classes$analysis_tbl %>% 
    count(CTSS_class)
  
  ## normality: clearly non-normal distribution
  
  ctss_classes$normality <- ctss_classes$analysis_tbl %>% 
    explore(split_factor = 'CTSS_class', 
            variables = ctss_classes$variables, 
            what = 'normality', 
            pub_styled = TRUE)
  
  ## descriptive stats
  
  ctss_classes$stats <- ctss_classes$analysis_tbl %>% 
    explore(split_factor = 'CTSS_class', 
            variables = ctss_classes$variables, 
            what = 'table', 
            pub_styled = TRUE)
  
# Testing for trend with Kruskal-Wallis test -----
  
  insert_msg('Testing')
  
  ctss_classes$test <- ctss_classes$analysis_tbl %>% 
    compare_variables(variables = ctss_classes$variables, 
                      split_factor = 'CTSS_class', 
                      what = 'eff_size', 
                      types = 'kruskal_etasq', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH') %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plots -------
  
  insert_msg('Plots')
  
  ctss_classes$plots <-
    list(variable = ctss_classes$variables, 
         plot_title = exchange(ctss_classes$variables, 
                               ctss_globals$lexicon), 
         plot_subtitle = ctss_classes$test$plot_cap, 
         y_lab = exchange(ctss_classes$variables, 
                          ctss_globals$lexicon, 
                          value = 'unit')) %>% 
    pmap(plot_variable, 
         ctss_classes$analysis_tbl, 
         split_factor = 'CTSS_class', 
         type = 'box', 
         cust_theme = globals$common_theme,
         x_lab = 'CTSS', 
         x_n_labs = TRUE, 
         point_hjitter = 0) %>% 
    map(~.x + 
          theme(legend.position = 'none') + 
          scale_fill_brewer(palette = 'Reds'))
  
# END ------
  
  insert_tail()