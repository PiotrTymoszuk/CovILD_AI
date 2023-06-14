# Distribution tests: checking normality (splitting by severity and timepoint) 
# of the longitudinally recorded 
# CT and LFT variables

  insert_head()
  
# container ------
  
  distr_long <- list()
  
# Parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ----
  
  insert_msg('Analysis globals')
  
  ## variable lexicon, selecting only the numeric ones
  
  distr_long$lexicon <- 
    rbind(covild$ct_lexicon, 
          covild$lft_lexicon) %>% 
    filter(!variable %in% c('ID', 'follow_up', 'severity_class')) %>% 
    filter(format == 'numeric') %>% 
    filter(!stri_detect(variable, regex = '(DLCO|volume)$'))
  
  ## analysis table
  
  distr_long$analysis_tbl <- 
    list(covild$ct, 
         covild$lft %>% 
           select(-follow_up), 
         covild$baseline[c('ID', 'severity_class')]) %>% 
    reduce(left_join, by = 'ID') %>% 
    filter(follow_up != 'acute COVID-19') %>% 
    mutate(follow_up = droplevels(follow_up), 
           analysis_split = interaction(follow_up, severity_class)) %>% 
    select(ID, follow_up, severity_class, analysis_split, 
           all_of(distr_long$lexicon$variable))
  
# Shapiro test ------
  
  insert_msg('Shapiro test')
  
  distr_long$test <- distr_long$lexicon$variable %>% 
    future_map(test_long_distribution, 
               data = distr_long$analysis_tbl, 
               split_factor = 'analysis_split', 
               .options = furrr_options(seed = TRUE)) %>% 
    set_names(distr_long$lexicon$variable) %>% 
    compress(names_to = 'variable')
  
# Plotting the global Shapiro stats -------
  
  insert_msg('Plotting the global Shapiro test stats')
  
  distr_long$shapiro_plot <- distr_long$test %>% 
    filter(analysis_split == 'global', 
           best_transf == 'yes') %>% 
    mutate(var_lab = exchange(variable, dict = distr_long$lexicon), 
           var_lab  = paste(var_lab, transformation, sep = ', ')) %>% 
    ggplot(aes(x = statistic, 
               y = reorder(var_lab, statistic))) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             fill = 'steelblue') + 
    geom_vline(xintercept = 0.95, 
               linetype = 'dashed') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Normality testing with Shapiro-Wilk test', 
         subtitle = 'entire longitudinal dataset', 
         x = 'Test statistic, W')
  
# Plotting the mean and variance -----
  
  insert_msg('Variance and mean')
  
  distr_long$mean_var_plot <- distr_long$test %>% 
    filter(analysis_split == 'global') %>% 
    mutate(var_lab = exchange(variable, dict = distr_long$lexicon)) %>% 
    blast(transformation) %>% 
    map2(., 
         paste('Mean and variance,', names(.)), 
         ~ggplot(.x, 
                 aes(x = mean, 
                     y = variance, 
                     fill = transformation)) + 
           geom_abline(slope = 1, 
                       intercept = 0, 
                       linetype = 'dashed') + 
           geom_point(shape = 21, 
                      size = 2, 
                      alpha = 0.75) + 
           geom_text_repel(aes(label = var_lab), 
                           size = 2.75) + 
           scale_fill_manual(values = c('steelblue', 
                                        'darkolivegreen4', 
                                        'coral3')) + 
           scale_x_continuous(trans = 'log',
                              labels = function(x) signif(x, 2)) + 
           scale_y_continuous(trans = 'log', 
                              labels = function(x) signif(x, 2)) + 
           guides(fill = 'none') + 
           globals$common_theme + 
           labs(title = .y, 
                subtitle = 'Entire longitudinal dataset', 
                x = 'Mean', 
                y = 'Variance'))

# END ------
  
  plan('sequential')
  
  insert_tail()