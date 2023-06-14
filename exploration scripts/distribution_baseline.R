# Distribution of the numeric baseline variables

  insert_head()
  
# container ------
  
  distr_base <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## numeric variables
  
  distr_base$lexicon <- covild$baseline_lexicon %>% 
    filter(format == 'numeric', 
           !variable %in% c('ID', 'follow_up'))
  
  distr_base$analysis_tbl <- covild$baseline %>% 
    select(all_of(distr_base$lexicon$variable)) %>% 
    mutate(weight_change_kg = -min(weight_change_kg) + weight_change_kg)
  
# Shapiro-Wilk test, mean and normality -----
  
  insert_msg('Shairo-Wilk test')
  
  distr_base$test <- distr_base$lexicon$variable %>% 
    map(test_long_distribution, 
        data = distr_base$analysis_tbl, 
        split_factor = NULL) %>% 
    set_names(distr_base$lexicon$variable) %>% 
    compress(names_to = 'variable')
  
# Plotting the Shapiro-Wilk testing results ------
  
  insert_msg('Shapiro-Wilk testing results')
  
  distr_base$shapiro_plot <- distr_base$test %>% 
    filter(best_transf == 'yes') %>% 
    mutate(var_lab = exchange(variable, dict = distr_base$lexicon), 
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
         subtitle = 'entire baseline dataset', 
         x = 'Test statistic, W')
  
# Plotting the means and variances -----
  
  insert_msg('Plotting the means and variances')
  
  distr_base$mean_var_plot <- distr_base$test %>% 
    mutate(var_lab = exchange(variable, dict = distr_base$lexicon)) %>% 
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
                subtitle = 'Entire baseline dataset', 
                x = 'Mean', 
                y = 'Variance'))
  
# END ------
  
  insert_tail()