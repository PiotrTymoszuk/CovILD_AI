# Checking performance of Random Forest models for the numeric responses: 
# any DLCO, FVC and FEV1

  insert_head()
  
# container -----
  
  reg_models <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## caret models
  
  reg_models$models <- 
    c(dlco_rf$models, 
      fvc_rf$models, 
      fev1_rf$models) %>% 
    set_names(c('dlco_matched', 
                'dlco_unmatched', 
                'fvc_matched', 
                'fvc_unmatched', 
                'fev1_matched', 
                'fev1_unmatched'))
  
  ## model name lexicon
  
  reg_models$lexicon <- 
    c('dlco_matched' = 'DLCO, matched', 
      'dlco_unmatched' = 'DLCO, unmatched', 
      'fvc_matched' = 'FVC, matched', 
      'fvc_unmatched' = 'FVC, unmatched', 
      'fev1_matched' = 'FEV1, matched', 
      'fev1_unmatched' = 'FEV1, unmatched') %>% 
    compress(names_to = 'variable', 
             values_to = 'label') %>% 
    mutate(label_short = stri_replace(label, regex = ',.*', replacement = ''))
  
# Predictions ------
  
  insert_msg('Predictions')
  
  reg_models$predictions <- reg_models$models %>% 
    future_map(predict.caretx, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~.x[c('train', 'cv')]) %>% 
    transpose
  
# Model residuals: plots -----
  
  insert_msg('Model residuals plots')
  
  ## working with the cache: this is a time-consuming step
  
  if(!file.exists('./cache/reg_resid_plots.RData')) {
    
    reg_models$resid_plots <- reg_models$models %>% 
      future_map(plot.caretx, 
                 type = 'diagnostic', 
                 cust_theme = globals$common_theme, 
                 .options = furrr_options(seed = TRUE)) %>% 
      transpose
    
    ## caching
    
    reg_resid_plots <-  reg_models$resid_plots
    
    save(reg_resid_plots, file = './cache/reg_resid_plots.RData')
    
  } else {
    
    load('./cache/reg_resid_plots.RData')
    
    reg_models$resid_plots <- reg_resid_plots
  
  }
  
# Model square errors, per observation -------
  
  insert_msg('Model square errors, per observation')
  
  ## normalized errors, normalized MSE and normalized RMSE, 
  ### in order to compare results easily
  
  reg_models$square_errors <- reg_models$predictions %>% 
    map(map, reg_model_squares)

# Model stats ------
  
  insert_msg('Model stats')
  
  ## working with the cache
  
  if(!file.exists('./cache/reg_stats.RData')) {
    
    reg_models$stats <- reg_models$predictions %>% 
      map(future_map, summary.predx, 
          .options = furrr_options(seed = TRUE))
    
    reg_stats <- reg_models$stats
    
    save(reg_stats, file = './cache/reg_stats.RData')
    
  } else {
    
    load('./cache/reg_stats.RData')
    
    reg_models$stats <- reg_stats
    
  }
  
  reg_models$stats <- reg_models$stats %>% 
    map(compress, names_to = 'model')
  
  ## appending with the normalized MSE and normalized RMSE
  
  reg_models$stats <- 
    map2(reg_models$stats, 
         reg_models$square_errors %>% 
           map(map, ~.x$stats) %>% 
           map(compress, names_to = 'model'), 
         rbind) %>%
    map(mutate, 
        model_name = exchange(model, reg_models$lexicon), 
        model_type = stri_extract(model, regex = 'matched|unmatched'))
  
  ## removal of the redundant information from the square error list
  
  reg_models$square_errors <- reg_models$square_errors %>% 
    map(map, ~.x$square_errors)

# Plotting of the squared errors ------
  
  insert_msg('Plotting of square errors')
  
  reg_models$sqare_error_plots <- 
    list(train_errors = reg_models$square_errors$train, 
         cv_errors = reg_models$square_errors$cv, 
         plot_title = exchange(names(reg_models$square_errors[[1]]), 
                               reg_models$lexicon)) %>% 
    pmap(plot_bin_error, 
         sort = TRUE, 
         y_lab = 'Square normalized error', 
         point_shape = 16, 
         point_alpha = 0.25, 
         point_size = 1)
  
  ## adding the 2\times SD line
  
  reg_models$sqare_error_plots <- reg_models$sqare_error_plots %>% 
    map(~.x + 
          geom_hline(yintercept = qnorm(0.975)^2, 
                     linetype = 'dashed'))
  
# Plotting the model stats -----
  
  insert_msg('Plotting the model stats')
  
  reg_models$stat_plots <- 
    list(data = reg_models$stats, 
         plot_subtitle = c('Training', 'CV')) %>% 
    pmap(plot_regression_stats, 
         fill_var = 'model_type', 
         point_shape = 21, 
         color = 'black') %>% 
    map(map, 
        ~.x + 
          scale_color_manual(values = c(matched = 'orangered4', 
                                        unmatched = 'darkolivegreen4'), 
                             label = c(matched = 'participant-matched', 
                                       unmatched = 'unmatched'), 
                             name = 'Model training type') + 
          scale_fill_manual(values = c(matched = 'orangered4', 
                                       unmatched = 'darkolivegreen4'), 
                            label = c(matched = 'participant-matched', 
                                      unmatched = 'unmatched'), 
                            name = 'Model training type'))
  
# Captions for calibration plots -------
  
  insert_msg('Captions for calibration plots')
  
  ## with the R-squared and Pearson's r
  
  reg_models$scatter_caps  <- reg_models$stats %>% 
    map(blast, model) %>%
    map(map, make_scatter_caps)
    
# Visualizing model calibration with scatter plots -------
  
  insert_msg('Calibration scatter plots')
  
  reg_models$scatter_plots <- 
    list(x = reg_models$predictions, 
         y = c('training', 'CV'), 
         z = reg_models$scatter_caps,
         v = c('steelblue', 'coral3')) %>% 
    pmap(function(x, y, z, v) list(x = x, 
                                   plot_title = exchange(names(x), 
                                                         reg_models$lexicon) %>% 
                                     paste(y, sep = ', '), 
                                   plot_subtitle = z) %>% 
           pmap(plot, 
                type = 'regression', 
                point_color = v, 
                cust_theme = globals$common_theme, 
                show_trend = FALSE) %>% 
           map(same_scale_calib) %>% 
           map(~.x + 
                 labs(subtitle = paste(.x$labels$subtitle, 
                                       .x$labels$tag, 
                                       sep = ', ')) + 
                 geom_smooth(method = 'gam', 
                             formula = y ~ s(x, bs = "cs")) + 
                 theme(plot.tag = element_blank())))

# Result table ------
  
  insert_msg('Result table')
  
  reg_models$result_tbl <- reg_models$stats %>% 
    map(filter, 
        statistic %in% c('MAE', 'RMSE', 'RMSE_normalized', 
                         'rsq', 'pearson')) %>% 
    compress(names_to = 'data_type') %>% 
    mutate(statistic = car::recode(statistic, 
                                   "'RMSE_normalized' = 'normalized RMSE'; 
                                   'rsq' = 'R\u00B2'; 
                                   'pearson' = 'Pearson r'"), 
           data_type = car::recode(data_type, 
                                   "'train' = 'training'; 'cv' = 'CV'"), 
           data_type = factor(data_type, c('training', 'CV')), 
           estimate = ifelse(data_type == 'training', 
                             signif(estimate, 2), 
                             paste0(signif(estimate, 2), 
                                    ' [95% CI: ', signif(lower_ci, 2), 
                                    ' - ', signif(upper_ci, 2), ']')), 
           model_name = stri_replace(model_name, 
                                     regex = ',.*', 
                                     replacement = ''))
  
  reg_models$result_tbl <- reg_models$result_tbl %>% 
    arrange(model_name, 
            model_type, 
            data_type) %>% 
    select(model_name, 
           model_type, 
           data_type, 
           statistic, 
           estimate) %>% 
    set_names(c('Response', 
                'Participant matching', 
                'Data type', 
                'Statistic name', 
                'Statistic value'))

# END -----
  
  plan('sequential')
  
  rm(reg_resid_plots, reg_stats)
  
  insert_tail()