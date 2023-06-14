# Checking performance of Random Forest models for the binary responses: 
# any CT findings, reduced DLCO, reduced FVC and reduced FEV1

  insert_head()
  
# container ------
  
  bin_models <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## caret models
  
  bin_models$models <- 
    c(lft_rf$models, 
      dlco_red_rf$models, 
      fvc_red_rf$models, 
      fev1_red_rf$models) %>% 
    set_names(c('lft_matched', 
                'lft_unmatched', 
                'dlco_matched', 
                'dlco_unmatched', 
                'fvc_matched', 
                'fvc_unmatched', 
                'fev1_matched', 
                'fev1_unmatched'))
  
  ## model name lexicon
  
  bin_models$lexicon <- 
    c('lft_matched' = 'LFT findings, matched', 
      'lft_unmatched' = 'LFT findings, unmatched', 
      'dlco_matched' = 'DLCO < 80%, matched', 
      'dlco_unmatched' = 'DLCO < 80%, unmatched', 
      'fvc_matched' = 'FVC < 80%, matched', 
      'fvc_unmatched' = 'FVC < 80%, unmatched', 
      'fev1_matched' = 'FEV1 < 80%, matched', 
      'fev1_unmatched' = 'FEV1 < 80%, unmatched') %>% 
    compress(names_to = 'variable', 
             values_to = 'label') %>% 
    mutate(label_short = stri_replace(label, regex = ',.*', replacement = ''))
  
# Predictions ------
  
  insert_msg('Predictions')
  
  bin_models$predictions <- bin_models$models %>% 
    future_map(predict.caretx, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~.x[c('train', 'cv')]) %>% 
    transpose
  
# Model residuals: plots -----
  
  insert_msg('Model residuals plots')
  
  bin_models$resid_plots <- bin_models$models %>% 
    future_map(plot.caretx, 
               type = 'diagnostic', 
               cust_theme = globals$common_theme, 
               .options = furrr_options(seed = TRUE)) %>% 
    transpose
  
# Model square errors ------
  
  insert_msg('Model square errors and Brier scores')
  
  bin_models$square_errors <- bin_models$predictions %>% 
    map(map, bin_model_squares)
  
# Model stats ------
  
  insert_msg('Model stats')
  
  bin_models$stats <- bin_models$predictions %>% 
    map(future_map, summary.predx, 
        .options = furrr_options(seed = TRUE)) %>% 
    map(compress, names_to = 'model')
  
  ## appending with Brier scores
  
  bin_models$stats <- 
    map2(bin_models$stats, 
         bin_models$square_errors %>% 
           map(map, ~.x$stats) %>% 
           map(compress, names_to = 'model'), 
         rbind) %>%
    map(mutate, 
        model_name = exchange(model, bin_models$lexicon), 
        model_type = stri_extract(model, regex = 'matched|unmatched'))
  
  ## removal of the redundant information from the square error list

  bin_models$square_errors <- bin_models$square_errors %>% 
    map(map, ~.x$square_errors)
  
# Plotting of the squared errors ------
  
  insert_msg('Plotting of square errors')
  
  bin_models$sqare_error_plots <- 
    list(train_errors = bin_models$square_errors$train, 
         cv_errors = bin_models$square_errors$cv, 
         plot_title = exchange(names(bin_models$square_errors[[1]]), 
                               bin_models$lexicon)) %>% 
    pmap(plot_bin_error, 
         sort = TRUE, 
         point_shape = 16, 
         point_alpha = 0.25, 
         point_size = 1)
  
  ## adding the 'fence-sitter' line
  
  bin_models$sqare_error_plots <- bin_models$sqare_error_plots %>% 
    map(~.x + 
          geom_hline(yintercept = 0.25, 
                     linetype = 'dashed'))
  
# Plotting model stats: training and CV ------
  
  insert_msg('Plotting the model stats')
  
  bin_models$stat_plots <- 
    list(data = bin_models$stats, 
         plot_subtitle = c('Training', 'CV')) %>% 
    pmap(plot_binary_stats, 
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
  
# ROC plots ------
  
  insert_msg('ROC plots')
  
  bin_models$roc_plots <- 
    list(x = bin_models$predictions, 
         y = c('training', 'CV'), 
         z = c('steelblue', 'coral3')) %>% 
    pmap(function(x, y, z) list(x = x, 
                                plot_title = names(x) %>% 
                                  exchange(bin_models$lexicon) %>% 
                                  paste(y, sep = ', ')) %>% 
           future_pmap(plot.predx, 
                       type = 'roc', 
                       cust_theme = globals$common_theme, 
                       line_color = z, 
                       .options = furrr_options(seed = TRUE)))
  
  ## extra styling
  
  bin_models$roc_plots <- bin_models$roc_plots %>% 
    map(function(x) x %>% 
          map(~.x + 
                labs(subtitle = .x$labels$subtitle %>% 
                       stri_replace(fixed = 'Rsq', replacement = 'R\u00B2') %>% 
                       paste(.x$labels$tag, sep = ', ')) + 
                theme(plot.tag = element_blank())))
  
# Confusion matrices ------
  
  insert_msg('Confusion matrices')
  
  bin_models$confusion_plots <- 
    list(x = bin_models$predictions, 
         y = c('training', 'CV')) %>% 
    pmap(function(x, y) list(x = x, 
                             plot_title = names(x) %>% 
                               exchange(bin_models$lexicon) %>% 
                               paste(y, sep = ', '), 
                             plot_subtitle = paste('% of total, n =', 
                                                   map_dbl(x, ~nrow(.x$data))), 
                             x_lab = names(x) %>% 
                               exchange(bin_models$lexicon, 
                                        value = 'label_short') %>% 
                               paste0(', outcome'), 
                             y_lab = names(x) %>% 
                               exchange(bin_models$lexicon, 
                                        value = 'label_short') %>% 
                               paste0(', predicted')) %>% 
           future_pmap(plot.predx, 
                       type = 'confusion', 
                       scale = 'percent', 
                       cust_theme = globals$common_theme, 
                       .options = furrr_options(seed = TRUE)) %>% 
           map(~.x + 
                 scale_fill_gradient(low = 'white', 
                                     high = 'firebrick', 
                                     limits = c(0, 100)) + 
                 scale_x_discrete(labels = c(no = 'absent', yes = 'present')) + 
                 scale_y_discrete(labels = c(no = 'absent', yes = 'present')) + 
                 theme(plot.tag = element_blank())))
  
# Table with model stats ------
  
  insert_msg('Result table')
  
  bin_models$result_tbl <- bin_models$stats %>% 
    map(filter, 
        statistic %in% c('class_error', 'correct_rate', 
                         'kappa', 'Se', 'Sp', 
                         'AUC', 'c_index', 'BS', 'rsq')) %>% 
    compress(names_to = 'data_type') %>% 
    mutate(statistic = car::recode(statistic, 
                                   "'class_error' = 'classification error'; 
                                    'correct_rate' = 'accuracy'; 
                                    'kappa' = '\u03BA'; 
                                    'Se' = 'sensitivity'; 
                                    'Sp' = 'specificity'; 
                                    'AUC' = 'AUC, ROC'; 
                                    'c_index' = 'C-index'; 
                                    'BS' = 'Brier score'; 
                                   'rsq' = 'R\u00B2'"), 
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
  
  bin_models$result_tbl <- bin_models$result_tbl %>% 
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
  
  insert_tail()