# Predictions of insufficient DLCO and DLCO by the machine learning models 
# as a function of CTSS, opacity, and high opacity. 
# Out-of-fold predictions are evaluated.

  insert_head()
  
# container ------
  
  dlco_ct <- list()
  
# analysis data: predictions ------
  
  insert_msg('Analysis data')
  
  ## CT variables
  
  dlco_ct$ct_data <- lft_globals$analysis_tbl %>% 
    select(CTSS, opacity_percent, high_opacity_percent)
  
  ## out-of-fold predictions, classification and mean absolute error
  ## collapsing by the observations
  
  dlco_ct$data[c('DLCO_reduced', 
                 'DLCO_percent')] <- 
    list(bin_models$predictions$cv$DLCO_reduced, 
         reg_models$predictions$cv$DLCO_percent) %>% 
    map(map, model.frame)
  
  dlco_ct$data$DLCO_reduced <- dlco_ct$data$DLCO_reduced %>% 
    map(mutate, error = as.numeric(.outcome != .fitted))
  
  dlco_ct$data$DLCO_percent <- dlco_ct$data$DLCO_percent %>% 
    map(mutate, error = abs(.outcome - .fitted))
  
  dlco_ct$data <- dlco_ct$data %>% 
    map(map, select, .observation, error) %>% 
    map(map, group_by, .observation) %>% 
    map(map, summarise, error = mean(error))
  
  ## Brier's squares
  
  dlco_ct$squares <- bin_models$predictions$cv$DLCO_reduced %>% 
    map(squared) %>% 
    map(group_by, .observation) %>% 
    map(summarise, square_dist = mean(square_dist))
  
  dlco_ct$data$DLCO_reduced <- 
    map2(dlco_ct$data$DLCO_reduced, 
         dlco_ct$squares, 
         left_join, by = '.observation')
  
  ## merging with the CT readouts
  
  dlco_ct$data <- dlco_ct$data %>% 
    map(map, cbind, dlco_ct$ct_data) %>% 
    map(map, as_tibble)
  
  dlco_ct <- dlco_ct[c("data")]
  
# Plotting variables -------
  
  insert_msg('Plotting variables')
  
  dlco_ct$ct_variables <- c('CTSS', 'opacity_percent', 'high_opacity_percent')
  
# Global errors and Brier scores to be shown in the plots ------
  
  insert_msg('Global stats')
  
  dlco_ct$global_stats <- 
    list(class_error = bin_models$stats$DLCO_reduced, 
         brier_score = bin_models$stats$DLCO_reduced, 
         mae = reg_models$stats$DLCO_percent)
  
  dlco_ct$global_stats$class_error <- 
    dlco_ct$global_stats$class_error %>% 
    mutate(class_error = 1 - correct_rate)
  
  dlco_ct$global_stats <- dlco_ct$global_stats %>% 
    map(filter, dataset == 'cv') %>% 
    list(x = ., 
         y = c('class_error', 'brier_score', 'MAE'), 
         z = c('global error', 
               'global BS', 
               'global MAE')) %>%
    pmap(function(x, y, z) x[[y]] %>% 
           signif(2) %>% 
           paste(z, ., sep = ' = ') %>% 
           set_names(x$algorithm))

# Plots of errors and Brier's squares ------
  
  insert_msg('Plots of errors and Brier squares')
  
  for(i in names(dlco_ct$data$DLCO_reduced)) {
    
    ## classification errors
    
    dlco_ct$plots$class_errors[[i]] <- 
      list(x_var = dlco_ct$ct_variables, 
           plot_title = dlco_ct$ct_variables %>% 
             exchange(covild$ct_lexicon) %>% 
             paste(., globals$algo_labs[i], sep = ', ') %>% 
             capitalize_first_char, 
           point_wjitter = c(0.05, 0.01, 0.01)) %>% 
      pmap(plot_error_scatters, 
           data = dlco_ct$data$DLCO_reduced[[i]], 
           y_var = 'error', 
           y_lab = 'mean classificatiion error', 
           plot_subtitle = dlco_ct$global_stats$class_error[[i]], 
           point_hjitter = 0.05, 
           point_alpha = 0.5, 
           point_color = globals$algo_colors[[i]]) %>% 
      map2(., c('identity', 'sqrt', 'sqrt'), 
           ~.x + scale_x_continuous(trans = .y)) %>% 
      set_names(dlco_ct$ct_variables)
    
    ## Brier squares
    
    dlco_ct$plots$squares[[i]] <- 
      list(x_var = dlco_ct$ct_variables, 
           plot_title = dlco_ct$ct_variables %>% 
             exchange(covild$ct_lexicon) %>% 
             paste(., globals$algo_labs[i], sep = ', ') %>% 
             capitalize_first_char, 
           point_wjitter = c(0.05, 0.01, 0.01)) %>% 
      pmap(plot_error_scatters, 
           data = dlco_ct$data$DLCO_reduced[[i]], 
           y_var = 'square_dist', 
           y_lab = 'square distance to prediction', 
           plot_subtitle = dlco_ct$global_stats$brier_score[[i]], 
           point_hjitter = 0.05, 
           point_alpha = 0.5, 
           point_color = globals$algo_colors[[i]]) %>% 
      map2(., c('identity', 'sqrt', 'sqrt'), 
           ~.x + scale_x_continuous(trans = .y)) %>% 
      set_names(dlco_ct$ct_variables)
    
    ## mean absolute errors
    
    dlco_ct$plots$mae[[i]] <- 
      list(x_var = dlco_ct$ct_variables, 
           plot_title = dlco_ct$ct_variables %>% 
             exchange(covild$ct_lexicon) %>% 
             paste(., globals$algo_labs[i], sep = ', ') %>% 
             capitalize_first_char, 
           point_wjitter = c(0.05, 0.01, 0.01)) %>% 
      pmap(plot_error_scatters, 
           data = dlco_ct$data$DLCO_percent[[i]], 
           y_var = 'error', 
           y_lab = 'absolute error', 
           plot_subtitle = dlco_ct$global_stats$mae[[i]], 
           point_hjitter = 0.05, 
           point_alpha = 0.5, 
           point_color = globals$algo_colors[[i]]) %>% 
      map2(., c('identity', 'sqrt', 'sqrt'), 
           ~.x + 
             scale_x_continuous(trans = .y) + 
             scale_y_continuous(trans = 'sqrt')) %>% 
      set_names(dlco_ct$ct_variables)

  }

# END -----
  
  rm(i)
  
  insert_tail()