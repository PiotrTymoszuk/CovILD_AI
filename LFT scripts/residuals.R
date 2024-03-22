# Analysis of fit (residuals and Brier's squares) of the machine learning models
# in the severity strata and at the study visits. Included aare only successful
# models

  insert_head()
  
# container -------
  
  dlco_resid <- list()
  
# analysis data -------
  
  insert_msg('Analysis data')
  
  ## Brier's squares for the classifiers
  
  dlco_resid$data$DLCO_reduced <- 
    bin_models$predictions$cv$DLCO_reduced %>% 
    map(squared) %>% 
    map(transmute, 
        .observation = .observation, 
        error = square_dist)
  
  ## error rate for the classifiers
  
  dlco_resid$data$DLCO_accuracy <- 
    bin_models$predictions$cv$DLCO_reduced %>% 
    map(model.frame) %>% 
    map(transmute, 
        .observation = .observation, 
        error = as.numeric(.outcome != .fitted))

  ## absolute residuals for the regression models
  
  dlco_resid$data$DLCO_percent <- 
    reg_models$predictions$cv$DLCO_percent %>% 
    map(residuals) %>% 
    map(transmute, 
        .observation = .observation, 
        error = abs(.resid))
  
  ## residuals for the regression models
  
  dlco_resid$data$DLCO_residuals <- 
    reg_models$predictions$cv$DLCO_percent %>% 
    map(residuals) %>% 
    map(transmute, 
        .observation = .observation, 
        error = .resid)
  
  ## collapsing of the resamples
  
  dlco_resid$data <- dlco_resid$data %>% 
    map(map, 
        group_by, 
        .observation) %>% 
    map(map, 
        summarise, 
        error = mean(error, na.rm = TRUE))
  
  ## appending with the visit and severity information
  
  dlco_resid$assignment <- lft_globals$analysis_tbl %>% 
    transmute(.observation = 1:nrow(.), 
              follow_up = follow_up, 
              severity_class = severity_class) %>% 
    as_tibble
  
  dlco_resid$data <- dlco_resid$data %>% 
    map(map, 
        ~left_join(dlco_resid$assignment, 
                   .x, 
                   by = '.observation'))
  
# Statistics for the strata --------
  
  insert_msg('Strata statistics')
  
  ## distribution stats
  
  dlco_resid$stats <- dlco_resid$data %>% 
    map(map,
        group_by, 
        follow_up, 
        severity_class) %>% 
    map(map, 
        summarise, 
        mean = mean(error, na.rm = TRUE), 
        sd = sd(error, na.rm = TRUE), 
        median = median(error, na.rm = TRUE), 
        q025 = quantile(error, 0.025, na.rm = TRUE), 
        q975 = quantile(error, 0.975, na.rm = TRUE), 
        min = min(error, na.rm = TRUE), 
        max = max(error, na.rm = TRUE)) %>% 
    map(map, ungroup)
  
  ## n numbers
  
  dlco_resid$n_numbers <- dlco_resid$data %>% 
    map(map, 
        count, 
        follow_up, 
        severity_class)
  
  for(i in names(dlco_resid$stats)) {
    
    dlco_resid$stats[[i]] <- 
      map2(dlco_resid$stats[[i]],
           dlco_resid$n_numbers[[i]], 
           left_join, 
           by = c('follow_up', 'severity_class'))
    
  }
  
# Cohen's kappa for the strata defined by Cov severity and FUP -------
  
  insert_msg('Cohen kappa')

  dlco_resid$stats$kappa <- 
    bin_models$predictions$cv$DLCO_reduced %>% 
    map(model.frame) %>% 
    map(~left_join(.x, 
                   mutate(lft_globals$analysis_tbl, 
                          .observation = 1:nrow(lft_globals$analysis_tbl)), 
                   by = '.observation')) %>% 
    map(select, .outcome, .fitted, severity_class, follow_up) %>% 
    map(blast, severity_class, follow_up, .skip = TRUE) %>% 
    map(map, set_names, c('obs', 'pred')) %>% 
    map(map, defaultSummary, lev = c('no', 'yes')) %>% 
    map(map_dbl, ~.x[['Kappa']])  %>% 
    map(compress, 
        names_to = 'severity_class', 
        values_to = 'kappa')
  
  dlco_resid$stats$kappa <- dlco_resid$stats$kappa %>% 
    map(mutate, 
        kappa = ifelse(kappa < 0, 0, kappa), 
        follow_up = stri_split_fixed(severity_class, 
                                     pattern = '.', 
                                     simplify = TRUE)[, 2], 
        severity_class = stri_split_fixed(severity_class, 
                                          pattern = '.', 
                                          simplify = TRUE)[, 1], 
        follow_up = factor(follow_up, 
                           levels(dlco_resid$stats$DLCO_reduced[[1]]$follow_up)),
        severity_class = factor(severity_class, 
                                levels(dlco_resid$stats$DLCO_reduced[[1]]$severity_class)))
    
# Global stats to be presented in the plot subtitles -------
  
  insert_msg('Global stats to be shown in the plot subtitles')

  dlco_resid$global_stats <- 
    list(class_error = bin_models$stats$DLCO_reduced, 
         brier_score = bin_models$stats$DLCO_reduced, 
         kappa = bin_models$stats$DLCO_reduced, 
         mae = reg_models$stats$DLCO_percent)
  
  dlco_resid$global_stats$class_error <- 
    dlco_resid$global_stats$class_error %>% 
    mutate(class_error = 1 - correct_rate)
  
  dlco_resid$global_stats <- dlco_resid$global_stats %>% 
    map(filter, dataset == 'cv') %>% 
    list(x = ., 
         y = c('class_error', 'brier_score', 'kappa', 'MAE'), 
         z = c('global error', 
               'global BS', 
               'global \u03BA', 
               'global MAE')) %>%
    pmap(function(x, y, z) x[[y]] %>% 
           signif(2) %>% 
           paste(z, ., sep = ' = '))
  
# Heat maps of the mean normalized squared residuals --------
  
  insert_msg('Heat maps')
  
  ## classifiers: Brier's squares
  
  dlco_resid$hm_plots$DLCO_reduced <- 
    list(data = dlco_resid$stats$DLCO_reduced, 
         plot_title = paste('DLCO < 80%,', 
                            globals$algo_labs[names(dlco_resid$stats$DLCO_reduced)]), 
         plot_subtitle = dlco_resid$global_stats$brier_score) %>% 
    pmap(plot_resid_hm, 
         plot_variable = 'mean', 
         fill_lab = 'mean square distance to prediction', 
         midpoint = 0.125, 
         limits = c(0, 0.25), 
         oob = scales::squish)
  
  ## classifiers: classification errors
  
  dlco_resid$hm_plots$DLCO_accuracy <- 
    list(data = dlco_resid$stats$DLCO_accuracy, 
         plot_title = paste('DLCO < 80%,', 
                            globals$algo_labs[names(dlco_resid$stats$DLCO_accuracy)]), 
         plot_subtitle = dlco_resid$global_stats$class_error) %>% 
    pmap(plot_resid_hm, 
         plot_variable = 'mean', 
         fill_lab = 'classification error', 
         label_range = FALSE, 
         midpoint = 0.15, 
         limits = c(0, 0.3), 
         oob = scales::squish)
  
  ## classifiers: Cohen's kappa
  
  dlco_resid$hm_plots$kappa <- 
    list(data = dlco_resid$stats$kappa, 
         plot_title = paste('DLCO < 80%,', 
                            globals$algo_labs[names(dlco_resid$stats$kappa)]), 
         plot_subtitle = dlco_resid$global_stats$kappa) %>% 
    pmap(plot_resid_hm, 
         plot_variable = 'kappa', 
         fill_lab = "Cohen's \u03BA", 
         label_range = FALSE, 
         midpoint = 0.25, 
         limits = c(0, 0.5), 
         oob = scales::squish, 
         palette = c(low = 'firebrick', 
                     mid = 'white', 
                     high = 'steelblue'))
  
  ## regression models, MAE presented in the subtitle
  
  dlco_resid$hm_plots$DLCO_percent <- 
    list(data = dlco_resid$stats$DLCO_percent, 
         plot_title = paste('DLCO,', 
                            globals$algo_labs[names(dlco_resid$stats$DLCO_percent)]), 
         plot_subtitle = dlco_resid$global_stats$mae) %>% 
    pmap(plot_resid_hm, 
         plot_variable = 'mean', 
         fill_lab = 'mean absolute error', 
         label_range = FALSE, 
         midpoint = 10, 
         limits = c(5, 15), 
         oob = scales::squish)
  
  ## regression models, MAE shown in the subtitle
  
  dlco_resid$hm_plots$DLCO_residuals <- 
    list(data = dlco_resid$stats$DLCO_residuals, 
         plot_title = paste('DLCO,', 
                            globals$algo_labs[names(dlco_resid$stats$DLCO_residuals)]), 
         plot_subtitle = dlco_resid$global_stats$mae) %>% 
    pmap(plot_resid_hm, 
         plot_variable = 'mean', 
         fill_lab = 'mean error', 
         label_range = FALSE, 
         #midpoint = 10, 
         #limits = c(5, 15), 
         oob = scales::squish)

# END -----
  
  dlco_resid$n_numbers <- NULL
  
  dlco_resid <- compact(dlco_resid)
  
  insert_tail()