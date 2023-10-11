# Checking performance of Random Forest models for the numeric responses: 
# any DLCO, FVC and FEV1

  insert_head()
  
# container -----
  
  reg_models <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## caret models
  
  reg_models$models <- 
    list(DLCO_percent = dlco_tune, 
         FVC_percent = fvc_tune, 
         FEV1_percent = fev1_tune) %>% 
    map(~.x$models)
  
# Predictions ------
  
  insert_msg('Predictions')
  
  reg_models$predictions <- reg_models$models %>% 
    map(map, predict) %>% 
    map(map, compact) %>% 
    map(transpose) %>% 
    transpose
  
# Model residuals: plots -----
  
  insert_msg('Diagnostic model residuals plots')
  
  reg_models$diagnostic_plots <- reg_models$models %>% 
    future_map(map, 
               plot.caretx, type = 'diagnostic', 
               .options = furrr_options(seed = TRUE))
  
# Model residuals: normality --------
  
  insert_msg('Normality of model residuals')
  
  reg_models$residuals <- reg_models$predictions %>% 
    map(unlist, recursive = FALSE) %>% 
    map(map, residuals)
  
  ## Shapiro-Wilk test
  
  reg_models$resid_normailty <- reg_models$residuals %>%
    map(map, ~.x$.resid) %>% 
    map(map, shapiro_test) %>% 
    map(compress, names_to = 'response') %>% 
    map(mutate, 
        algorithm = stri_split_fixed(response, pattern = '.', simplify = TRUE)[, 2], 
        response = stri_split_fixed(response, pattern = '.', simplify = TRUE)[, 1],
        response = factor(response, names(reg_models$models))) %>% 
    map(select, -variable) %>% 
    compress(names_to = 'dataset') %>% 
    blast(response)

# Model stats ------
  
  insert_msg('Model stats')
  
  reg_models$stats <- reg_models$models %>% 
    future_map(map, 
               summary.caretx, 
               .options = furrr_options(seed = TRUE))

  ## formatting the summary stats
  
  reg_models$stats <- reg_models$stats %>% 
    format_ml_summary
  
# CV stats for the subsets --------
  
  insert_msg('CV performance stats for the severity subsets and FUP')
  
  ## severity strata results
  
  reg_models$stats_dlco_severity <- reg_models$models$DLCO_percent %>% 
    map(split, severity_class) %>% 
    map(~.x[stri_detect(names(.x), fixed = 'cv')]) %>% 
    map(future_map, summary.predx, 
        .options = furrr_options(seed = TRUE)) %>% 
    map(format_strata_summary, 'severity_class') %>% 
    map(mutate, 
        severity_class = stri_replace(severity_class, fixed = 'cv.', replacement = '')) %>% 
    compress(names_to = 'algorithm') %>% 
    mutate(severity_class = factor(severity_class, 
                                   levels(model.frame(bin_models$models[[1]][[1]])$severity_class)))
  
  ## follow-up
  
  reg_models$stats_dlco_fup <- reg_models$models$DLCO_percent %>% 
    map(split, follow_up) %>% 
    map(~.x[stri_detect(names(.x), fixed = 'cv')]) %>% 
    map(future_map, summary.predx, 
        .options = furrr_options(seed = TRUE)) %>% 
    map(format_strata_summary, 'follow_up') %>% 
    map(mutate, 
        follow_up = stri_replace(follow_up, fixed = 'cv.', replacement = '')) %>% 
    compress(names_to = 'algorithm') %>% 
    mutate(follow_up = factor(follow_up, 
                              levels(model.frame(bin_models$models[[1]][[1]])$follow_up)))
  

# Plotting the general performance ------
  
  insert_msg('Plots of general performance stats')
  
  ## MAE and R-squared
  ## setting common scales
  
  reg_models$performance_plots <- 
    list(stats = reg_models$stats, 
         model = map(reg_models$models, ~.x[[1]]), 
         plot_title = exchange(names(reg_models$stats), 
                               globals$lft_lexicon)) %>%
    pmap(plot_reg_performance,
         box.padding = 0.4) %>% 
    map(~.x + 
          geom_vline(xintercept = 0,
                     linetype = 'dashed') + 
          scale_radius(limits = c(0, 1), 
                       range = c(1, 5), 
                       name = expression("Spearman's " * rho)) + 
          scale_x_continuous(limits = c(0, 1)) + 
          scale_y_continuous(limits = c(0.05, 0.23)))
  
# Plotting fitted vs observed values --------
  
  insert_msg('Plots of fitted vs observed values')

  reg_models$scatter_plots <- 
    list(train_predictions = reg_models$predictions$train, 
         cv_predictions = reg_models$predictions$cv, 
         stats = reg_models$stats, 
         title_prefix = exchange(names(reg_models$predictions$train), 
                                 globals$lft_lexicon)) %>% 
    pmap(plot_fit_observed)
  
# Performance of the models in the severity strata and at FUP ------
  
  insert_msg('Predictive performance in the severity stats and FUPs')
  
  ## done only for DLCO and cross-validation
  
  reg_models[c('performance_plots_severity', 
               'performance_plots_fup')] <- 
    list(stats = reg_models[c("stats_dlco_severity", "stats_dlco_fup")], 
         strata = c('severity_class', 'follow_up'), 
         plot_title = paste('DLCO and', 
                            c('COVID-19 severity', 
                              'follow-up')), 
         x_lab = c('COVID-19 severity', 
                   'post-COVID-19 follow-up')) %>% 
    pmap(plot_reg_strata_stats, 
         invert_mae = FALSE) %>% 
    map2(., globals[c("sev_labels", "fup_labels")], 
         function(x, y) map(x, ~.x + scale_x_discrete(labels = y)))
  
# Time course of DLCO --------
  
  insert_msg('Time course of DLCO')
  
  reg_models$DLCO_course_plots <- 
    list(caretx_model = reg_models$models$DLCO_percent, 
         title_prefix = globals$algo_labs[names(bin_models$models$DLCO_reduced)]) %>% 
    pmap(plot_reg_course)
  
# END -----
  
  plan('sequential')
  
  rm(reg_diagnostic_plots, reg_stats)
  
  insert_tail()