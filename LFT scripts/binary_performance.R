# Checking performance of ML models for the binary responses: 
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
    list(LFT_findings = lft_tune, 
         DLCO_reduced = dlco_red_tune, 
         FVC_reduced = fvc_red_tune, 
         FEV1_reduced = fev1_red_tune) %>% 
    map(~.x$models)

# Predictions ------
  
  insert_msg('Predictions')
  
  bin_models$predictions <- bin_models$models %>% 
    map(map, predict) %>% 
    map(map, compact) %>% 
    map(transpose) %>% 
    transpose

# Model stats ------
  
  insert_msg('Model stats')
  
  ## for the entire cohort
  
  bin_models$stats <- bin_models$models %>% 
    future_map(map, summary.caretx, 
               .options = furrr_options(seed = TRUE)) %>% 
    format_ml_summary %>% 
    map(mutate, 
        J = Se + Sp - 1)
  
  ## CV stats for severity strata for the best performing DLCO models
  
  bin_models$stats_dlco_severity <- bin_models$models$DLCO_reduced %>% 
    map(split, severity_class) %>% 
    map(~.x[stri_detect(names(.x), fixed = 'cv')]) %>% 
    future_map(map, summary.predx, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(format_strata_summary, 'severity_class') %>% 
    map(mutate, 
        severity_class = stri_replace(severity_class, fixed = 'cv.', replacement = '')) %>% 
    compress(names_to = 'algorithm') %>% 
    mutate(severity_class = factor(severity_class, 
                                   levels(model.frame(bin_models$models[[1]][[1]])$severity_class)))
  
  ## for the time points
  
  bin_models$stats_dlco_fup <- bin_models$models$DLCO_reduced %>% 
    map(split, follow_up) %>% 
    map(~.x[stri_detect(names(.x), fixed = 'cv')]) %>% 
    future_map(map, summary.predx, 
               .options = furrr_options(seed = TRUE)) %>% 
    map(format_strata_summary, 'follow_up') %>% 
    map(mutate, 
        follow_up = stri_replace(follow_up, fixed = 'cv.', replacement = '')) %>% 
    compress(names_to = 'algorithm') %>% 
    mutate(follow_up = factor(follow_up, 
                              levels(model.frame(bin_models$models[[1]][[1]])$follow_up)))
  
  ## Youden's J for the subsets
  
  bin_models[c("stats_dlco_severity", "stats_dlco_fup")] <- 
    bin_models[c("stats_dlco_severity", "stats_dlco_fup")] %>% 
    map(mutate, J = Se + Sp - 1)

# Diagnostic plots: squared errors and class assignment p ------
  
  insert_msg('Plotting of square errors and class assignment, CV')
  
  ## done for the CV only
  
  bin_models$diagnostic_plots <- bin_models$predictions$cv %>%
    future_map(map, 
               plot, 
               type = 'class_p', 
               cust_theme = globals$common_theme, 
               label_misclassified = FALSE, 
               .options = furrr_options(seed = TRUE))
  
# Performance plots: Brier score and Kappa -------
  
  insert_msg('Performance plots')
  
  ## setting common scales
  
  bin_models$performance_plots <- 
    list(stats = bin_models$stats, 
         model = map(bin_models$models, ~.x[[1]]), 
         plot_title = exchange(names(bin_models$stats), 
                               globals$lft_lexicon)) %>%
    pmap(plot_binary_performance,
         box.padding = 0.4) %>% 
    map(~.x + 
          scale_radius(limits = c(0.5, 1), 
                       range = c(2, 5), 
                       name = 'Overall accuracy'))
  
# Confusion matrices -------
  
  insert_msg('Confusion matrices')
  
  bin_models$confusion_plots <- 
    list(train_predictions = bin_models$predictions$train, 
         cv_predictions = bin_models$predictions$cv, 
         title_prefix = exchange(names(bin_models$predictions$train), 
                                 globals$lft_lexicon)) %>% 
    future_pmap(plot_confusion_hm,
                .options = furrr_options(seed = TRUE))

# ROC curves ---------
  
  insert_msg('ROC curves')
  
  ## separate training and CV plots. Algorithm is coded by color.
  ## AUC, sensitivity and specificity are indicated in the plot
  
  bin_models$roc_plots <- 
    list(train_predictions = bin_models$predictions$train, 
         cv_predictions = bin_models$predictions$cv, 
         stats = bin_models$stats, 
         title_prefix = exchange(names(bin_models$predictions$train), 
                                 globals$lft_lexicon)) %>% 
    future_pmap(plot_ml_roc,
                annot_x = 0.2, 
                .options = furrr_options(seed = TRUE))

# Predictive performance in the severity strata and follow-ups ------
  
  insert_msg('Predictive performance in the severity stats and FUPs')
  
  ## done only for DLCO and cross-validation
  
  bin_models[c('performance_plots_severity', 
               'performance_plots_fup')] <- 
    list(stats = bin_models[c("stats_dlco_severity", "stats_dlco_fup")], 
         strata = c('severity_class', 'follow_up'), 
         plot_title = paste('DLCO < 80% and', 
                            c('COVID-19 severity', 
                              'follow-up')), 
         x_lab = c('COVID-19 severity', 
                   'post-COVID-19 follow-up')) %>% 
    pmap(plot_binary_strata_stats) %>% 
    map2(., globals[c("sev_labels", "fup_labels")], 
         function(x, y) map(x, ~.x + scale_x_discrete(labels = y)))
    
# Time course plots for DLCO findings ------
  
  insert_msg('Time course plots for DLCO findings')

  bin_models$DLCO_frequency_plots <- 
    list(caretx_model = bin_models$models$DLCO_reduced, 
         title_prefix = globals$algo_labs[names(bin_models$models$DLCO_reduced)]) %>% 
    pmap(plot_binary_course)

# END -----
  
  plan('sequential')
  
  insert_tail()