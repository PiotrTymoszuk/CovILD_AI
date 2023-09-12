# Figures for the analysis report

  insert_head()
  
# container ------
  
  figures <- list()
  
# Figure 1: time course of CT parameters ------
  
  insert_msg('Figure 1: time course of the CT findings')
  
  figures$ct_finding_course <- 
    cohort_ct$plots$factor[c("CT_findings", 
                             "GGO_finding", 
                             "reticulation_finding", 
                             "consolidation_finding")] %>% 
    map(~.x + 
          expand_limits(y = 130) + 
          theme(legend.position = 'none') + 
          labs(y = '% of FUP strata')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    plot_grid(get_legend(cohort_ct$plots$factor[[1]] + 
                           labs(fill = 'Abnormality')), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_1_ct_abnormality_time_course', 
              ref_name = 'ct_finding_course', 
              caption = paste('Frequency of computed tomography abnormalities', 
                              'at the consecutive follow-ups in the CovILD', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 290)
  
  
# Figure 2: time course of numeric CT variables -----
  
  insert_msg('Figure 2: pathology scoring at the follow-ups')
  
  figures$ct_numeric_course <- 
    cohort_ct$plots$numeric[c("CTSS", 
                              "opacity_percent", 
                              "high_opacity_percent")] %>% 
    map2(c(25, 50, 4), 
         ~.x + expand_limits(y = .y)) %>%
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    as_figure(label = 'figure_2_ct_paramater_time_course', 
              ref_name = 'ct_numeric_course', 
              caption = paste('Time course of numeric computed tomography',  
                              'readouts at the consecutive follow-ups in', 
                              'the CovILD participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 230)
  
# Figure 3: time course of LFT abnormalities ------
  
  insert_msg('Figure 3: time course of LFT abnormalities')
  
  figures$lft_finding_course <- 
    cohort_lft$plots$factor[c("LFT_findings", 
                              "DLCO_reduced", 
                              "FVC_reduced", 
                              "FEV1_reduced")] %>% 
    map(~.x + 
          expand_limits(y = 130) + 
          theme(legend.position = 'none') + 
          labs(y = '% of FUP strata')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    plot_grid(get_legend(cohort_ct$plots$factor[[1]] + 
                           labs(fill = 'Abnormality')), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_3_lft_abnormality_time_course', 
              ref_name = 'lft_finding_course', 
              caption = paste('Frequency of lung function testing abnormalities', 
                              'at the consecutive follow-ups in the CovILD', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 290)
  

  
# Figure 4: time course of numeric LFT parameters -----
  
  insert_msg('Figure 4: time course of numeric LFT features')
  
  figures$lft_numeric_course <- 
    cohort_lft$plots$numeric[c("DLCO_percent", 
                               "FVC_percent", 
                               "FEV1_percent")] %>% 
    map2(c(175, 155, 165), 
         ~.x + expand_limits(y = .y)) %>%
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    as_figure(label = 'figure_4_lft_paramater_time_course', 
              ref_name = 'lft_numeric_course', 
              caption = paste('Time course of numeric lung function testing',  
                              'readouts at the consecutive follow-ups in', 
                              'the CovILD participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 230)
  
# Figure 5 and 6: ROC for AI at detection of CT abnormality -----
  
  insert_msg('Figures 5 and 6: ROC for opacity and high opacity, CT findings')
  
  figures[c('ai_opacity_roc', 
            'ai_high_opacity_roc')] <- 
    list(cut_opacity, cut_high) %>% 
    map(~.x$roc_plots[c("CT_findings", 
                        "GGO_finding", 
                        "reticulation_finding", 
                        "consolidation_finding")]) %>% 
    map2(., c('AI opactity', 'AI high opacity'), 
         function(x, y) x %>% 
           map(~.x + 
                 labs(title = paste(.x$labels$title, y, sep = ', ')))) %>% 
    map(~plot_grid(plotlist = ., 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr'))
  
  figures[c('ai_opacity_roc', 
            'ai_high_opacity_roc')] <- 
    figures[c('ai_opacity_roc', 
              'ai_high_opacity_roc')] %>% 
    list(x = ., 
         label = c('figure_5_ct_findings_ai_opacity', 
                   'figure_6_ct_findings_ai_high_opacity'), 
         ref_name = names(.), 
         caption = c(paste('Detection of human-recognizable abnormalities', 
                           'of chest computed tomography by artificial', 
                           'intelligence-measured lung opacity.'), 
                     paste('Detection of human-recognizable abnormalities', 
                           'of chest computed tomography by artificial', 
                           'intelligence-measured lung high opacity.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 180)
  
# Figure 7 and 8: CT abnormalities by opacity in severity classes and at FUPs -----
  
  insert_msg('Figure 7 - 8: opacity and CT abnormalities, CoV severity and FUP')
  
  ## I'm showing kappa only, setting the common x scale
  
  figures[c('ai_opacity_severity', 
            'ai_opacity_fup')] <- op_strata[c("severity_plots", 
                                              "follow_up_plots")] %>% 
    map(~.x[c("CT_findings", 
              "GGO_finding", 
              "reticulation_finding", 
              "consolidation_finding")]) %>% 
    map(map, 
        ~.x$kappa + 
          labs(title = paste0(.x$kappa$labels$title, ', AI opacity')) + 
          scale_x_continuous(limits = c(0, 1)) + 
          theme(legend.position = 'none')) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv',
                   axis = 'tblr'))
  
  figures[c('ai_opacity_severity', 
            'ai_opacity_fup')] <- 
    figures[c('ai_opacity_severity', 
              'ai_opacity_fup')] %>% 
    list(x = ., 
         label = c('figure_7_ai_opacity_cov_severity', 
                   'figure_8_ai_opacity_follow_up'), 
         ref_name = names(.), 
         caption = c(paste('Performance of artificial intelligence-determined', 
                           'lung opacity at detection of chest computed', 
                           'tomography abnormalities in patients stratified', 
                           'by severity of acute COVID-19.'), 
                     paste('Performance of artificial intelligence-determined', 
                           'lung opacity at detection of chest computed', 
                           'tomography abnormalities at post-COVID-19 follow-up', 
                           'evaluations.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 210)
  
# Figure 9 and 10: CT abnormalities by high opacity, severity classes and FUPs -----
  
  insert_msg('Figures 9 - 10, CT abnormalities, high opacity: severity and FUP')

  figures[c('ai_high_opacity_severity', 
            'ai_high_opacity_fup')] <- high_strata[c("severity_plots", 
                                                     "follow_up_plots")] %>% 
    map(~.x[c("CT_findings", 
              "GGO_finding", 
              "reticulation_finding", 
              "consolidation_finding")]) %>% 
    map(map, 
        ~.x$kappa + 
          labs(title = paste0(.x$kappa$labels$title, ', AI high opacity')) + 
          scale_x_continuous(limits = c(0, 1)) + 
          theme(legend.position = 'none')) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv',
                   axis = 'tblr'))
  
  figures[c('ai_high_opacity_severity', 
            'ai_high_opacity_fup')] <- 
    figures[c('ai_high_opacity_severity', 
              'ai_high_opacity_fup')] %>% 
    list(x = ., 
         label = c('figure_9_ai_high_opacity_cov_severity', 
                   'figure_10_ai_high_opacity_follow_up'), 
         ref_name = names(.), 
         caption = c(paste('Performance of artificial intelligence-determined', 
                           'lung high opacity at detection of chest computed', 
                           'tomography abnormalities in patients stratified', 
                           'by severity of acute COVID-19.'), 
                     paste('Performance of artificial intelligence-determined', 
                           'lung high opacity at detection of chest computed', 
                           'tomography abnormalities at post-COVID-19 follow-up', 
                           'evaluations.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 210)
    
# Figure 11: CTSS and ILD detection --------
  
  insert_msg('Figure 11: CTSS and ILD detection')
  
  ## upper panel: determination of a cutoff
  
  figures$ctss_ild_cutoff$upper <- 
    plot_grid(ild_cutoff$roc_plot, 
              ncol = 2)
  
  ## bottom panel: kappa in the severity strata and at the follow-ups
  
  figures$ctss_ild_cutoff$bottom <- 
    ild_strata[c("severity_plots", "follow_up_plots")] %>%
    map(~.x$kappa + 
          scale_x_continuous(limits = c(0, 1)) + 
          labs(title = 'ILD detection by CTSS') + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figure
  
  figures$ctss_ild_cutoff <- 
    plot_grid(figures$ctss_ild_cutoff$upper, 
              figures$ctss_ild_cutoff$bottom, 
              nrow = 2, 
              rel_heights = c(90, 105), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_11_ILD_detection_by_CTSS', 
              ref_name = 'ctss_ild_cutoff', 
              caption = paste('Detection of interstitial lung disease', 
                              'defined by opacity >5% of the lung by', 
                              'a human-determined computed tomography', 
                              'severity score.'), 
              w = 180, 
              h = 200)

# Figure 12: CTSS - AI relationship -------
  
  insert_msg('Figure 12: CTSS vs AI')
  
  figures$ctss_ai_relation <- ctss_fun$fit_plots %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', '', 
                         'B', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'figure_12_ctss_ai_relation', 
              ref_name = 'ctss_ai_relation', 
              caption = paste("Mathematical relationship between artificial", 
                              "intelligence-determined lung opacity and high", 
                              "opacity and radiologist's computed tomography", 
                              "severity score."), 
              w = 180, 
              h = 140)
  
# Figure 13: tuning of the reduced DLCO models ------  
  
  insert_msg('Figures 13: tuning of the DLCO < 80% models')
  
  ## upper panel: GBM
  
  figures$dlco_red_tuning$upper <- 
    plot_grid(dlco_red_tune$tuning_plots$gbm + 
                theme(legend.position = 'bottom'),
              labels = 'A', 
              label_size = 10)
  
  ## left panel: neuronal network
  
  figures$dlco_red_tuning$left <- 
    plot_grid(dlco_red_tune$tuning_plots$nnet + 
                theme(legend.position = 'none'), 
              labels = 'B', 
              label_size = 10)
  
  ## right panel: ranger and SVM
  
  figures$dlco_red_tuning$right <- 
    plot_grid(dlco_red_tune$tuning_plots$ranger + 
                theme(legend.position = 'bottom'), 
              dlco_red_tune$tuning_plots$svmRadial, 
              nrow = 2, 
              rel_heights = c(1.2, 1), 
              labels = c('C', 'D'), 
              label_size = 10)
  
  ## the entire figure
  
  figures$dlco_red_tuning <- 
    plot_grid(figures$dlco_red_tuning$left, 
              figures$dlco_red_tuning$right, 
              ncol = 2) %>% 
    plot_grid(figures$dlco_red_tuning$upper, ., 
              nrow = 2, 
              rel_heights = c(1, 2)) %>% 
    as_figure(label = 'figure_13_dlco_reduced_tuning', 
              ref_name = 'dlco_red_tuning', 
              caption = paste('Tuning of machine learning models', 
                              'of insufficient diffusion capacity for carbon', 
                              'monoxide.'), 
              w = 180, 
              h = 230)
  
# Figure 15: tuning of the DLCO regression models ------
  
  insert_msg('Figures 15: tuning of the DLCO regression models')
  
  ## upper panel: GBM
  
  figures$dlco_tuning$upper <- 
    plot_grid(dlco_tune$tuning_plots$gbm + 
                theme(legend.position = 'bottom'),
              labels = 'A', 
              label_size = 10)
  
  ## left panel: neuronal network
  
  figures$dlco_tuning$left <- 
    plot_grid(dlco_tune$tuning_plots$nnet + 
                theme(legend.position = 'none'), 
              labels = 'B', 
              label_size = 10)
  
  ## right panel: ranger and SVM
  
  figures$dlco_tuning$right <- 
    plot_grid(dlco_tune$tuning_plots$ranger + 
                theme(legend.position = 'bottom'), 
              dlco_tune$tuning_plots$svmRadial, 
              nrow = 2, 
              rel_heights = c(1.2, 1), 
              labels = c('C', 'D'), 
              label_size = 10)
  
  ## the entire figure
  
  figures$dlco_tuning <- 
    plot_grid(figures$dlco_tuning$left, 
              figures$dlco_tuning$right, 
              ncol = 2) %>% 
    plot_grid(figures$dlco_tuning$upper, ., 
              nrow = 2, 
              rel_heights = c(1, 2)) %>% 
    as_figure(label = 'figure_15_dlco_regression_tuning', 
              ref_name = 'dlco_tuning', 
              caption = paste('Tuning of machine learning regression models', 
                              'of diffusion capacity for carbon monoxide.'), 
              w = 180, 
              h = 230)
  
# Figures 14 and 16: DLCO models, resamples ------
  
  insert_msg('Figure 14 and 16: DLCO models, resamples')
  
  figures[c('dlco_red_resamples', 
            'dlco_resamples')] <- 
    list(dlco_red_tune, dlco_tune) %>% 
    map(~.x$resample_plots)
  
  figures$dlco_red_resamples <- figures$dlco_red_resamples %>% 
    map(~.x + 
          geom_hline(yintercept = 1, 
                     linetype = 'dashed', 
                     color = 'orangered2'))
  
  figures[c("dlco_red_resamples", 
            "dlco_resamples")] <- 
    figures[c("dlco_red_resamples", 
              "dlco_resamples")] %>% 
    map2(., 
         list(c(0.75, 2), c(5, 20)), 
         function(plots, lims) plots %>% 
           map(~.x + scale_y_continuous(limits = lims))) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr'))
  
  figures[c('dlco_red_resamples', 
            'dlco_resamples')] <- 
    figures[c('dlco_red_resamples', 
              'dlco_resamples')] %>% 
    list(x = ., 
         label = c('figure_14_reduced_dlco_model_resamples', 
                   'figure_16_dlco_model_resamples'), 
         ref_name = names(.),
         caption = c(paste('Performance of machine learning models', 
                           'of insufficient diffusion capacity for carbon', 
                           'monoxide in cross-validation folds.'), 
                     paste('Performance of machine learning regression models', 
                           'of diffusion capacity for carbon', 
                           'monoxide in cross-validation folds.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 180)

# Figure 17: performance of binary classifiers --------
  
  insert_msg('Figure 17: performance of binary classifiers of LFT abnormalities')
  
  figures$bin_performance <- bin_models$performance_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(bin_models$performance_plots[[1]] + 
                           theme(legend.position = 'bottom', 
                                 legend.box = 'vertical', 
                                 legend.margin = ggplot2::margin())), 
              nrow = 2, 
              rel_heights = c(0.8, 0.2)) %>% 
    as_figure(label = 'figure_17_performance_lft_abnormality_models', 
              ref_name = 'bin_performance', 
              caption = paste('Performance of machine learning models', 
                              'at prediction of abnormalities', 
                              'of lung function testing in the training', 
                              'data set and repeated cross-validation.'), 
              w = 180, 
              h = 210)
  
# Figure 18: ROC plots for DLCO and predicted DLCO frequencies ------
  
  insert_msg('Figure 18: DLCO, ROC and predicted frequencies')
  
  ## upper panel: ROC

  figures$dlco_red_performance$upper <- bin_models$roc_plots$DLCO_reduced %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(bin_models$roc_plots$DLCO_reduced[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  ## bottom panel
  
  figures$dlco_red_performance$bottom <- bin_models$DLCO_frequency_plots %>% 
    map(~.x$cohort + 
          labs(y = 'DLCO < 80%, % of cohort') + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 40))) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(bin_models$DLCO_frequency_plots[[1]]$cohort + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  ## the entire figure
  
  figures$dlco_red_performance <- 
    plot_grid(figures$dlco_red_performance$upper, 
              figures$dlco_red_performance$bottom, 
              nrow = 2, 
              rel_heights = c(100, 140), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_18_reduced_dlco_model_performance', 
              ref_name = 'dlco_red_performance', 
              caption = paste('Quality of prediction of insufficient diffusion', 
                              'capacity for carbon monoxide with', 
                              'machine learning models.'), 
              w = 180, 
              h = 240)
    
# Figure 19: performance of regression models -----
  
  insert_msg('Figure 19: performance of regression models')
  
  figures$reg_performance <- reg_models$performance_plots %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(reg_models$performance_plots[[1]] + 
                           theme(legend.position = 'bottom', 
                                 legend.box = 'vertical', 
                                 legend.margin = ggplot2::margin())), 
              nrow = 2, 
              rel_heights = c(0.8, 0.2)) %>% 
    as_figure(label = 'figure_19_performance_lft_regression_models', 
              ref_name = 'reg_performance', 
              caption = paste('Performance of machine learning regression', 
                              'models at prediction of lung function testing', 
                              'parameters in the training', 
                              'data set and repeated cross-validation.'), 
              w = 180, 
              h = 210)
  
# Figure 20 - 21: fitted vs observed and time course of DLCO ------
  
  insert_msg('Figures 20 - 21: fitted vs observed and time course of DLCO')
  
  ## fitted vs observed
  
  figures$dlco_performance <- reg_models$scatter_plots$DLCO_percent %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  ## time course
  
  figures$dlco_model_course <- reg_models$DLCO_course_plots %>% 
    map(~.x$cohort + 
          scale_y_continuous(limits = c(85, 100)) + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(reg_models$DLCO_course_plots[[1]]$cohort + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  ## the figure objects
  
  figures[c('dlco_performance', 'dlco_model_course')] <- 
    figures[c('dlco_performance', 'dlco_model_course')] %>% 
    list(x = ., 
         label = c('figure_20_dlco_regression_model_performance', 
                   'figure_21_dlco_regression_time_course'), 
         ref_name = names(.), 
         caption = c(paste('Observed and model-fitted values of diffusion', 
                           'capacity for carbon dioxide.'), 
                     paste('Observed and model-predicted time course', 
                           'of diffusion capacity for carbon monoxide', 
                           'at consecutive post-COVID-19 follow-up', 
                           'examinations.')), 
         h = c(240, 140)) %>% 
    pmap(as_figure, 
         w = 180)
  
# Figure 22 - 23: DLCO, global variable importance -------
  
  insert_msg('Figures 22 - 23 DLCO variable importance')
  
  figures[c('dlco_reduced_global_vim', 
            'dlco_regression_global_vim')] <- 
    caret_imp$plots[c("DLCO_reduced", "DLCO_percent")] %>% 
    map(~plot_grid(plotlist = ., 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tlr'))
  
  figures[c('dlco_reduced_global_vim', 
            'dlco_regression_global_vim')] <- 
    figures[c('dlco_reduced_global_vim', 
              'dlco_regression_global_vim')] %>% 
    list(x = ., 
         label = c('figure_22_reduced_dlco_model_variable_importance', 
                   'figure_23_regression_dlco_model_variable_importance'), 
         ref_name = names(.), 
         caption = c(paste('Top most influential variables for prediction', 
                           'of abnormalities of diffusion capacity', 
                           'for carbon monoxide by machine learning', 
                           'models.'), 
                     paste('Top most influential variables for prediction', 
                           'of diffusion capacity for carbon monoxide by', 
                           'machine learning models.'))) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 180)
    
  
# Figure 24: univariable insufficient DLCO -------
  
  insert_msg('Figure 24: univariable insufficient DLCO')
  
  ## upper panel: effect plot
  
  figures$univariable_dlco_red$upper <- 
    dlco_red_uni$eff_size_plot
  
  ## bottom panel: single variables
  
  figures$univariable_dlco_red$bottom <- 
    dlco_red_uni$plots[c("CTSS", 
                         "opacity_percent", 
                         "high_opacity_percent", 
                         "anticoagulant_treatment")]
  
  figures$univariable_dlco_red$bottom$anticoagulant_treatment <- 
    figures$univariable_dlco_red$bottom$anticoagulant_treatment + 
    labs(fill = 'Anti-coagulation')
  
  figures$univariable_dlco_red$bottom <- 
    figures$univariable_dlco_red$bottom %>% 
    map(~.x + labs(x = 'DLCO < 80%'))
  
  figures$univariable_dlco_red$bottom[c("CTSS", 
                                        "opacity_percent", 
                                        "high_opacity_percent")] <- 
    figures$univariable_dlco_red$bottom[c("CTSS", 
                                          "opacity_percent", 
                                          "high_opacity_percent")] %>% 
    map2(., c('identity', 'sqrt', 'sqrt'), 
         ~.x + 
           scale_y_continuous(trans = .y) + 
           theme(legend.position = 'none'))
  
  figures$univariable_dlco_red$bottom <- 
    figures$univariable_dlco_red$bottom %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figure
  
  figures$univariable_dlco_red <- 
    plot_grid(figures$univariable_dlco_red$upper, 
              figures$univariable_dlco_red$bottom, 
              nrow = 2, 
              rel_heights = c(90, 140), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'figure_24_univariable_analysis_dlco', 
              ref_name = 'univariable_dlco_red', 
              caption = paste('Human- and artificial intelligence-derived', 
                              'measures of severity of radiological lung', 
                              'abnormality and anti-coagulation treatment', 
                              'are strong predictors of insufficient', 
                              'diffusion capacity for carbon monoxide.'), 
              w = 180, 
              h = 230)
  
# Figure 25: correlations with DLCO ------
  
  insert_msg('Figure 25: correlations with DLCO')
  
  figures$univariable_dlco_corr <- 
    dlco_uni$correlation$plots[c("CTSS", 
                                 "opacity_percent", 
                                 "high_opacity_percent", 
                                 "hospital_stay_days")] %>% 
    map(~.x + theme(plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_25_dlco_univariable_correlations', 
              ref_name = 'univariable_dlco_corr', 
              caption = paste("Radiologist's computed tomography severity", 
                              "score, artificial intelligence-computed lung", 
                              "opacity and lung high opacity, and length", 
                              "of hospital stay are significant correlates", 
                              "of diffusion capacity for carbon monoxide."), 
              w = 180, 
              h = 170)
  
# Figure 26: DLCO, ROC --------
  
  insert_msg('Figure 27: DLCO ROC')
  
  figures$dlco_roc <- 
    lft_roc$roc_plots$DLCO_reduced[c("CTSS", 
                                     "opacity_percent", 
                                     "high_opacity_percent")] %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'figure_26_dlco_detection_by_ct_roc', 
              ref_name = 'lft_roc', 
              caption = paste('Detection of insufficient', 
                              'diffusion capacity for carbon monoxide', 
                              'by human- and artificial intelligence-', 
                              'derived measures of radiological', 
                              'lung abnormality.'), 
              w = 180, 
              h = 180)
  
# saving the figures of the disc ------
  
  insert_msg('Saving the figures on the disc')
  
  figures %>%
    walk(pickle,
         path = './report/figures', 
         dev = cairo_pdf)
  
# END ------
  
  insert_tail()