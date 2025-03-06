# Supplementary Figures

  insert_head()
  
# container --------
  
  suppl_figs <- list()
  
# time course of CT findings -------
  
  insert_msg('Time course of the CT findings')
  
  suppl_figs$ct_finding_course <- 
    cohort_ct$plots$factor[c("GGO_finding", 
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
    as_figure(label = 'ct_abnormality_time_course', 
              ref_name = 'ct_finding_course', 
              caption = paste('Frequency of the most common abnormalities in', 
                              'computed tomography of the chest', 
                              'at the consecutive follow-ups in', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 210)
  
# time course of numeric CT variables -----
  
  insert_msg('CT pathology scoring at the follow-ups')
  
  suppl_figs$ct_numeric_course <- 
    cohort_ct$plots$numeric[c("CTSS", 
                              "opacity_percent", 
                              "high_opacity_percent")] %>% 
    list(x = ., 
         y = c(26, 62, 6), 
         z = c('identity', 'sqrt', 'sqrt')) %>% 
    pmap(function(x, y, z) x + 
           expand_limits(y = y) + 
           scale_y_continuous(trans = z)) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    as_figure(label = 'figure_s2_ct_paramater_time_course', 
              ref_name = 'ct_numeric_course', 
              caption = paste('Time course of numeric computed tomography',  
                              'readouts at the consecutive follow-ups in', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 210)
  
# CT feature overlap --------
  
  insert_msg('Overlap of the CT features')
  
  ## upper 
  
  suppl_figs$ct_corr$upper <- 
    plot_grid(mca_lft$column_plot$ct, 
              mca_lft$venn_plot$ct,
              ncol = 2, 
              #align = 'hv', 
              axis = 'tblr')
  
  ## bottom
  
  suppl_figs$ct_corr$bottom <- corr_lft$plots$ct %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr')
  
  ## the entire figure
  
  suppl_figs$ct_corr <- 
    plot_grid(suppl_figs$ct_corr$upper, 
              suppl_figs$ct_corr$bottom, 
              nrow = 2, 
              rel_heights = c(1, 2), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'cooccurrence_ct_findings', 
              ref_name = 'ct_corr', 
              caption = paste('Co-occurrence of abnormalities of chest', 
                              'computed tomography and correlation of computed', 
                              'tomography readouts in COVID-19 convalescents.'), 
              w = 180, 
              h = 210)
  
# time course of LFT abnormalities -------
  
  insert_msg('Time course of LFT abnormalities')
  
  suppl_figs$lft_finding_course <- 
    cohort_lft$plots$factor[c("DLCO_reduced", 
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
    as_figure(label = 'lft_abnormality_time_course', 
              ref_name = 'lft_finding_course', 
              caption = paste('Frequency of lung function testing abnormalities', 
                              'at the consecutive follow-ups in', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 210)
  
# time course of numeric LFT parameters -------
  
  insert_msg('Time course of numeric LFT features')
  
  suppl_figs$lft_numeric_course <- 
    cohort_lft$plots$numeric[c("DLCO_percent", 
                               "FVC_percent", 
                               "FEV1_percent")] %>% 
    map2(c(175, 155, 165), 
         ~.x + expand_limits(y = .y)) %>%
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    as_figure(label = 'lft_parameter_time_course', 
              ref_name = 'lft_numeric_course', 
              caption = paste('Time course of numeric lung function testing',  
                              'readouts at the consecutive follow-ups in', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 210)
  
# correspondence and correlation of LFT outcomes ---------
  
  insert_msg('Correlation and correspondence, LFT outcomes')
  
  ## upper 
  
  suppl_figs$lft_corr$upper <- 
    plot_grid(mca_lft$column_plot$lft, 
              mca_lft$venn_plot$lft,
              ncol = 2, 
              #align = 'hv', 
              axis = 'tblr')
  
  ## bottom
  
  suppl_figs$lft_corr$bottom <- corr_lft$plots$lft %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr')
  
  ## the entire figure
  
  suppl_figs$lft_corr <- 
    plot_grid(suppl_figs$lft_corr$upper, 
              suppl_figs$lft_corr$bottom, 
              nrow = 2, 
              rel_heights = c(1, 2), 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'cooccurrence_lft_findings', 
              ref_name = 'lft_corr', 
              caption = paste('Co-occurrence of abnormalities of lung', 
                              'function testing and correlation of lung', 
                              'function readouts in COVID-19 convalescents.'), 
              w = 180, 
              h = 210)
  
# time course of symptoms of relevance for respiratory function -------
  
  insert_msg('Time course of symptoms of relevance for lung function')
  
  suppl_figs$symptom_course <- 
    cohort_sympt$plots$factor[c("dyspnea_symptom", 
                                "cough_symptom", 
                                "impaired_performance_symptom")] %>% 
    map(~.x + 
          expand_limits(y = 130) + 
          theme(legend.position = 'none') + 
          labs(y = '% of FUP strata')) %>% 
    plot_grid(plotlist = ., 
              ncol = 1, 
              align = 'hv') %>% 
    plot_grid(get_legend(cohort_ct$plots$factor[[1]] + 
                           labs(fill = 'Symptom')), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1)) %>% 
    as_figure(label = 'symptom_time_course', 
              ref_name = 'symptom_course', 
              caption = paste('Frequency of persistent symptoms of relevance',
                              'for lung function', 
                              'at the consecutive follow-ups in', 
                              'participants stratified by severity', 
                              'of acute COVID-19.'), 
              w = 180, 
              h = 210)
  
# modeling strategy --------
  
  insert_msg('Modeling strategy')
  
  suppl_figs$modeling_strategy <- 
    plot_grid(ggdraw() + 
                draw_image('./aux files/modeling_strategy.png')) %>% 
    as_figure(label = 'modeling_strategy', 
              ref_name = 'modeling_strategy', 
              caption = paste('Modeling strategy.'), 
              w = 180, 
              h = 2524/3404 * 180)

# evaluation of the models of findings in FVC and FEV ------
  
  insert_msg('Models of FVC and FEV1 abnormalities')
  
  suppl_figs$fvc_fev_reduced_models <- 
    list(bin_models$performance_plots$FVC_reduced, 
         bin_models$roc_plots$FVC_reduced$cv, 
         bin_models$performance_plots$FEV1_reduced, 
         bin_models$roc_plots$FEV1_reduced$cv) %>% 
    map(~.x + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         'B', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(bin_models$performance_plots$FVC_reduced + 
                           guides(size = 'none', 
                                  shape = 'none') + 
                           theme(legend.position = 'bottom', 
                                 legend.title = element_blank())), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  suppl_figs$fvc_fev_reduced_models <- 
    suppl_figs$fvc_fev_reduced_models %>% 
    as_figure(label = 'performance_fvc_fev_findings_models', 
              ref_name = 'fvc_fev_reduced_models', 
              caption = paste('Evaluation of performance of machine learning', 
                              'classification models at prediction', 
                              'of insufficient', 
                              'forced vital capacity and forced expiratory', 
                              'volume in one second.'), 
              w = 180, 
              h = 200)
  
# evaluation of regressors of FVC and FEV1 -------
  
  insert_msg('Models of FVC and FEV1')
  
  suppl_figs$fvc_fev_models <- 
    reg_models$performance_plots[c("FVC_percent", "FEV1_percent")] %>% 
    map(~.x + 
          theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(reg_models$performance_plots$FVC_percent + 
                           guides(size = 'none', 
                                  shape = 'none') + 
                           theme(legend.position = 'bottom', 
                                 legend.title = element_blank())), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'performance_fvc_fev_regression', 
              ref_name = 'fvc_fev_models', 
              caption = paste('Evaluation of performance of machine learning', 
                              'regression models at prediction of', 
                              'forced vital capacity and forced expiratory', 
                              'volume in one second.'), 
              w = 180, 
              h = 95)
  
# residuals of the models at the FUP and severity strata -------
  
  insert_msg('Model residuals, severity and FUP')
  
  suppl_figs$residuals <- 
    c(dlco_resid$hm_plots$kappa[c("ranger", "nnet", "svmRadial", "gbm")], 
      list(ggdraw(), ggdraw()), 
      dlco_resid$hm_plots$DLCO_residuals[c("ranger", "svmRadial", "gbm")]) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', '', 
                         '', '', '', 
                         'B', '', ''), 
              label_size = 10) %>% 
    as_figure(label = 'model_errors_severity_follow_up', 
              ref_name = 'residuals', 
              caption = paste("Cohen's kappa and mean absolute error", 
                              'of the machine learning models of insufficiency', 
                              'and percentage of reference of diffusion', 
                              'capacity for carbon monoxide.'), 
              w = 180, 
              h = 180)
  
# Learning curves for the models of DLCO < 80% and DLCO -------
  
  insert_msg('Learning curves for the models of reduced and percentage DLCO')

  suppl_figs$learn_curves <- lft_lcurves$plots %>% 
    map(~.x$gbm) %>% 
    unlist(recursive = FALSE)
  
  suppl_figs$learn_curves <- suppl_figs$learn_curves %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         'B', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(suppl_figs$learn_curves[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1))
  
  suppl_figs$learn_curves <- suppl_figs$learn_curves %>% 
    as_figure(label = 'learning_curves_DLCO_models', 
              ref_name = 'learn_curves', 
              caption = paste('Learning curves', 
                              'of the gradient boosted machines models', 
                              'of insufficiency and percentage of reference', 
                              'of diffusion capacity for carbon monoxide.'), 
              w = 180, 
              h = 180)
  
# variable importance for DLCO modeling -------
  
  insert_msg('Variable importance for the DLCO models')

  suppl_figs$varimp_dlco <- 
    shap_imp$violin_bee_plots$DLCO_percent[c("ranger", "svmRadial", "gbm")] %>% 
    map2(., names(.), 
         ~.x + 
           guides(x = guide_axis(angle = 45)) + 
           labs(title = globals$algo_labs[.y]) + 
           theme(legend.position = 'none', 
                 plot.subtitle = element_blank())) %>% 
    c(list(get_legend(shap_imp$violin_bee_plots$DLCO_percent[[1]] + 
                        theme(legend.position = 'right')))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure(label = 'dlco_shap_importance', 
              ref_name = 'varimp_dlco',
              caption = paste('Explanatory variable importance for models', 
                              'of diffusion capacity for carbon monoxide', 
                              'measured by Shapley additive explanations.'), 
              w = 180, 
              h = 200)
  
# Associations between the most influential explanatory variables --------
  
  insert_msg('Correlations between the top influential explanatory variables')
  
  ## upper panel: the correlation graph
  
  suppl_figs$top_corr$upper <- lft_net$plots$all
  
  ## middle panel: scatter plots for visualization of correlations, 
  ## redundant, already present in Supplemetary Figure S3B
  
  #suppl_figs$top_corr$middle <- corr_lft$plots$ct %>% 
   # map(~.x + theme(plot.title.position = 'plot')) %>% 
    #plot_grid(plotlist = .,
     #         ncol = 3, 
      #        align = 'hv', 
       #       axis = 'tblr')
  
  ## bottom panel: comparison of CTSS, and AI measures between observations 
  ## with and without GGO and reticulation
  
  suppl_figs$top_corr$bottom <- 
    list(x = comp_ct$box_plots[c("GGO_finding.CTSS", 
                                 "reticulation_finding.CTSS", 
                                 "GGO_finding.opacity_percent", 
                                 "reticulation_finding.opacity_percent")], 
         y =  c(rep('identity', 2), rep('sqrt', 2)), 
         z = list(c('steelblue2', 'steelblue4'), 
                  c('steelblue2', 'steelblue4'),
                  c('orangered2', 'orangered4'), 
                  c('orangered2', 'orangered4'))) %>% 
    pmap(function(x, y, z) x + 
           scale_y_continuous(trans = y) + 
           scale_fill_manual(values = z) + 
           theme(plot.title.position = 'plot')) %>% 
    plot_grid(plotlist = .,
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr')
  
  ## the entire figure
  
  suppl_figs$top_corr <- 
    plot_grid(suppl_figs$top_corr$upper, 
              ggdraw(), #suppl_figs$top_corr$middle, 
              suppl_figs$top_corr$bottom, 
              nrow = 3, 
              rel_heights = c(1.4, 
                              0.1, 
                              1), 
              labels = c('A', '', 'B'), #LETTERS, 
              label_size = 10) %>% 
    as_figure(label = 'top_variables_co_linearity', 
              ref_name = 'top_corr', 
              caption =paste('Co-linearity of the most influential computed', 
                             'tomography-related variables for prediction of', 
                             'insufficiency and percentage of reference of', 
                             'diffusion capacity for carbon monoxide.'), 
              w = 190, 
              h = 160)
    
# Saving figures on the disc --------
  
  insert_msg('Saving figures on the disc')
  
  suppl_figs %>% 
    number_figures(prefix = 'figure_s') %>%
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf',
         device = cairo_pdf)  
  
# END ------
  
  insert_tail()