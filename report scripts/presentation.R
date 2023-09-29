# Figures for the OERG presentation

  insert_head()
  
# container ------
  
  conf_figures <- list()
  
# Figure 1 and 2: performance of the DLCO and FEV1 classifiers -------
  
  insert_msg('Figures 1 and 2: performance of the classifiers')
  
  conf_figures[c('dlco_reg_perf', 'fev1_red_perf')] <- 
    bin_models$performance_plots[c("DLCO_reduced", "FEV1_reduced")] %>% 
    map(~.x + 
          globals$presentation_theme + 
          theme(legend.position = 'none')) %>% 
    map(~plot_grid(.x, 
                   get_legend(bin_models$performance_plots$DLCO_reduced + 
                                globals$presentation_theme + 
                                theme(legend.position = 'right', 
                                      legend.box = 'vertical', 
                                      legend.box.just = 'left', 
                                      legend.margin = ggplot2::margin())), 
                   ncol = 2, 
                   rel_widths = c(1, 0.4))) %>% 
    list(x = ., 
         label = c('figure_1_dlco_reduced_model_performance', 
                   'figure_2_fev1_reduced_model_performance'), 
         ref_name = names(.)) %>% 
    pmap(as_figure, 
         w = 180, 
         h = 120)
  
# Figure 3 and 4: ROC curves --------
  
  insert_msg('Figures 3 and 4: ROC curves')
  
  conf_figures[c('dlco_red_roc', 'fev1_red_roc')] <- 
    bin_models$roc_plots[c("DLCO_reduced", "FEV1_reduced")] %>% 
    map(map, 
        ~.x + 
          globals$presentation_theme + 
          theme(legend.position = 'none')) %>% 
    map(~plot_grid(plotlist = ., 
                   ncol = 2, 
                   align = 'hv')) %>% 
    map(~plot_grid(.x, 
                   get_legend(bin_models$roc_plots$DLCO_reduced[[1]] + 
                                globals$presentation_theme + 
                                theme(legend.position = 'bottom')), 
                   nrow = 2, 
                   rel_heights = c(1, 0.2)))
  
  conf_figures[c('dlco_red_roc', 'fev1_red_roc')] <- 
    conf_figures[c('dlco_red_roc', 'fev1_red_roc')] %>% 
    list(x = ., 
         label = c('figure_3_dlco_reduced_roc', 
                   'figure_4_fev1_reduced_roc'), 
         ref_name = names(.)) %>% 
    pmap(as_figure, 
         w = 230, 
         h = 140)
  
# Figure 5 and 6: SHAP for DLCO -------
  
  insert_msg('Figures 5 and 6: SHAP for DLCO')
  
  conf_figures[c('dlco_shap1', 'dlco_shap2')] <- 
    list(shap_imp$bar_plots$DLCO_reduced[c("ranger", "gbm")], 
         shap_imp$bar_plots$DLCO_reduced[c("nnet", "svmRadial")]) %>% 
    map(map, ~.x + theme(plot.subtitle = element_blank())) %>% 
    map(~plot_grid(plotlist = ., 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr'))
  
  conf_figures[c('dlco_shap1', 'dlco_shap2')] <- 
    conf_figures[c('dlco_shap1', 'dlco_shap2')] %>% 
    list(x = ., 
         label = c('figure_5_shap_dlco1', 
                   'figure_6_shap_dlco2'), 
         ref_name = names(.)) %>% 
    pmap(as_figure, 
         w = 245, 
         h = 130)
  
# saving the figures on the disc ------
  
  insert_msg('Saving the figures on the disc')
  
  conf_figures %>% 
    walk(pickle, 
         format = 'png', 
         path = './report/oerg figures', 
         device = png)
  
# END -----
  
  insert_tail()