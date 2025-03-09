# Main figures for the manuscript

  insert_head()
  
# container ------
  
  paper_figs <- list()
  
# Figure globals --------
  
  insert_msg('Figure globals')
  
  dlco_range <- range(lft_globals$analysis_tbl$DLCO_percent)
  
# Figure 1: analysis inclusion scheme -------
  
  insert_msg('Figure 1: analysis inclusion scheme')
  
  paper_figs$consort <- 
    plot_grid(ggdraw() + 
                draw_image('./aux files/covild_consort.png')) %>% 
    as_figure(label = 'figure_1_analysis_inclusion_scheme', 
              ref_name = 'consort', 
              caption = 'Analysis inclusion scheme.', 
              w = 180, 
              h = 2480/2950 * 180)
  
# Figure 2: model performance ---------
  
  insert_msg('Figure 2: model performance')
  
  paper_figs$ml_performance <- 
    c(list(bin_models$performance_plots$DLCO_reduced, 
           bin_models$roc_plots$DLCO_reduced$cv, 
           reg_models$performance_plots$DLCO_percent), 
      reg_models$scatter_plots$DLCO_percent$cv[c("gbm", 
                                                 "ranger", 
                                                 "svmRadial")] %>% 
        map(~.x + 
              scale_x_continuous(limits = dlco_range) + 
              scale_y_continuous(limits = dlco_range))) %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                legend.title = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         'B', '', 
                         '', ''), 
              label_size = 10) %>% 
    plot_grid(get_legend(bin_models$performance_plots$DLCO_reduced + 
                           guides(shape = 'none', 
                                  size = 'none') + 
                           theme(legend.position = 'bottom', 
                                 legend.title = element_blank())), 
              nrow = 2, 
              rel_heights = c(0.94, 0.06))
  
  paper_figs$ml_performance <- paper_figs$ml_performance %>% 
    as_figure(label = 'figure_2_machine_learning_performance', 
              ref_name = 'ml_performance', 
              caption = paste('Evaluation of performance of machine learning', 
                              'models of diffusion capacity for CO during', 
                              'COVID-19 convalescence.'), 
              w = 180, 
              h = 230)
  
# Figure 3: variable importance, reduced DLCO --------
  
  insert_msg('Figure 3: Variable importance, reduced DLCO')
  
  paper_figs$varimp_dlco_red <- 
    shap_imp$violin_bee_plots$DLCO_reduced %>% 
    map2(., names(.), 
         ~.x + 
           guides(x = guide_axis(angle = 45)) + 
           labs(title = globals$algo_labs[.y]) + 
           theme(legend.position = 'none', 
                 plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(shap_imp$violin_bee_plots$DLCO_reduced[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure(label = 'figure_3_dlco_reduced_shap_importance', 
              ref_name = 'varimp_dlco_red',
              caption = paste('Explanatory variable importance for models', 
                              'of insufficient diffusion capacity for', 
                              'carbon monoxide measured by', 
                              'Shapley additive explanations.'), 
              w = 180, 
              h = 210)
  
# Figure 4: detection of insufficient DLCO with numeric CT parameters --------
  
  insert_msg('Figure 4: detection of DLCO insufficiency with CT parameters')
  
  ## upper panel: box plots
  
  paper_figs$dlco_red_detection$upper <- 
    list(x = lft_uniboot$box_plots[c("DLCO_reduced.CTSS", 
                                     "DLCO_reduced.opacity_percent", 
                                     "DLCO_reduced.high_opacity_percent")], 
         y = c('identity', 'sqrt', 'sqrt'), 
         z = list(c('steelblue2', 'steelblue4'),
                  c('orangered2', 'orangered4'), 
                  c('gray80', 'gray30'))) %>% 
    pmap(function(x, y, z) x + 
           scale_y_continuous(trans = y) + 
           scale_fill_manual(values = z) + 
           theme(legend.position = 'none') + 
           labs(x = 'DLCO < 80%')) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  ## bottom panel: ROC plots
  
  paper_figs$dlco_red_detection$bottom <- 
    lft_roc$roc_plots$DLCO_reduced[c("CTSS", 
                                     "opacity_percent", 
                                     "high_opacity_percent")] %>% 
    map(~.x + 
          theme(legend.position = 'none', 
                plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') 
  
  ## the entire figure
  
  paper_figs$dlco_red_detection <- 
    plot_grid(paper_figs$dlco_red_detection$upper, 
              paper_figs$dlco_red_detection$bottom, 
              nrow = 2, 
              rel_heights = c(1, 2), 
              labels = LETTERS, 
              label_size = 10) %>%  
    as_figure(label = 'figure_4_dlco_insufficiency_ct_parameters', 
              ref_name = 'dlco_red_detection', 
              caption = paste('Detection of DLCO insufficiency by', 
                              'human- and artificial intelligence-determined', 
                              'CT readouts of severity of structural lung', 
                              'damage.'), 
              w = 180, 
              h = 210)
  
# Saving figures on the disc --------
  
  insert_msg('Saving figures on the disc')
  
  paper_figs %>% 
    walk(pickle, 
         path = './paper/figures', 
         format = 'pdf',
         device = cairo_pdf)
  
# END ------
  
  rm(dlco_range)
  
  insert_tail()