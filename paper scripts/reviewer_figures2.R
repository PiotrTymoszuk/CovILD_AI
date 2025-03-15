# Figures for Reviewer 1 in the second rounf od revision

  insert_head()
  
# container ------
  
  rev2_figs <- list()
  
# Tuning of classification and regression models of FVC and FEV -------
  
  insert_msg('Tuning process of models of FVC and FEV')
  
  rev2_figs[c('class_fvc_tune', 
              'class_fev1_tune',
              'reg_fvc_tune', 
              'reg_fev1_tune')] <- 
    list(fvc_red_tune, fev1_red_tune, fvc_tune, fev1_tune) %>% 
    map(~.$tuning_plots) %>% 
    map(~plot_grid(.x$nnet + 
                     labs(title = paste('Tuning:', globals$algo_labs["nnet"])) + 
                     facet_grid(.~ size) + 
                     guides(x = guide_axis(angle = 45)) + 
                     theme(legend.position = 'none', 
                           axis.text.x = element_text(size = 7)) + 
                     scale_color_manual(values = rep('steelblue4', 10)), 
                   plot_grid(.x$ranger + 
                               labs(title = paste('Tuning:', globals$algo_labs["ranger"])) + 
                               theme(legend.position = 'bottom'), 
                             .x$svmRadial + 
                               labs(title = paste('Tuning:', globals$algo_labs["svmRadial"])), 
                             ncol = 2, 
                             rel_widths = c(2, 1), 
                             align = 'hv', 
                             axis = 'tblr'),
                   .x$gbm + 
                     labs(title = paste('Tuning:', globals$algo_labs["gbm"])) + 
                     theme(legend.position = 'bottom'), 
                   nrow = 3))
  
  rev2_figs[c('class_fvc_tune', 
              'class_fev1_tune',
              'reg_fvc_tune', 
              'reg_fev1_tune')]  <- 
    rev2_figs[c('class_fvc_tune', 
                'class_fev1_tune',
                'reg_fvc_tune', 
                'reg_fev1_tune')] %>% 
    list(label = c('tuning_classification_reduced_fvc', 
                   'tuning_classification_reduced_fev1', 
                   'tuning_regression_fvc',  
                   'tuning_regression_fev1'),
         ref_name = names(.),
         caption = paste('Choice of hyper-parameters for machine learning', 
                         c(rep('classification', 2), rep('regression', 2)), 
                         'models of', 
                         c('reduced forced vital capacity.', 
                           'reduced forced expiratory volume in one second.', 
                           'forced vital capacity.', 
                           'forced expiratory volume in one second.'))) %>% 
    pmap(as_figure,
         w = 190, 
         h = 230)
  
# Saving the reviewer figures on the disc -------
  
  insert_msg('Saving the reviewer figures on the disc')
  
  rev2_figs %>% 
    number_figures %>% 
    walk(pickle, 
         path = './paper/reviewer figures 2', 
         device = cairo_pdf)
  
# END ------
  
  insert_tail()