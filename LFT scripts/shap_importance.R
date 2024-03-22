# Analysis of the SHAP importance metrics

  insert_head()
  
# container ------
  
  shap_imp <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variable lexicon 
  
  shap_imp$lexicon <- lft_globals$lexicon %>% 
    mutate(label = ifelse(!is.na(unit), 
                          paste(label, unit, sep = ', '), 
                          label), 
           label = ifelse(variable %in% lft_globals$ct_variables & 
                            variable != 'follow_up', 
                          paste0('<b>', label, '</b>'), 
                          label))  
  
# SHAP objects ------
  
  insert_msg('SHAP objects')
  
  shap_imp$shap_objects <- shap_devel$objects[c("DLCO_reduced", "DLCO_percent")]
  
  shap_imp$shap_viz_objects <- shap_imp$shap_objects %>% 
    map(map, shapviz)

# Bar plots with the mean importance measures -------  
  
  insert_msg('Bar plots')
  
  ## binary responses
  
  shap_imp$bar_plots$DLCO_reduced <- 
    list(x = shap_imp$shap_viz_objects$DLCO_reduced, 
         y = paste('DLCO < 80%,', 
                   globals$algo_labs[names(shap_imp$shap_viz_objects$DLCO_reduced)]), 
         z = globals$algo_colors[names(shap_imp$shap_viz_objects$DLCO_reduced)]) %>% 
    pmap(function(x, y, z) sv_importance(x, 
                                         kind = 'bar', 
                                         fill = z, 
                                         color = 'black') + 
           labs(title = y,
                subtitle = 'Importance for probability of DLCO < 80%'))
  
  ## regression
  
  shap_imp$bar_plots$DLCO_percent <- 
    list(x = shap_imp$shap_viz_objects$DLCO_percent, 
         y = paste('DLCO,', 
                   globals$algo_labs[names(shap_imp$shap_viz_objects$DLCO_percent)]), 
         z = globals$algo_colors[names(shap_imp$shap_viz_objects$DLCO_percent)]) %>% 
    pmap(function(x, y, z) sv_importance(x, 
                                         kind = 'bar', 
                                         fill = z, 
                                         color = 'black') + 
           labs(title = y,
                subtitle = 'Importance for DLCO, % of reference'))
  
  ## common styling
  
  shap_imp$bar_plots <- shap_imp$bar_plots %>% 
    map(map, 
        ~.x + 
          scale_y_discrete(labels = function(x) exchange(x, dict = shap_imp$lexicon)) + 
          globals$common_theme + 
          theme(axis.text.y = element_markdown()))
  
  
# Bee swarm plots ------
  
  insert_msg('Bee sharm plots')
  
  ## bee swarm plots of the SHAP values
  ## Note: in case of the binary response, the importance refers to the
  ## probability of impaired DLCO!!!
  
  ## binary responses
  
  shap_imp$bee_plots$DLCO_reduced <- 
    list(x = shap_imp$shap_viz_objects$DLCO_reduced, 
         y = paste('DLCO < 80%,', 
                   globals$algo_labs[names(shap_imp$shap_viz_objects$DLCO_reduced)])) %>% 
    pmap(function(x, y, z) sv_importance(x, 
                                         kind = 'bee', 
                                         size = 2, 
                                         alpha = 0.5) + 
           labs(title = y,
                subtitle = 'Importance for probability of DLCO < 80%'))
  
  ## regression responses
  
  shap_imp$bee_plots$DLCO_percent <- 
    list(x = shap_imp$shap_viz_objects$DLCO_percent, 
         y = paste('DLCO,', 
                   globals$algo_labs[names(shap_imp$shap_viz_objects$DLCO_percent)])) %>% 
    pmap(function(x, y, z) sv_importance(x, 
                                         kind = 'bee', 
                                         size = 2, 
                                         alpha = 0.5) + 
           labs(title = y,
                subtitle = 'Importance for DLCO, % of reference'))
  
  ## common fill scale and styling
  
  shap_imp$bee_plots <- shap_imp$bee_plots %>% 
    map(map, 
        ~.x +
          scale_y_discrete(labels = function(x) exchange(x, dict = shap_imp$lexicon)) + 
          scale_color_gradient2(low = 'steelblue', 
                                mid = 'black', 
                                high = 'firebrick', 
                                midpoint = 0.5) + 
          globals$common_theme + 
          theme(axis.text.y = element_markdown()))
  
# A table with mean SHAP values ---------
  
  insert_msg('A table with mean absolute SHAP values')
  
  shap_imp$mean_table <- shap_imp$bar_plots %>% 
    map(map, ~.x$data) %>% 
    map(~map2(., names(.), 
              ~set_names(.x, c('Variable', globals$algo_labs[.y])))) %>% 
    map(reduce, full_join, by = 'Variable') %>% 
    set_names(c('DLCO < 80%', 'DLCO')) %>% 
    compress(names_to = 'Response') %>% 
    relocate(Response) %>% 
    as_tibble
  
  shap_imp$mean_table <- shap_imp$mean_table %>% 
    mutate(Variable = exchange(Variable, shap_imp$lexicon))
  
# Customized bee swarm plots --------
  
  insert_msg('Customized bee swarm plots')
  
  shap_imp$violin_bee_plots$DLCO_reduced <- 
    shap_imp$bee_plots$DLCO_reduced %>% 
    map(my_shap_bee, 
        reorder_ft = 'median', 
        midpoint = 0.5)
  
  shap_imp$violin_bee_plots$DLCO_percent <- 
    shap_imp$bee_plots$DLCO_percent %>% 
    map(my_shap_bee, 
        reorder_ft = 'median', 
        midpoint = 0.5, 
        point_wjitter = 0.05)
  
  shap_imp$violin_bee_plots <- shap_imp$violin_bee_plots %>% 
    map(map, ~.x + theme(axis.text.y = element_markdown()))

# END -----

  insert_tail()