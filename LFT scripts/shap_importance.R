# Analysis of the SHAP importance metrics

  insert_head()
  
# container ------
  
  shap_imp <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
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
           scale_y_discrete(labels = function(x) exchange(x, dict = lft_globals$lexicon)) + 
           labs(title = y,
                subtitle = 'Importance for probability of DLCO < 80%') + 
           globals$common_theme)
  
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
           scale_y_discrete(labels = function(x) exchange(x, dict = lft_globals$lexicon)) + 
           labs(title = y,
                subtitle = 'Importance for DLCO, % of reference') + 
           globals$common_theme)
  
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
           scale_y_discrete(labels = function(x) exchange(x, dict = lft_globals$lexicon)) + 
           labs(title = y,
                subtitle = 'Importance for probability of DLCO < 80%') + 
           globals$common_theme)
  
  ## regression responses
  
  shap_imp$bee_plots$DLCO_percent <- 
    list(x = shap_imp$shap_viz_objects$DLCO_percent, 
         y = paste('DLCO,', 
                   globals$algo_labs[names(shap_imp$shap_viz_objects$DLCO_percent)])) %>% 
    pmap(function(x, y, z) sv_importance(x, 
                                         kind = 'bee', 
                                         size = 2, 
                                         alpha = 0.5) + 
           scale_y_discrete(labels = function(x) exchange(x, dict = lft_globals$lexicon)) + 
           labs(title = y,
                subtitle = 'Importance for DLCO, % of reference') + 
           globals$common_theme)
  
  ## common fill scale
  
  shap_imp$bee_plots <- shap_imp$bee_plots %>% 
    map(map, 
        ~.x + 
          scale_color_gradient2(low = 'steelblue', 
                                mid = 'black', 
                                high = 'firebrick', 
                                midpoint = 0.5))
  
# END -----
  
  insert_tail()