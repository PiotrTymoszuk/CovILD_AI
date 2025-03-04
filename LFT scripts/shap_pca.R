# Principal component analysis for SHAP importance measures of 
# the explanatory factors for numeric DLCO and DLCO. 
# For the sake of simplicity, we're analysing just the top 15 most important 
# features as gauged by mean absolute SHAP.
# Pre-processing 

  insert_head()
  
# container -------
  
  shap_pca <- list()
  
# analysis globals --------
  
  insert_msg('Analysis globals')
  
  ## the top most influential explanatory factors
  
  shap_pca$top_features <- shap_imp$top_features
  
  ## analysis data: SHAP values for the variables and observations, 
  ## row names set for smooth identification of observations 
  ## by tools for `clust_tools`. The data are median centered
  
  shap_pca$analysis_tbl <- shap_imp$shap_objects %>% 
    map(map, ~.x$S) %>% 
    map(map, as.data.frame) %>% 
    map(map, 
        ~set_rownames(.x, paste0('obs_', 1:nrow(.x)))) %>% 
    map(map, center_data, type = 'median')
  
  shap_pca$analysis_tbl <- 
    map2(shap_pca$analysis_tbl, 
         shap_pca$top_features, 
         function(x, y) map2(x, y, 
                             ~select(.x, all_of(.y))))
  
# PCA objects -------
  
  insert_msg('PCA objects')
  
  ## predictions of DLCO findings: 2 - 3 components that explain the majority
  ## of the variance. 
  ## analogically 3 - 4 principal components for SHAPs for prediction of 
  ## DLCO percentages

  shap_pca$pca_obj <- shap_pca$analysis_tbl %>%
    map(map, 
        reduce_data, 
        kdim = 6, 
        red_fun = 'pca')

# Plots for the PCA objects -------
  
  insert_msg('Plots for the PCA objects')
  
  ## scree plots
  
  for(i in names(shap_pca$pca_obj)) {
    
    shap_pca$scree_plots[[i]] <- 
      list(x = shap_pca$pca_obj[[i]]) %>% 
      pmap(plot, 
           type = 'scree', 
           cust_theme = globals$common_theme) %>% 
      map(tag2subtitle)
    
    ## styling
    
    shap_pca$scree_plots[[i]] <- 
      list(x = shap_pca$scree_plots[[i]], 
           y = globals$algo_labs[names(shap_pca$pca_obj[[i]])]) %>% 
      pmap(function(x, y) x + 
             labs(title = y))
    
  }
  
  ## plots of the loadings 
  
  for(i in names(shap_pca$pca_obj)) {
    
    shap_pca$loading_plots[[i]] <- 
      list(x = shap_pca$pca_obj[[i]], 
           plot_title = globals$algo_labs[names(shap_pca$pca_obj[[i]])], 
           point_color = globals$algo_colors[names(shap_pca$pca_obj[[i]])]) %>% 
      pmap(plot_loadings, 
           n_top = 15)
    
  }
  
# END ------
  
  shap_pca$analysis_tbl <- NULL
  
  shap_pca <- compact(shap_pca)
  
  insert_tail()