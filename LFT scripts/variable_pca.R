# Principal component analysis for the top explanatory variables for predictions 
# of reduced DLCO and of DLCO percentage. 
# Pre-processing: median centering after conversion of factors to integers

  insert_head()
  
# container ------
  
  lft_pca <- list()
  
# variables of interest and analysis table --------
  
  insert_msg('Top variables')
  
  ## the top variables: top 15 most important variables (mean absolute SHAP)
  
  lft_pca$top_variables <- shap_imp$top_features %>% 
    map(reduce, union)
  
  lft_pca$analysis_tbl <- lft_pca$top_variables %>% 
    map(~select(lft_globals$analysis_tbl, all_of(.x))) %>%
    map(map_dfc, function(x) if(is.factor(x)) as.integer(x) else x) %>% 
    map(center_data, 'median') %>% 
    map(set_rownames, 
        paste0('obs_', 1:nrow(lft_globals$analysis_tbl)))
  
# PCA objects -------
  
  insert_msg('PCA objects')
  
  ## there are 2 - 3 components that explain the majority of the variance
  
  lft_pca$pca_obj <- lft_pca$analysis_tbl %>%
    map(reduce_data, 
        kdim = 6, 
        red_fun = 'pca')
  
# Plots --------
  
  insert_msg('Plots')

  ## scree plots 
  
  lft_pca$scree_plots <- 
    list(x = lft_pca$pca_obj) %>% 
    pmap(plot, 
         type = 'scree', 
         cust_theme = globals$common_theme) %>% 
    map(tag2subtitle)
  
  lft_pca$scree_plots <- 
    list(x = lft_pca$scree_plots, 
         y = paste('Top explanatory variables,', 
                   exchange(names(lft_pca$scree_plots), 
                            dict = lft_globals$lexicon))) %>% 
    pmap(function(x, y) x + 
           labs(title = y))
  
  ## plots of the loadings
  
  lft_pca$loading_plots <- 
    list(x = lft_pca$pca_obj, 
         plot_title = paste('Top explanatory variables,', 
                            exchange(names(lft_pca$scree_plots), 
                                     dict = lft_globals$lexicon)), 
         point_color = c('orangered3', 'orangered4')) %>% 
    pmap(plot_loadings, 
         n_top = 30)
  
# END ------
  
  lft_pca$analysis_tbl <- NULL
  
  lft_pca <- compact(lft_pca)
  
  insert_tail()
    