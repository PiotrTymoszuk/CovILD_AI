# Principal component analysis for the top explanatory variables for predictions 
# of reduced DLCO and of DLCO percentage. 
# Pre-processing: median centering after conversion of factors to integers

  insert_head()
  
# container ------
  
  lft_net <- list()
  
# variables of interest and analysis table --------
  
  insert_msg('Top variables')
  
  ## the top variables: top 15 most important variables (mean absolute SHAP)
  
  lft_net$top_variables <- shap_imp$top_features %>% 
    map(reduce, union)
  
  lft_net$top_variables$all <- reduce(lft_net$top_variables, union)
  
  lft_net$analysis_tbl <- lft_net$top_variables %>% 
    map(~select(lft_globals$analysis_tbl, all_of(.x))) %>%
    map(map_dfc, function(x) if(is.factor(x)) as.integer(x) else x) %>% 
    map(set_rownames, 
        paste0('obs_', 1:nrow(lft_globals$analysis_tbl)))
  
  ## names of the graphs
  
  lft_net$graph_titles <- 
    paste('Top explanatory factors,', 
          c(exchange(names(lft_net$analysis_tbl)[1:2], 
                     lft_globals$lexicon), 
            'DLCO < 80% and DLCO'))
  
# Correlation graphs --------
  
  insert_msg('Correlation graphs')
  
  lft_net$graph_obj <- lft_net$analysis_tbl %>% 
    map(as_iGraph, 
        fun = cor, 
        method = 'kendall', 
        cutoff = 0.3, 
        diag = FALSE, 
        weighted = TRUE) %>% 
    map(prune_degree, cutoff = 0)
  
  ## setting the attributes: 
  ## reader-friendly labels of the vertices, and 
  ## classification of the explanatory factors
  
  lft_net$attr_tbl <- lft_net$graph_obj %>% 
    map(get_vertex_attributes) %>% 
    map(mutate, 
        variable_label = exchange(name, lft_globals$lexicon), 
        variable_class = exchange(name, lft_globals$lexicon, value = 'class')) %>% 
    map(select, -index)

  lft_net$graph_obj <- 
    map2(lft_net$graph_obj, 
         lft_net$attr_tbl, 
         set_vertex_attributes)
  
# Fruchterman-Reingold network plots --------
  
  insert_msg('Network plots')
  
  ## vertex color codes for assignment of the explanatory variables 
  ## to their classes
  
  lft_net$plots <- 
    list(x = lft_net$graph_obj, 
         plot_title = lft_net$graph_titles) %>% 
    pmap(plot, 
         layout = layout.fruchterman.reingold, 
         vertex_fill_variable = 'variable_class', 
         vertex_color_variable = 'variable_class', 
         weighting_order = 1, 
         vertex_size = 2, 
         label_vertices = TRUE, 
         vertex_label_variable = 'variable_label', 
         vertex_txt_color_variable = 'variable_class', 
         vertex_txt_size = 2.5, 
         cust_theme = theme_void() + 
           theme(plot.title = element_text(size = 8, face = 'bold'), 
                 plot.subtitle = globals$common_text, 
                 legend.text = globals$common_text, 
                 legend.title = globals$common_text, 
                 plot.margin = globals$common_margin), 
         show.legend = FALSE, 
         seed = 12345)
  
  ## additional styling by setting color and line width scales
  
  lft_net$plots <- lft_net$plots %>% 
    map(~.x + 
          scale_fill_manual(values = lft_globals$class_colors, 
                            name = 'variable\nclassifcation', 
                            drop = FALSE) + 
          scale_color_manual(values = lft_globals$class_colors, 
                             name = 'variable\nclassifcation', 
                             drop = FALSE) + 
          scale_linewidth(range = c(0.25, 1.5), 
                          limits = c(0.3, 1), 
                          name = "Kendall's \u03C4") + 
          scale_alpha_continuous(range = c(0.15, 0.6), 
                                 limits = c(0.3, 1), 
                                 name = "Kendall's \u03C4"))
  
# END ------
  
  lft_net$analysis_tbl <- NULL
  
  lft_net <- compact(lft_net)
  
  insert_tail()
    