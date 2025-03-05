# Correlation of numeric LFT readouts and CT readouts. Because the observations 
# are not independent, we're resorting to a blocked bootstrap Spearman's test.

  insert_head()
  
# container -------
  
  corr_lft <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis data ------
  
  insert_msg('Analysis data')
  
  corr_lft$analysis_tbl$lft <- covild$lft %>% 
    select(ID, DLCO_percent, FVC_percent, FEV1_percent)
  
  corr_lft$analysis_tbl$ct <- covild$ct %>% 
    select(ID, CTSS, opacity_percent, high_opacity_percent)
  
  ## variable lexicon 
  
  corr_lft$lexicon <- covild[c("lft_lexicon", "ct_lexicon")] %>% 
    map_dfr(filter, 
            variable %in% c('DLCO_percent', 'FVC_percent', 'FEV1_percent', 
                            'CTSS', 'opacity_percent', 'high_opacity_percent'))

  ## variable pairs and plot titles
  
  corr_lft$pairs <- corr_lft$analysis_tbl %>% 
    map(names) %>% 
    map(~.x[-1]) %>% 
    map(combn, 
        m = 2, 
        simplify = FALSE)

  corr_lft$titles <- corr_lft$pairs %>% 
    map(map, exchange, corr_lft$lexicon) %>% 
    map(map, paste, collapse = ' and ') %>% 
    map(map, capitalize_first_char)
  
# Correlation coefficients ---------
  
  insert_msg('Correlation coefficients')
  
  for(i in names(corr_lft$pairs)) {
    
    corr_lft$test[[i]] <- corr_lft$pairs[[i]] %>% 
      map_dfr(corr_test, 
              data = corr_lft$analysis_tbl[[i]], 
              B = 2000, 
              positive = TRUE, 
              method = 'spearman') %>% 
      re_adjust %>% 
      mutate(est_lab = paste0('\u03C1 = ', signif(rho, 2), 
                              ' [', signif(lower_ci, 2), 
                              ' - ', signif(upper_ci, 2), ']'), 
             n = nrow(corr_lft$analysis_tbl[[i]]), 
             plot_cap = paste(est_lab, significance, sep = ', '), 
             plot_cap = paste(plot_cap, n, sep = ', n = '))
    
  }
  
# Scatter plots --------
  
  insert_msg('Scatter plots')
  
  for(i in names(corr_lft$pairs)) {
    
    corr_lft$plots[[i]] <- 
      list(variables =  corr_lft$pairs[[i]], 
           plot_title = corr_lft$titles[[i]], 
           plot_subtitle = corr_lft$test[[i]]$plot_cap, 
           x_lab = corr_lft$test[[i]]$variable1 %>% 
             exchange(corr_lft$lexicon) %>% 
             paste0(', rank'), 
           y_lab = corr_lft$test[[i]]$variable2 %>% 
             exchange(corr_lft$lexicon) %>% 
             paste0(', rank')) %>% 
      pmap(plot_correlation, 
           data = corr_lft$analysis_tbl[[i]][, -1] %>% 
             map_dfc(rank), 
           cust_theme = globals$common_theme + 
             theme(plot.tag = element_blank()), 
           point_hjitter = 0, 
           point_wjitter = 0) %>% 
      set_names(corr_lft$titles[[i]])
    
  }
  
# END ------
  
  rm(i)
  
  insert_tail()