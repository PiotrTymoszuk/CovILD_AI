# Correspondence analysis of co-occurrence 
# of the most frequent CT, LFT findings and symptoms of relevance for
# lung function

  insert_head()
  
# container ------
  
  mca_lft <- list()
  
# analysis data -------
  
  insert_msg('Analysis data')
  
  mca_lft$analysis_tbl$ct <- covild$ct %>% 
    select(GGO_finding, reticulation_finding, consolidation_finding) %>% 
    set_names(c('GGO', 
                'reticulation', 
                'consolidation'))
  
  mca_lft$analysis_tbl$lft <- covild$lft %>% 
    select(DLCO_reduced, FVC_reduced, FEV1_reduced) %>% 
    set_names(c('DLCO < 80%', 
                'FVC < 80%', 
                'FEV1 < 80%'))
  
  mca_lft$analysis_tbl$symptoms <- covild$symptoms %>% 
    select(dyspnea_symptom, cough_symptom, impaired_performance_symptom) %>% 
    set_names(c('dyspnea', 
                'cough', 
                'impaired performance'))
  
  ## plot titles
  
  mca_lft$plot_titles <- 
    c('Co-occurrence of CT findings', 
      'Co-occurrence of LFT findings', 
      'Co-occurrence of symptoms') %>% 
    set_names(names(mca_lft$analysis_tbl))
  
# Correspondence analysis -------
  
  insert_msg('Correspondence analysis')
  
  ## the genuine MASS mca object
  
  mca_lft$test <- mca_lft$analysis_tbl %>% 
    map(MASS::mca)

  ## column factors to a red_analysis object
  ## which will be used later for plotting
  
  for(i in names(mca_lft$test)) {
    
    mca_lft$red_object[[i]] <- mca_lft$test[[i]][c("rs", "cs")] %>% 
      map(as.data.frame) %>% 
      map(set_names, c('comp_1', 'comp_2')) %>% 
      map2(., c('observation', 'variable'), rownames_to_column) %>% 
      set_names(c('component_tbl', 'loadings')) %>% 
      c(list(red_obj = mca_lft$test[[i]], 
             red_fun = 'mca', 
             dist_method = 'custom', 
             data = quo(mca_lft$analysis_tbl[[i]]))) %>% 
      red_analysis
    
  }
  
# Plotting the column factors -------
  
  insert_msg('Plotting the column factors')
  
  for(i in names(mca_lft$red_object)) {
    
    mca_lft$column_plot[[i]] <- mca_lft$red_object[[i]] %>% 
      plot('loadings', 
           cust_theme = globals$common_theme, 
           txt_type = 'text') + 
      geom_hline(yintercept = 0, 
                 linetype = 'dashed') + 
      geom_vline(xintercept = 0,
                 linetype = 'dashed') + 
      labs(title = mca_lft$plot_titles[[i]], 
           subtitle = '2D correspondence analysis, column factors', 
           x = paste('Dimension 1, ', signif(mca_lft$test[[i]]$d[1]/2 * 100, 2), '%'), 
           y = paste('Dimension 2, ', signif(mca_lft$test[[i]]$d[2]/2 * 100, 2), '%')) + 
      theme(plot.tag = element_blank())
    
  }
  
# Venn plot ------
  
  insert_msg('Venn plot')
  
  mca_lft$venn_plot <- 
    list(x = mca_lft$analysis_tbl, 
         y = mca_lft$plot_titles) %>% 
    pmap(function(x, y) x %>% 
           map_dfc(~.x == 'yes') %>% 
           ggvenn(fill_color = c('coral3', 'steelblue', 'darkolivegreen'), 
                  set_name_size = 2.75, 
                  text_size = 2.75, 
                  show_percentage = FALSE, 
                  stroke_size = 0.5) + 
           labs(title = y, 
                subtitle = paste('total observations: n =', 
                                 nrow(x))) + 
           theme(plot.title = globals$common_text, 
                 plot.subtitle = globals$common_text) + 
           theme(plot.title = element_text(face = 'bold')))

# END ------
  
  rm(i)
  
  insert_tail()