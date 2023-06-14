# Optimal cutpoints for opacity, high opacity and CTSS
# for detection of any CT abnormalities, reduced DLCO, 
# reduced FVC and reduced FEV1
# 
# The optimal cutoff is determined by the Youden criterion

  insert_head()
  
# container -------
  
  lft_roc <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## responses and marker variables
  
  lft_roc$variables <- 
    c('opacity_percent', 'high_opacity_percent', 'CTSS')
  
  lft_roc$variables <- 
    set_names(lft_roc$variables, 
              lft_roc$variables)
  
  lft_roc$responses <- 
    c('LFT_findings', 
      'DLCO_reduced', 
      'FVC_reduced', 
      'FEV1_reduced')
  
  lft_roc$responses <- 
    set_names(lft_roc$responses, 
              lft_roc$responses)
  
  ## analysis table
  
  lft_roc$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(all_of(lft_roc$responses), 
           all_of(lft_roc$variables)) 
  
  ## marker colors
  
  lft_roc$variable_colors <- 
    c('opacity_percent' = 'orangered3', 
      'high_opacity_percent' = 'gray30', 
      'CTSS' = 'steelblue')
  
# N numbers ------
  
  insert_msg('N numbers')
  
  lft_roc$n_numbers <- lft_roc$responses %>% 
    map(~count(lft_roc$analysis_tbl, .data[[.x]]))
  
  lft_roc$n_caps <- lft_roc$n_numbers %>% 
    map(~paste0('total: n = ', sum(.x$n), 
                ', findings: n = ', .x$n[2]))
  
# Optimal cutpoint objects ------
  
  insert_msg('Optimal cutpoint objects')
  
  lft_roc$cut_obj <- lft_roc$responses %>% 
    map(function(resp) lft_roc$variables %>% 
          map(~optimal.cutpoints(.x, 
                                 status = resp, 
                                 tag.healthy = 'no', 
                                 methods = 'Youden', 
                                 data = lft_roc$analysis_tbl)))
  
# Cutoff stats -----
  
  insert_msg('Cutoff stats')
  
  ## for all possible cutoffs
  
  lft_roc$stats$all_cutoffs <- lft_roc$cut_obj %>% 
    map(map, extract_cut) %>% 
    map(map, ~.x$all_cutoffs)
  
  ## and for the optimal ones
  
  lft_roc$stats$optimal_cutoff <- lft_roc$cut_obj %>% 
    map(map, extract_cut) %>% 
    map(map, ~.x$optimal_cutoff) %>% 
    map(compress, names_to = 'variable') %>% 
    map(mutate, 
        variable = factor(variable, lft_roc$variables), 
        auc_lab = paste0('AUC = ', signif(auc, 2), 
                         ' [', signif(lower_ci, 3), 
                         ' - ', signif(upper_ci, 2), ']'), 
        plot_lab = paste0('Se = ', signif(Se, 2), 
                          '\nSp = ', signif(Sp, 2), 
                          '\n', auc_lab)) %>% 
    map2(., names(.), 
         ~mutate(.x, response = factor(.y, lft_roc$responses)))
  
# Plots for all possible cutoffs -------
  
  insert_msg('Plots for all cutoffs')
  
  for(i in names(lft_roc$stats$all_cutoffs)) {
    
    lft_roc$all_plots[[i]] <- 
      list(data = lft_roc$stats$all_cutoffs[[i]], 
           plot_title = names(lft_roc$stats$all_cutoffs[[i]]) %>% 
             exchange(lft_globals$lexicon) %>% 
             capitalize_first_char %>% 
             paste(exchange(i, lft_globals$lexicon), sep = ' and '), 
           opt_cutoff = lft_roc$stats$optimal_cutoff[[i]]$cutoff, 
           x_lab = names(lft_roc$stats$all_cutoffs[[i]]) %>% 
             exchange(lft_globals$lexicon, 
                      value = 'table_label')) %>% 
      pmap(plot_cut_search, 
           x_trans = 'log')
    
  }
  
# ROC plots ------
  
  insert_msg('ROC plots')
  
  for(i in names(lft_roc$stats$all_cutoffs)) {
    
    lft_roc$roc_plots[[i]] <- 
      list(cutoff_stats = blast(lft_roc$stats$optimal_cutoff[[i]], variable), 
           plot_title = names(lft_roc$stats$all_cutoffs[[i]]) %>% 
             exchange(lft_globals$lexicon) %>% 
             capitalize_first_char %>% 
             paste(exchange(i, lft_globals$lexicon), sep = ' and '), 
           plot_subtitle = lft_roc$n_caps[[i]], 
           m_variable = as.character(lft_roc$stats$optimal_cutoff[[i]]$variable), 
           color = lft_roc$variable_colors) %>% 
      pmap(plot_roc,
           data = lft_roc$analysis_tbl %>% 
             map_dfc(function(x) if(is.factor(x)) as.numeric(x) - 1 else x), 
           d_variable = i)
    
  }
  
# Summary ROC plots for aeach LFT abnormality ------
  
  insert_msg('Summary ROC plots')
  
  ## color scale labels
  
  lft_roc$summary_roc_plots$labels <- lft_roc$stats$optimal_cutoff %>% 
    map(mutate, 
        var_lab = exchange(as.character(variable), dict = lft_globals$lexicon), 
        auc_lab = paste(var_lab, auc_lab, sep = ', '), 
        auc_lab = stri_replace(auc_lab, fixed = 'AUC = ', replacement = ''))
  
  lft_roc$summary_roc_plots$labels <- lft_roc$summary_roc_plots$labels %>%
    map(~set_names(.x$auc_lab, 
                   .x$variable))
  
  ## plotting data
  
  lft_roc$summary_roc_plots$data <- lft_roc$analysis_tbl %>% 
    map_dfc(function(x) if(is.factor(x)) as.numeric(x) - 1 else x) %>% 
    pivot_longer(cols = all_of(unname(lft_roc$variables)), 
                 names_to = 'variable', 
                 values_to = 'value') %>% 
    mutate(variable = factor(variable, lft_roc$variables))
  
  ## plots
  
  lft_roc$summary_roc_plots$plots <- 
    list(x = lft_roc$responses, 
         y = exchange(lft_roc$responses, 
                      lft_globals$lexicon), 
         z = lft_roc$n_caps, 
         v = lft_roc$summary_roc_plots$labels) %>% 
    pmap(function(x, y, z, v) lft_roc$summary_roc_plots$data %>% 
           ggplot(aes(m = value, 
                      d = .data[[x]], 
                      color = variable)) + 
           geom_roc(labels = FALSE, 
                    n.cuts = 0) + 
           scale_color_manual(values = lft_roc$variable_colors, 
                              labels = v, 
                              name = '') + 
           style_roc(guide = TRUE, 
                     xlab = '1 - Sp', 
                     ylab = 'Se') + 
           geom_abline(slope = 1, 
                       intercept = 0, 
                       color = 'gray90') + 
           globals$common_theme + 
           labs(title = y, 
                subtitle = z))

# Summary table -------
  
  insert_msg('Summary table')
  
  lft_roc$result_tbl <- lft_roc$stats$optimal_cutoff %>% 
    reduce(rbind) %>% 
    mutate(response = exchange(as.character(response), 
                               dict = lft_globals$lexicon), 
           variable = exchange(as.character(variable), 
                               dict = lft_globals$lexicon), 
           auc_lab = stri_replace(auc_lab, 
                                  fixed = 'AUC = ', 
                                  replacement = '')) %>% 
    select(response, 
           variable, 
           auc_lab, 
           cutoff, 
           Se, 
           Sp, 
           J, 
           PPV, 
           NPV) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    set_names(c('LFT abnormality', 
                'CT variable', 
                'AUC', 
                'Cutoff', 
                'Sensitivity', 
                'Specificity',
                'J', 
                'PPV',
                'NPV'))
  
# END ------
  
  rm(i)
  
  insert_tail()