# Optimal cutpoints for opacity, high opacity and CTSS
# for detection of any CT abnormalities, reduced DLCO, 
# reduced FVC and reduced FEV1
# 
# The optimal cutoff is determined by the Youden criterion

  insert_head()
  
# container -------
  
  lft_roc <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## responses and marker variables
  
  lft_roc$variables <- 
    c('opacity_percent', 'high_opacity_percent', 'CTSS')

  lft_roc$responses <- 
    c('LFT_findings', 
      'DLCO_reduced', 
      'FVC_reduced', 
      'FEV1_reduced')
  
  lft_roc[c("variables", "responses")] <- 
    lft_roc[c("variables", "responses")] %>% 
    map(~set_names(.x, .x))

  ## analysis table
  
  lft_roc$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(ID, 
           all_of(lft_roc$responses), 
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
                ', events: n = ', .x$n[2]))
  
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
  
# Summary ROC plots for each LFT abnormality ------
  
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

# Bootstrapped kappa and ROC metrics for the optimal cutoffs --------
  
  insert_msg('Bootstrapped cutoff stats')
  
  ## stratified CT parameters and the LFT responses
  
  lft_roc$bootstrap$data <- lft_roc$cut_obj %>% 
    map(map, ~.x$data) %>% 
    map(map, set_names, c('marker', 'response')) %>% 
    map(map, rownames_to_column, 'ID') %>% 
    map(map, mutate, ID = stri_extract(ID, regex = 'P\\d{3}'))
  
  lft_roc$bootstrap$cutoff_value <- lft_roc$cut_obj %>% 
    map(map, ~.x$Youden$Global$optimal.cutoff$cutoff)
  
  lft_roc$bootstrap$data <- 
    map2(lft_roc$bootstrap$data, 
         lft_roc$bootstrap$cutoff_value, 
         function(data, cutoff) map2(data, cutoff,  
                                     ~mutate(.x, 
                                             marker = cut(marker, 
                                                          c(-Inf, .y, Inf), 
                                                          c('no', 'yes'), 
                                                          right = FALSE))))
  
  ## bootstrapped inter-rater reliability and ROC stats
  
  lft_roc$bootstrap$stats <- lft_roc$bootstrap$data %>% 
    map(future_map, 
        rater_stats, 
        d_variable = 'response',
        m_variable = 'marker', 
        B = 2000, 
        positive = 'yes', 
        .options = furrr_options(seed = TRUE))
  
  lft_roc$bootstrap$stats <- lft_roc$bootstrap$stats %>% 
    map(compress, names_to = 'marker') %>% 
    compress(names_to = 'response')
  
  ## Forest plots with the bootstrapped stats
  
  lft_roc$bootstrap$plots <- lft_roc$bootstrap$stats %>% 
    blast(response) %>% 
    map(plot_lft_forests)

# Summary tables -------
  
  insert_msg('Summary tables')
  
  ## Cutoff finding
  
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
  
  ## bootstap stats
  
  lft_roc$bootstrap_result_tbl <- lft_roc$bootstrap$stats %>% 
    filter(statistic %in% c('kappa', 'Se', 'Sp')) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    mutate(table_lab = paste0(estimate, ' [', 
                              boot_lower_ci, ' - ', 
                              boot_upper_ci, ']'), 
           marker = exchange(marker, 
                             lft_globals$lexicon), 
           response = exchange(response, 
                               lft_globals$lexicon)) %>% 
    select(response, marker, statistic, table_lab) %>% 
    pivot_wider(names_from = 'statistic', 
                values_from = 'table_lab') %>% 
    set_names(c('LFT abnormality', 
                'CT variable', 
                'Kappa', 
                'Sensitivity', 
                'Specificity'))
  
  ## A single summary table
  
  lft_roc$summary_result_tbl <- 
    lft_roc[c("result_tbl", "bootstrap_result_tbl")] %>% 
    map2(., 
         list(c('Cutoff', 'AUC'), 
              c('Kappa', 'Sensitivity', 'Specificity')), 
         ~select(.x, 
                 `LFT abnormality`, 
                 `CT variable`, 
                 all_of(.y))) %>% 
    reduce(left_join, by = c('LFT abnormality', 'CT variable')) %>% 
    pivot_longer(cols = c('AUC', 'Kappa', 'Sensitivity', 'Specificity'), 
                 names_to = 'Statistic', 
                 values_to = 'Value, 95% CI') %>% 
    mutate(Cutoff = ifelse(Statistic == 'AUC', NA, Cutoff), 
           Statistic = ifelse(Statistic == 'Kappa', 
                              '\u03BA', Statistic))
  
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
  
# END ------
  
  plan('sequential')
  
  rm(i)
  
  insert_tail()