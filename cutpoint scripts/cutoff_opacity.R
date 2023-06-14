# Searching for the optimal cutoff of AI opacity to detect any CT abnormalities, 
# GGOs, reticulation, consolidation and bronchiectasis. Youden criterion 
# implemented by the optimalCutpoint package

  insert_head()
  
# container -------
  
  cut_opacity <- list()
  
# Analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## CT findings defined manually to serve as responses
  
  cut_opacity$responses <- cut_globals$responses

  ## analysis table
  
  cut_opacity$analysis_tbl <- covild$ct %>% 
    select(ID, follow_up, opacity_percent, all_of(cut_opacity$responses)) %>% 
    filter(complete.cases(.),
           follow_up != 'acute COVID-19')
  
# N numbers ------
  
  insert_msg('N numbers')
  
  cut_opacity$n_numbers <- cut_opacity$responses %>% 
    map(~count(cut_opacity$analysis_tbl, .data[[.x]]))
  
  cut_opacity$n_caps <- cut_opacity$n_numbers %>% 
    map(~paste0('total: n = ', sum(.x$n), 
                ', findings: n = ', .x$n[2]))
  
# Optimal cutpoint objects ------
  
  insert_msg('Optimal cutpoint objects')
  
  cut_opacity$cut_obj <- cut_opacity$responses %>% 
    map(~optimal.cutpoints('opacity_percent', 
                           status = .x, 
                           tag.healthy = 'no', 
                           methods = 'Youden', 
                           data = as.data.frame(cut_opacity$analysis_tbl)))
  
# Extracting the optimal cutpoint values and its stats ------
  
  insert_msg('Extracting the optimal cutpoint values and stats')
  
  cut_opacity$stats <- cut_opacity$cut_obj %>% 
    map(extract_cut) %>% 
    map(~.x[c('all_cutoffs', 'optimal_cutoff')]) %>% 
    transpose
  
  cut_opacity$stats$optimal_cutoff <- cut_opacity$stats$optimal_cutoff %>% 
    compress(names_to = 'response') %>% 
    mutate(response = factor(response, cut_opacity$responses), 
           auc_lab = paste0('AUC = ', signif(auc, 2), 
                            ' [', signif(lower_ci, 3), 
                            ' - ', signif(upper_ci, 2), ']'), 
           plot_lab = paste0('Se = ', signif(Se, 2), 
                             '\nSp = ', signif(Sp, 2), 
                             '\n', auc_lab))
  
# Plots for all possible cutoffs ------
  
  insert_msg('Plots for all possible cutoffs')
  
  cut_opacity$all_plots <- 
    list(data = cut_opacity$stats$all_cutoffs, 
         plot_title = exchange(names(cut_opacity$stats$all_cutoffs), 
                               dict = cut_globals$lexicon) %>% 
           capitalize_first_char,
         plot_subtitle = cut_opacity$n_caps, 
         opt_cutoff = cut_opacity$stats$optimal_cutoff$cutoff) %>% 
    pmap(plot_cut_search, 
         x_lab = 'AI opacity, %', 
         x_trans = 'log') %>% 
    transpose
  
# ROC plots -----
  
  insert_msg('ROC plots')
  
  cut_opacity$roc_plots <- 
    list(cutoff_stats = blast(cut_opacity$stats$optimal_cutoff, response), 
         d_variable = cut_opacity$responses, 
         color = cut_globals$response_colors, 
         plot_title = exchange(cut_opacity$responses, 
                               dict = cut_globals$lexicon) %>% 
           capitalize_first_char, 
         plot_subtitle = cut_opacity$n_caps) %>% 
    pmap(plot_roc, 
         data = cut_opacity$analysis_tbl, 
         m_variable = 'opacity_percent')
  
# Summary ROC plots for the CT abnormalities ------
  
  insert_msg('Summary ROC plots')
  
  ## color scale labels
  
  cut_opacity$summary_roc_plots$labels <- cut_opacity$stats$optimal_cutoff %>% 
    mutate(var_lab = exchange(response, dict = cut_globals$lexicon), 
           auc_lab = paste(var_lab, auc_lab, sep = ', '), 
           auc_lab = stri_replace(auc_lab, fixed = 'AUC = ', replacement = ''))
  
  cut_opacity$summary_roc_plots$labels <- 
    set_names(cut_opacity$summary_roc_plots$labels$auc_lab, 
              cut_opacity$summary_roc_plots$labels$response)
    
  ## plot
  
  cut_opacity$summary_roc_plots$plot <- cut_opacity$analysis_tbl %>% 
    pivot_longer(cols = all_of(unname(cut_opacity$responses)), 
                 names_to = 'abnormality', 
                 values_to = 'disease') %>% 
    mutate(abnormality = factor(abnormality, cut_opacity$responses)) %>% 
    ggplot(aes(m = opacity_percent, 
               d = disease, 
               color = abnormality)) + 
    geom_roc(labels = FALSE, 
             n.cuts = 0) + 
    scale_color_manual(values = cut_globals$response_colors, 
                       labels = cut_opacity$summary_roc_plots$labels, 
                       name = '') + 
    style_roc(guide = TRUE, 
              xlab = '1 - Sp', 
              ylab = 'Se') + 
    geom_abline(slope = 1, 
                intercept = 0, 
                color = 'gray90') + 
    globals$common_theme + 
    labs(title = 'AI opacity and CT abnormality')
  
# Table with the cutoff finding results -----
  
  insert_msg('Result table')
  
  cut_opacity$result_tbl <- cut_opacity$stats$optimal_cutoff %>% 
    mutate(response = exchange(response, dict = cut_globals$lexicon), 
           auc_lab = stri_replace(auc_lab, fixed = 'AUC = ', replacement = '')) %>% 
    select(response, 
           auc_lab, 
           cutoff, 
           Se, 
           Sp, 
           J, 
           PPV, 
           NPV) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    set_names(c('CT abnormality', 
                'AUC', 
                'Cutoff', 
                'Sensitivity', 
                'Specificity',
                'J', 
                'PPV',
                'NPV'))
  
# END ------
  
  insert_tail()