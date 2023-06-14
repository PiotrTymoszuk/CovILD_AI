# Checking for the optimal CTSS cutoff to detect interstitial lung disease
# defined as opacity in >5% of the lung.
#
# Done with Youden criterion for the entire cohort

  insert_head()
  
# container ------
  
  ild_cutoff <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ild_cutoff$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    filter(severity_class == 'cohort') %>% 
    select(ID, CTSS, opacity_class, opacity_percent) %>% 
    filter(complete.cases(.))
  
# Number of cases ------
  
  insert_msg('Number of cases')
  
  ild_cutoff$n_numbers <- ild_cutoff$analysis_tbl %>% 
    count(opacity_class)
  
  ild_cutoff$n_caps <- 
    paste0('total: n = ', sum(ild_cutoff$n_numbers$n), 
           ', findings: n = ', ild_cutoff$n_numbers$n[2])
  
# optimal cutpoint object ------
  
  insert_msg('Optrimal cutpoint object')
  
  ild_cutoff$cut_obj <- 
    optimal.cutpoints('CTSS', 
                      status = 'opacity_class', 
                      tag.healthy = '0-5%', 
                      methods = 'Youden', 
                      data = as.data.frame(ild_cutoff$analysis_tbl))

# Cutpoint stats ------
  
  insert_msg('Cutpoint stats')
  
  ild_cutoff$stats <- ild_cutoff$cut_obj %>% 
    extract_cut
  
  ild_cutoff$stats$optimal_cutoff <- ild_cutoff$stats$optimal_cutoff %>% 
    mutate(response = 'opacity_percent', 
           auc_lab = paste0('AUC = ', signif(auc, 2), 
                            ' [', signif(lower_ci, 3), 
                            ' - ', signif(upper_ci, 2), ']'), 
           plot_lab = paste0('Se = ', signif(Se, 2), 
                             '\nSp = ', signif(Sp, 2), 
                             '\n', auc_lab))
  
# Plotting cutpoint stats for all possible cutoffs -------
  
  insert_msg('Plotting cutoff stats for all possible cutoffs')
  
  ild_cutoff$all_plots <- 
    plot_cut_search(data = ild_cutoff$stats$all_cutoffs, 
                    opt_cutoff = ild_cutoff$stats$optimal_cutoff$cutoff, 
                    x_lab = 'CTSS')
  
# ROC plot -------
  
  insert_msg('ROC plot')
  
  ild_cutoff$roc_plot <- 
    plot_roc(data = ild_cutoff$analysis_tbl %>% 
               mutate(opacity_class = as.numeric(opacity_class) - 1), 
             cutoff_stats = ild_cutoff$stats$optimal_cutoff, 
             m_variable = 'CTSS', 
             d_variable = 'opacity_class', 
             plot_title = 'ILD detection by CTSS', 
             plot_subtitle = ild_cutoff$n_caps, 
             color = 'orangered3')
  
# Result table with the cutoff stats -------
  
  insert_msg('Result table')
  
  ild_cutoff$result_tbl <- ild_cutoff$stats$optimal_cutoff %>% 
    mutate(response = 'AI opacity >5%', 
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
  
# Scatter plot of CTSS and opacity -----
  
  insert_msg('Scatter plot of opacity and CTSS')
  
  ild_cutoff$scatter_plot <- 
    scatter_plot(data = ild_cutoff$analysis_tbl, 
                 x_var = 'CTSS', 
                 y_var = 'opacity_percent', 
                 fill_var = 'opacity_class', 
                 plot_title = 'AI opacity and CTSS', 
                 plot_subtitle = ild_cutoff$n_caps, 
                 point_wjitter = 0.1, 
                 point_alpha = 0.75) + 
    scale_fill_manual(values = c('gray60', 'orangered3'), 
                      name = '% AI opacity') + 
    scale_color_manual(values = c('black', 'black'), 
                       name = '% AI opacity') + 
    geom_vline(xintercept = ild_cutoff$stats$optimal_cutoff$cutoff, 
               linetype = 'dashed') + 
    geom_hline(yintercept = 5, 
               linetype = 'dashed')
  
# END -----
  
  insert_tail()