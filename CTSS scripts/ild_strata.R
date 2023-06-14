# Performance of the CTSS cutoff defining the ILD in the severity strata
# and at the follow-ups

  insert_head()
  
# container -----
  
  ild_strata <- list()
  
# parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## optimal cutoff
  
  ild_strata$cutoff <- ild_cutoff$stats$optimal_cutoff$cutoff

  ## analysis table
  
  ild_strata$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    filter(severity_class != 'cohort') %>% 
    mutate(CTSS = cut(CTSS, 
                      c(-Inf, ild_strata$cutoff, Inf), 
                      c('no', 'yes'), 
                      right = FALSE), 
           opacity_class = car::recode(opacity_class, 
                                       "'0-5%' = 'no'; '>5%' = 'yes'")) %>% 
    select(ID, severity_class, follow_up, CTSS, opacity_class) %>% 
    filter(complete.cases(.))
    
  ild_strata$analysis_tbl <- ild_strata$analysis_tbl %>% 
    mutate(severity_class = 'cohort', 
           follow_up = 'cohort') %>% 
    rbind(ild_strata$analysis_tbl, .) %>% 
    mutate(follow_up = factor(follow_up, 
                              c('cohort', 
                                levels(ild_strata$analysis_tbl$follow_up))))
  
# calculation of bootstrapped stats for the severity classes --------
  
  insert_msg('Inter-rater stats for the severity classes')
  
  ## working with safely: especially in mild cases, 
  ## where ILD may not appear
  
  ild_strata$severity_stats <- ild_strata$analysis_tbl %>% 
    blast(severity_class) %>% 
    map(rater_stats, 
        d_variable = 'opacity_class',
        m_variable = 'CTSS', 
        positive = 'yes', 
        B = 2000) %>% 
    compress(names_to = 'severity_class') %>%
    mutate(severity_class = factor(severity_class, 
                                   levels(ild_strata$analysis_tbl$severity_class)), 
           response = 'opacity_class')
  
# calculation of bootstrapped stats for the follow-ups --------
  
  insert_msg('Inter-rater stats for the follow-ups')
  
  ## working with safely: especially in mild cases, 
  ## where ILD may not appear
  
  ild_strata$follow_up_stats <- ild_strata$analysis_tbl %>% 
    blast(follow_up) %>% 
    map(rater_stats, 
        d_variable = 'opacity_class',
        m_variable = 'CTSS', 
        positive = 'yes', 
        B = 2000) %>% 
    compress(names_to = 'follow_up') %>%
    mutate(follow_up = factor(follow_up, 
                              levels(ild_strata$analysis_tbl$follow_up)), 
           response = 'opacity_class')
  
# Plotting the inter-rater stats for severity classes and follow-ups --------
  
  insert_msg('Plots')
  
  ild_strata$severity_plots <- 
    plot_rater_stats(data = ild_strata$severity_stats, 
                     response = 'opacity_class', 
                     split_factor = 'severity_class', 
                     split_palette = globals$sev_colors, 
                     split_labels = globals$sev_labels, 
                     plot_title = paste('ILD detection', 
                                        c("Cohen's kapps", 'Sensitivity', 
                                          'Specificity', 'Accuracy'), 
                                        sep = ', '))
  
  ild_strata$follow_up_plots <- 
    plot_rater_stats(data = ild_strata$follow_up_stats, 
                     response = 'opacity_class', 
                     split_factor = 'follow_up', 
                     split_palette = globals$fup_colors, 
                     split_labels = globals$fup_labels, 
                     plot_title = paste('ILD detection', 
                                        c("Cohen's kapps", 'Sensitivity', 
                                          'Specificity', 'Accuracy'), 
                                        sep = ', '))
  
# Result tables -----
  
  insert_msg('Result tables')
  
  ild_strata[c('severity_result_tbl', 'follow_up_result_tbl')] <- 
    ild_strata[c("severity_stats", "follow_up_stats")] %>% 
    map(map_dfc, function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    map(mutate, 
        kappa = paste0(kappa, ' [', kappa_lower, ' - ', kappa_upper, ']'), 
        Se = paste0(Se, ' [', Se_lower, ' - ', Se_upper, ']'), 
        Sp = paste0(Sp, ' [', Sp_lower, ' - ', Sp_upper, ']'), 
        accuracy = paste0(accuracy, ' [', accuracy_lower, ' - ', accuracy_upper, ']'), 
        response = exchange(response, dict = cut_globals$lexicon)) %>% 
    map(select, 
        any_of(c('follow_up', 'severity_class')), 
        kappa, Se, Sp, accuracy)
  
# END -----
  
  plan('sequential')
  
  insert_tail()