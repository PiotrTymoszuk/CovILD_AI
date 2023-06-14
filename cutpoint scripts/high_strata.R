# Testing for performance of the opacity cutoff at detecting any CT findings,  
# GGOs, consolidations etc. in the severity strata and timepoints after 
# infection

  insert_head()
  
# container -----
  
  high_strata <- list()
  
# Parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  ## optimal cutoffs identified by the Youden criterion
  
  high_strata$responses <- cut_globals$responses
  
  high_strata$cutoffs <- cut_high$stats$optimal_cutoff$cutoff %>% 
    set_names(cut_high$stats$optimal_cutoff$response)
  
  high_strata$markers <- stri_replace(high_strata$responses, 
                                    regex = '_find.*$', 
                                    replacement = '_marker')
  
  ## analysis tables with the binarized AI-determined opacity
  
  high_strata$analysis_tbl <- covild$ct %>% 
    select(ID, follow_up, high_opacity_percent, all_of(high_strata$responses)) %>% 
    left_join(covild$baseline[c('ID', 'severity_class')], 
              by = 'ID') %>% 
    filter(complete.cases(.), 
           follow_up != 'acute COVID-19') %>% 
    mutate(follow_up = droplevels(follow_up))
  
  ## binarization
  
  for(i in pmap(list(high_strata$marker, high_strata$cutoffs), list)) {
    
    high_strata$analysis_tbl <- high_strata$analysis_tbl %>% 
      mutate(!!i[[1]] := cut(high_opacity_percent, 
                             c(-Inf, i[[2]], Inf), 
                             c('no', 'yes'), 
                             right = FALSE))
    
  }
  
  ## merging with the data table for cohort stats
  
  high_strata$analysis_tbl <- high_strata$analysis_tbl %>% 
    rbind(high_strata$analysis_tbl %>% 
            mutate(follow_up = 'cohort', 
                   severity_class = 'cohort'), .) %>% 
    mutate(follow_up = factor(follow_up, 
                              c('cohort', 
                                levels(high_strata$analysis_tbl$follow_up))), 
           severity_class = factor(severity_class, 
                                   c('cohort', 
                                     levels(high_strata$analysis_tbl$severity_class))))

# calculation of bootstrapped stats for the severity classes --------
  
  insert_msg('Inter-rater stats for the severity classes')
  
  ## working with safely: especially in mild cases, some CT abnormalities
  ## may not appear (e.g. bronchiectasis)
  
  set.seed(1234)
  
  high_strata$severity_stats <- high_strata$analysis_tbl %>% 
    blast(severity_class) %>% 
    map(function(x) future_map2(high_strata$responses, 
                                high_strata$markers, 
                                ~safely(rater_stats)(data = x, 
                                                     d_variable = .x, 
                                                     m_variable = .y, 
                                                     B = 2000), 
                                .options = furrr_options(seed = 1234)))
  
  high_strata$severity_stats <- high_strata$severity_stats %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(compact) %>% 
    map(compress, names_to = 'response') %>% 
    compress(names_to = 'severity_class') %>% 
    mutate(severity_class = factor(severity_class, 
                                   levels(high_strata$analysis_tbl$severity_class)), 
           reponse = factor(response, high_strata$responses))
  
# Calculation of bootstrapped stats for the timepoints ------
  
  insert_msg('Inter-rater stats for the timepoints')
  
  set.seed(1234)
  
  high_strata$follow_up_stats <- high_strata$analysis_tbl %>% 
    blast(follow_up) %>% 
    map(function(x) future_map2(high_strata$responses, 
                                high_strata$markers, 
                                ~safely(rater_stats)(data = x, 
                                                     d_variable = .x, 
                                                     m_variable = .y, 
                                                     B = 2000), 
                                .options = furrr_options(seed = 1234)))
  
  high_strata$follow_up_stats <- high_strata$follow_up_stats %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(compact) %>% 
    map(compress, names_to = 'response') %>% 
    compress(names_to = 'follow_up') %>% 
    mutate(follow_up = factor(follow_up, 
                              levels(high_strata$analysis_tbl$follow_up)), 
           response = factor(response, high_strata$responses))
  
# Plotting of the stats for the severity classes and timepoints ------
  
  insert_msg('Plotting, severity classes and timepoints')

  high_strata$severity_plots <- high_strata$responses %>% 
    map(plot_rater_stats, 
        data = high_strata$severity_stats, 
        split_factor = 'severity_class', 
        split_palette = globals$sev_colors, 
        split_labels = globals$sev_labels)
  
  high_strata$follow_up_plots <- high_strata$responses %>% 
    map(plot_rater_stats, 
        data = high_strata$follow_up_stats, 
        split_factor = 'follow_up', 
        split_palette = globals$fup_colors, 
        split_labels = globals$fup_labels)

# Result table -----  
  
  insert_msg('Result tables')
  
  high_strata[c('severity_result_tbl', 'follow_up_result_tbl')] <- 
    high_strata[c("severity_stats", "follow_up_stats")] %>% 
    map(map_dfc, function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    map(mutate, 
        kappa = paste0(kappa, ' [', kappa_lower, ' - ', kappa_upper, ']'), 
        Se = paste0(Se, ' [', Se_lower, ' - ', Se_upper, ']'), 
        Sp = paste0(Sp, ' [', Sp_lower, ' - ', Sp_upper, ']'), 
        accuracy = paste0(accuracy, ' [', accuracy_lower, ' - ', accuracy_upper, ']'), 
        response = exchange(response, dict = cut_globals$lexicon)) %>% 
    map(select, 
        response, 
        any_of(c('follow_up', 'severity_class')), 
        kappa, Se, Sp, accuracy)
  
# END -----
  
  plan('sequential')
  
  insert_tail()