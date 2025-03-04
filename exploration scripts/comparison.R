# Bootstrap tests for comparison of opacity, high opacity and CTSS between 
# observations with and without lung CT findings

  insert_head()
  
# container ------
  
  comp_ct <- list()
  
# parallel backend --------
  
  insert_msg('Parallel backend')

  plan('multisession')
    
# analysis globals: variables and their values ------
  
  insert_msg('Analysis globals')
  
  ## variables of interest
  
  comp_ct$ct_findings <- 
    c('GGO_finding', 'reticulation_finding', 'consolidation_finding')
  
  comp_ct$ct_severity <- 
    c('CTSS', 'opacity_percent', 'high_opacity_percent')
  
  ## and the corresponding data
  
  comp_ct$analysis_tbl <- covild$ct %>% 
    select(ID, 
           all_of(comp_ct$ct_findings), 
           all_of(comp_ct$ct_severity))
  
  ## variable lexicon 
  
  comp_ct$lexicon <- covild$ct_lexicon %>% 
    filter(variable %in% c(comp_ct$ct_findings, comp_ct$ct_severity))
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  comp_ct$stats <- comp_ct$ct_findings %>% 
    set_names(comp_ct$ct_findings) %>% 
    map(~explore(comp_ct$analysis_tbl, 
                 variables = comp_ct$ct_severity, 
                 split_factor = .x, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'no', 'yes')) %>% 
    compress(names_to = 'ct_abnormality')
  
# Bootstrap tests -------
  
  insert_msg('Bootstrap tests')
  
  set.seed(12345)
  
  for(i in comp_ct$ct_findings) {
    
    comp_ct$test[[i]] <- comp_ct$ct_severity %>% 
      map_dfr(~delta_median(data = comp_ct$analysis_tbl, 
                            variable = .x, 
                            split_factor = i, 
                            B = 2000))
    
  }
  
  ## multiple testing adjustment and plot_captions
  
  comp_ct$test <- comp_ct$test %>%
    compress(names_to = 'ct_abnormality') %>% 
    re_adjust %>% 
    mutate(eff_size = paste('r =', signif(r, 2)), 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
# Box plots -------
  
  insert_msg('Box plots')
  
  comp_ct$box_plots <- 
    list(variable = comp_ct$test$variable, 
         split_factor = comp_ct$test$split_factor, 
         plot_title = exchange(comp_ct$test$variable, 
                               comp_ct$lexicon) %>% 
           capitalize_first_char, 
         plot_subtitle = comp_ct$test$plot_cap, 
         y_lab = exchange(comp_ct$test$variable, 
                          comp_ct$lexicon, 
                          value = 'unit'), 
         x_lab = exchange(comp_ct$test$split_factor, 
                          comp_ct$lexicon)) %>% 
    pmap(plot_variable, 
         comp_ct$analysis_tbl, 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE, 
         type = 'box') %>% 
    set_names(interaction(comp_ct$test$split_factor, 
                          comp_ct$test$variable))
  
  ## styling
  
  comp_ct$box_plots <- comp_ct$box_plots %>% 
    map(~.x + 
          scale_fill_manual(values = c(no = 'steelblue', 
                                       yes = 'orangered3')) + 
          guides(fill = 'none'))
  
# Summary table -----
  
  insert_msg('Summary table')
  
  ## numbers of cases with and without the CT abnormalities of interest
  
  comp_ct$n_numbers <- 
    comp_ct$analysis_tbl[comp_ct$ct_findings] %>% 
    map(table) %>% 
    map2(., names(.), 
         ~tibble(ct_abnormality = .y, 
                 no = .x[1], 
                 yes = .x[2]))
  
  ## N numbers, descriptive stats, and bootstrap test results
  
  comp_ct$result_tbl <- comp_ct$stats %>% 
    mutate(ct_abnormality = factor(ct_abnormality, 
                                    comp_ct$ct_findings)) %>% 
    blast(ct_abnormality) %>% 
    map2_dfr(comp_ct$n_numbers, ., full_rbind)
  
  comp_ct$result_tbl <- 
    left_join(comp_ct$result_tbl , 
              comp_ct$test[c('variable', 
                             'ct_abnormality', 
                             'eff_size', 
                             'significance')], 
              by = c('variable', 'ct_abnormality')) %>% 
    format_summ_tbl(dict = comp_ct$lexicon) %>% 
    mutate(variable = ifelse(is.na(variable), 
                             'Observations, n', variable), 
           ct_abnormality = exchange(as.character(ct_abnormality), 
                                     comp_ct$lexicon)) %>% 
    select(ct_abnormality, 
           variable, 
           no, yes, 
           significance, 
           eff_size) %>% 
    set_names(c('CT abnormality', 
                'Variable', 
                'Abnormality absent', 
                'Abnormality present', 
                'Significance', 
                'Effect size'))

# END -------
  
  comp_ct$analysis_tbl <- NULL
  
  comp_ct <- compact(comp_ct)
  
  plan('sequential')
  
  insert_tail()