# Bootstrap tests for the following problems:
#
# 1) Comparison of CT severity variables (CTSS, opacity, high opacity) between 
# patients with and without LFT abnormalities
#
# 2) Correlation of the CT severity variables with LFT parameters
#
# Those problems are addressed by blocked bootstrap tests for difference in 
# medians between the observations with and without the LFT abnormality 
# and by bootstrapped Spearman's correlation

  insert_head()
  
# container ------
  
  lft_uniboot <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')

  plan('multisession')
    
# analysis data -------
  
  insert_msg('Analysis data')
  
  ## LFT variables
  
  lft_uniboot$lft_binary <- 
    c('LFT_findings', 'DLCO_reduced', 'FVC_reduced', 'FEV1_reduced')
  
  lft_uniboot$lft_numeric <- 
    c('DLCO_percent', 'FVC_percent', 'FEV1_percent')
  
  ## CT variables
  
  lft_uniboot$ct_variables <- 
    c('CTSS', 'opacity_percent', 'high_opacity_percent')
  
  ## analysis table
  
  lft_uniboot$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(ID, 
           all_of(lft_uniboot$lft_binary), 
           all_of(lft_uniboot$lft_numeric), 
           all_of(lft_uniboot$ct_variables)) %>% 
    as_tibble
  
# Descriptive stats for the binary LFT variables ------
  
  insert_msg('Descriptive stats')
  
  lft_uniboot$stats <- lft_uniboot$lft_binary %>% 
    set_names(lft_uniboot$lft_binary) %>% 
    map(~explore(lft_uniboot$analysis_tbl, 
                 variables = lft_uniboot$ct_variables, 
                 split_factor = .x, 
                 what = 'table', 
                 pub_styled = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'no', 'yes')) %>% 
    compress(names_to = 'lft_abnormality')
  
# Bootstrap tests for the binary LFT variables -------
  
  insert_msg('Bootstrap tests')
  
  set.seed(12345)
  
  for(i in lft_uniboot$lft_binary) {
    
    lft_uniboot$test[[i]] <- lft_uniboot$ct_variables %>% 
      map_dfr(~delta_median(data = lft_uniboot$analysis_tbl, 
                            variable = .x, 
                            split_factor = i, 
                            B = 2000))
    
  }
  
  ## multiple testing adjustment and plot_captions
  
  lft_uniboot$test <- lft_uniboot$test %>%
    compress(names_to = 'lft_abnormality') %>% 
    re_adjust %>% 
    mutate(eff_size = paste('r =', signif(r, 2)), 
           plot_cap = paste(eff_size, significance, sep = ', '))
  
# Box plots -------
  
  insert_msg('Box plots')
  
  lft_uniboot$box_plots <- 
    list(variable = lft_uniboot$test$variable, 
         split_factor = lft_uniboot$test$split_factor, 
         plot_title = exchange(lft_uniboot$test$variable, 
                               lft_globals$lexicon) %>% 
           capitalize_first_char, 
         plot_subtitle = lft_uniboot$test$plot_cap, 
         y_lab = exchange(lft_uniboot$test$variable, 
                          lft_globals$lexicon, 
                          value = 'unit'), 
         x_lab = exchange(lft_uniboot$test$split_factor, 
                          lft_globals$lexicon)) %>% 
    pmap(plot_variable, 
         lft_uniboot$analysis_tbl, 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE, 
         type = 'box') %>% 
    set_names(interaction(lft_uniboot$test$split_factor, 
                          lft_uniboot$test$variable))
  
# Correlation test --------
  
  insert_msg('Correlation test')
  
  lft_uniboot$cor_pairs <- lft_uniboot$lft_numeric %>% 
    map(function(lft) lft_uniboot$ct_variables %>% 
          map(~c(lft, .x))) %>% 
    unlist(recursive = FALSE)
  
  lft_uniboot$cor_test <- lft_uniboot$cor_pairs %>% 
    map_dfr(spearman_rho, 
            data = lft_uniboot$analysis_tbl, 
            B = 2000)
  
  ## FDR adjustment and plot caps
  
  lft_uniboot$cor_test <- lft_uniboot$cor_test %>% 
    re_adjust %>% 
    mutate(eff_size = paste0('\u03C1 = ', 
                             signif(rho, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'), 
           n = nrow(lft_uni$analysis_tbl), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = paste(plot_cap, n, sep = ', N = '))
  
# Scatter plots to visualize the correlations -------
  
  insert_msg('Scatter plots, correlations')
  
  lft_uniboot$cor_plots <- 
    list(variables = map2(lft_uniboot$cor_test$variable1, 
                          lft_uniboot$cor_test$variable2, 
                          c), 
         plot_title = map2(lft_uniboot$cor_test$variable1, 
                           lft_uniboot$cor_test$variable2, 
                           ~paste(exchange(.x, lft_globals$lexicon), 
                                  exchange(.y, lft_globals$lexicon),
                                  sep = ' and ')), 
         plot_subtitle = lft_uniboot$cor_test$plot_cap, 
         x_lab = exchange(lft_uniboot$cor_test$variable1, 
                          lft_globals$lexicon, 
                          value = 'table_label'), 
         y_lab = exchange(lft_uniboot$cor_test$variable2, 
                          lft_globals$lexicon, 
                          value = 'table_label')) %>% 
    pmap(plot_correlation, 
         data = lft_uniboot$analysis_tbl[c(lft_uniboot$lft_numeric, lft_uniboot$ct_variables)] %>% 
           map_dfc(rank), 
         type = 'correlation', 
         point_hjitter = 0, 
         point_wjitter = 0, 
         cust_theme = globals$common_theme) %>% 
    set_names(interaction(lft_uniboot$cor_test$variable1, 
                          lft_uniboot$cor_test$variable2))
  
# Summary tables: binary variables --------
  
  insert_msg('Summary tables for the manuscript: binary variables')
  
  ## n numbers
  
  lft_uniboot$n_numbers <- 
    lft_uniboot$analysis_tbl[lft_uniboot$lft_binary] %>% 
    map(table) %>% 
    map2(., names(.), 
         ~tibble(lft_abnormality = .y, 
                 no = .x[1], 
                 yes = .x[2]))
  
  ## for binary LFT variables
  
  lft_uniboot$binary_result_tbl <- lft_uniboot$stats %>% 
    mutate(lft_abnormality = factor(lft_abnormality, 
                                    lft_uniboot$lft_binary)) %>% 
    blast(lft_abnormality) %>% 
    map2_dfr(lft_uniboot$n_numbers, ., full_rbind)
  
  lft_uniboot$binary_result_tbl <- 
    left_join(lft_uniboot$binary_result_tbl , 
              lft_uniboot$test[c('variable', 
                                 'lft_abnormality', 
                                 'eff_size', 
                                 'significance')], 
              by = c('variable', 'lft_abnormality')) %>% 
    format_summ_tbl(dict = lft_globals$lexicon) %>% 
    mutate(variable = ifelse(is.na(variable), 
                             'Observations, n', variable), 
           lft_abnormality = exchange(lft_abnormality, 
                                      lft_globals$lexicon)) %>% 
    select(lft_abnormality, 
           variable, 
           no, yes, 
           significance, 
           eff_size) %>% 
    set_names(c('LFT abnormality', 
                'Variable', 
                'Abnormality absent', 
                'Abnormality present', 
                'Significance', 
                'Effect size'))
  
# Summary tables for the manuscript: correlation analysis results -------
  
  insert_msg('Summary table: correlation analysis')
  
  lft_uniboot$cor_result_tbl <- 
    lft_uniboot$cor_test %>% 
    transmute(`LFT variable` = exchange(variable1, lft_globals$lexicon), 
              `CT variable` = exchange(variable2, lft_globals$lexicon), 
              N = n, 
              `Correlation coeffcient` = eff_size, 
              Significance = significance)

# END ------
  
  plan('sequential')
  
  insert_tail()