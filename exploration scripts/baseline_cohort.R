# Baseline characteristic of the study cohort
# Comparison of the study readouts between the severity strata
# with KruskalWallis or Chi-Squared test

  insert_head()
  
# container -----
  
  cohort <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')

  cohort$lexicon <- covild$baseline_lexicon %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'kruskal_eta', 'cramer_v'))
  
  ## analysis table: the entire cohort
  ## and particular severity strata
  
  cohort$analysis_tbl <- 
    c(list(cohort = covild$baseline), 
      blast(covild$baseline, severity_class)) %>% 
    compress(names_to = 'cohort_split') %>% 
    mutate(cohort_split = factor(cohort_split, 
                                 c('cohort', 
                                   levels(covild$baseline$severity_class))))
  
# Descriptive statistics ------
  
  insert_msg('Descriptive statistic')
  
  ## n numbers
  
  cohort$n_numbers <- cohort$analysis_tbl %>% 
    count(cohort_split)
  
  ## stats
  
  cohort$stats <- cohort$analysis_tbl %>% 
    explore(split_factor = 'cohort_split', 
            variables = cohort$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(cohort$analysis_tbl$cohort_split)))
  
# Testing for differences between the severity strata -----
  
  insert_msg('Testing for differences between the severity strata')
  
  cohort$test <- covild$baseline %>% 
    compare_variables(variables = cohort$lexicon$variable, 
                      split_factor = 'severity_class', 
                      what = 'eff_size', 
                      types = cohort$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE, 
                      adj_method = 'BH')
  
# Result table ------
  
  insert_msg('Result table')
  
  cohort$result_tbl <- 
    left_join(cohort$stats, 
              cohort$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_summ_tbl(dict = cohort$lexicon, rm_n = TRUE) %>% 
    full_rbind(tibble(variable = 'Participants, n', 
                      `ambulatory mild` = cohort$n_numbers$n[1], 
                      `hospitalized moderate` = cohort$n_numbers$n[2], 
                      `hospitalized severe` = cohort$n_numbers$n[3]), .)
  
# END -------
  
  insert_head()