# Descriptive stats for the symptoms split by the timepoint 
# and COVID-19 severity. No statistical testing is done - a 'proper' 
# statistical analysis is done later on with participant-matched data.
#
# This analytic task is here to get an idea, how the variables change
# in time and severity

  insert_head()

# container ------

  cohort_sympt <- list()

# Analysis globals ------

  insert_msg('Analysis globals')
  
  cohort_sympt$lexicon <- covild$symptoms_lexicon %>% 
    filter(variable != 'follow_up')

  ## numeric and categorical variables

  cohort_sympt$variables <- cohort_sympt$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)

  ## analysis table, introducing the severity variable
  ## removing the acute time point - there was no study CT but just 
  ## a diagnostic scan for some participants

  cohort_sympt$analysis_tbl <- covild$symptoms %>% 
    left_join(covild$baseline[c('ID', 'severity_class')], 
              by = 'ID') %>% 
    mutate(cohort_split = severity_class)
  
  cohort_sympt$analysis_tbl <- cohort_sympt$analysis_tbl %>% 
    full_rbind(covild$symptoms) %>% 
    filter(follow_up != 'acute COVID-19') %>% 
    mutate(cohort_split = ifelse(is.na(cohort_split), 
                                 'cohort', as.character(cohort_split)), 
           cohort_split = factor(cohort_split, 
                                 c('cohort', 
                                   levels(cohort_sympt$analysis_tbl$severity_class))), 
           follow_up = droplevels(follow_up))
  
  ## dummy factor for all follow-up points: all time points
  
  cohort_sympt$analysis_tbl <- cohort_sympt$analysis_tbl %>% 
    rbind(cohort_sympt$analysis_tbl, 
          mutate(cohort_sympt$analysis_tbl, 
                 follow_up = 'all time points')) %>% 
    mutate(follow_up = factor(follow_up,
                              c('all time points', 
                                levels(cohort_sympt$analysis_tbl$follow_up))))
  
  ## specification of the analysis splits
  
  cohort_sympt$analysis_tbl <- cohort_sympt$analysis_tbl %>% 
    mutate(analysis_split = interaction(follow_up, cohort_split)) %>% 
    relocate(cohort_split) %>% 
    relocate(analysis_split) %>% 
    relocate(ID)

# Number of complete cases per follow-up ------

  insert_msg('Number of complete cases per follow-up')
  
  cohort_sympt$n_numbers <- cohort_sympt$analysis_tbl %>% 
    filter(cohort_split != 'cohort') %>% 
    blast(severity_class, follow_up) %>%
    map(~filter(.x, complete.cases(.x))) %>% 
    map_dbl(nrow) %>% 
    compress(names_to = 'strata', 
             value = 'n') %>% 
    mutate(severity_class = stri_split_fixed(strata, 
                                             pattern = '.', 
                                             simplify = TRUE)[, 1], 
           follow_up = stri_split_fixed(strata, 
                                        pattern = '.', 
                                        simplify = TRUE)[, 2])

# Descriptive stats for the CT parameters ------

  insert_msg('Descriptive stats for the symptoms')
  
  cohort_sympt$raw_stats <- cohort_sympt$analysis_tbl %>% 
    explore(split_factor = 'analysis_split', 
            variables = cohort_sympt$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE)

  ## stitching the table in a user friendly format

  for(i in levels(cohort_sympt$analysis_tbl$cohort_split)) {
    
    cohort_sympt$stats[[i]] <- 
      cohort_sympt$raw_stats[stri_detect(names(cohort_sympt$raw_stats), fixed = i)] %>% 
      reduce(left_join, by = 'variable') %>% 
      set_names(c('variable', levels(cohort_sympt$analysis_tbl$follow_up)))
    
  }

# Result table ------

  insert_msg('Result table')
  
  cohort_sympt$result_tbl <- cohort_sympt$stats %>% 
    map(format_summ_tbl, 
        dict = cohort_sympt$lexicon) %>% 
    map(mutate, Variable = variable) %>% 
    map(select, -variable) %>% 
    map(relocate, Variable)

# Plotting ------

  insert_msg('Plots')
  
  ## numeric dependent variables: stack plots
  
  cohort_sympt$plots$numeric <- 
    list(outcome = cohort_sympt$variables$numeric, 
         plot_title = exchange(cohort_sympt$variables$numeric, 
                               dict = cohort_sympt$lexicon) %>% 
           capitalize_first_char) %>% 
    pmap(plot_explo_stack, 
         data = cohort_sympt$analysis_tbl %>% 
           map_dfc(function(x) if(is.numeric(x)) factor(x) else x), 
         time_variable = 'follow_up', 
         severity_variable = 'cohort_split', 
         x_lab = 'Follow-up, months after COVID-19', 
         y_lab = '% of strata', 
         show_n = TRUE) %>% 
    map2(c('mMRC', 'ECOG'), 
         ~.x + 
          scale_fill_brewer(palette = 'Purples') + 
          scale_x_discrete(labels = globals$short_fup_labs)) %>% 
    set_names(cohort_sympt$variables$numeric)
  
  ## categorical numeric variables
  
  cohort_sympt$plots$factor <- 
    list(outcome = cohort_sympt$variables$factor, 
         plot_title = exchange(cohort_sympt$variables$factor, 
                               dict = cohort_sympt$lexicon) %>% 
           capitalize_first_char) %>% 
    pmap(plot_explo_stack, 
         data = cohort_sympt$analysis_tbl, 
         time_variable = 'follow_up', 
         severity_variable = 'cohort_split', 
         show_n = TRUE, 
         y_lab = '% of strata', 
         x_lab = 'Follow-up, months after COVID-19', 
         txt_size = 2.5, 
         txt_form = 'text', 
         txt_color = 'white') %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'coral3')) + 
          scale_x_discrete(labels = globals$short_fup_labs)) %>% 
    set_names(cohort_sympt$variables$factor)

# END -----

  cohort_sympt$raw_stats <- NULL
  cohort_sympt <- compact(cohort_sympt)

  rm(i)
  
  insert_tail()