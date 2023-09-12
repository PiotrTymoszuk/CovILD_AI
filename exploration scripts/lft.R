# Descriptive stats for the LFT parameters split be the timepoint 
# and COVID-19 severity. No statistical testing is done - a 'proper' 
# statistical analysis is done later on with participant-matched data.
#
# This analytic task is here to get an idea, how the variables change
# in time and severity

  insert_head()

# container ------

  cohort_lft <- list()

# Analysis globals ------

  insert_msg('Analysis globals')
  
  cohort_lft$lexicon <- covild$lft_lexicon %>% 
    filter(!stri_detect(variable, regex = '(volume|DLCO)$'))

  cohort_lft$short_fup_labs <- globals$fup_labels %>% 
    stri_replace(fixed = ' mo', replacement = '') %>% 
    set_names(names(globals$fup_labels))
  
  ## numeric and categorical variables

  cohort_lft$variables <- cohort_lft$lexicon %>% 
    blast(format) %>% 
    map(~.x$variable)

  ## analysis table, introducing the severity variable
  ## removing the acute timepoint - there was no study CT but just 
  ## a diagnostic scan for some participants

  cohort_lft$analysis_tbl <- covild$lft %>% 
    left_join(covild$baseline[c('ID', 'severity_class')], 
              by = 'ID') %>% 
    mutate(cohort_split = severity_class)
  
  cohort_lft$analysis_tbl <- cohort_lft$analysis_tbl %>% 
    full_rbind(covild$lft) %>% 
    filter(follow_up != 'acute COVID-19') %>% 
    mutate(cohort_split = ifelse(is.na(cohort_split), 
                                 'cohort', as.character(cohort_split)), 
           cohort_split = factor(cohort_split, 
                                 c('cohort', 
                                   levels(cohort_lft$analysis_tbl$severity_class))), 
           follow_up = droplevels(follow_up), 
           analysis_split = interaction(follow_up, cohort_split)) %>% 
    relocate(cohort_split) %>% 
    relocate(analysis_split) %>% 
    relocate(ID)

# Number of complete cases per follow-up ------

  insert_msg('Number of complete cases per follow-up')
  
  cohort_lft$n_numbers <- cohort_lft$analysis_tbl %>% 
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

  insert_msg('Descriptive stats for the CT parameters')
  
  cohort_lft$raw_stats <- cohort_lft$analysis_tbl %>% 
    explore(split_factor = 'analysis_split', 
            variables = cohort_lft$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE)

## stitching the table in a user friendly format

  for(i in levels(cohort_lft$analysis_tbl$cohort_split)) {
    
    cohort_lft$stats[[i]] <- 
      cohort_lft$raw_stats[stri_detect(names(cohort_lft$raw_stats), fixed = i)] %>% 
      reduce(left_join, by = 'variable') %>% 
      set_names(c('variable', levels(cohort_lft$analysis_tbl$follow_up)))
    
  }

# Result table ------

  insert_msg('Result table')
  
  cohort_lft$result_tbl <- cohort_lft$stats %>% 
    map(format_summ_tbl, 
        dict = cohort_lft$lexicon)

# Plotting ------

  insert_msg('Plots')
  
  ## numeric dependent variables
  
  cohort_lft$plots$numeric <- 
    list(outcome = cohort_lft$variables$numeric, 
         plot_title = exchange(cohort_lft$variables$numeric, 
                               dict = cohort_lft$lexicon) %>% 
           capitalize_first_char, 
         y_lab = exchange(cohort_lft$variables$numeric, 
                          dict = cohort_lft$lexicon, 
                          value = 'unit')) %>% 
    pmap(plot_explo_numeric, 
         data = cohort_lft$analysis_tbl, 
         time_variable = 'follow_up', 
         severity_variable = 'cohort_split', 
         x_lab = 'Follow-up, months after COVID-19', 
         show_n = TRUE) %>% 
    map(~.x + 
          scale_fill_manual(values = globals$sev_colors) + 
          scale_color_manual(values = globals$sev_colors) + 
          scale_x_discrete(labels = cohort_lft$short_fup_labs) + 
          theme(legend.position = 'none')) %>% 
    set_names(cohort_lft$variables$numeric)
  
  ## categorical numeric variables
  
  cohort_lft$plots$factor <- 
    list(outcome = cohort_lft$variables$factor, 
         plot_title = exchange(cohort_lft$variables$factor, 
                               dict = cohort_lft$lexicon) %>% 
           capitalize_first_char) %>% 
    pmap(plot_explo_stack, 
         data = cohort_lft$analysis_tbl, 
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
          scale_x_discrete(labels = cohort_lft$short_fup_labs)) %>% 
    set_names(cohort_lft$variables$factor)

# END -----

  cohort_lft$raw_stats <- NULL
  cohort_lft <- compact(cohort_lft)

  rm(i)
  
  insert_tail()