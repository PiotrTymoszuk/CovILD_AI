# Analysis of kinetic of numeric CT and LFT variables
# statistical significance determined by Friedman test 
# (due to non-normality of e.g. AI opacity). 
# Effect size estimated by Kendall's W
#
# The analysis is done for individuals with the complete datasets

  insert_head()
  
# container -----
  
  kin_numeric <- list()
  
# parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# Analysis globals -------
  
  insert_msg('Analysis globals')
  
  ## variables
  
  kin_numeric$variables <- kin_globals$variables$numeric
  
  kin_numeric$variable_class <- 
    ifelse(kin_numeric$variables %in% covild$ct_lexicon$variable, 
           'CT', 'LFT') %>% 
    tibble(variable = kin_numeric$variables, 
           class = .)
  
  kin_numeric$variables <- 
    set_names(kin_numeric$variables, 
              kin_numeric$variables)

  ## analysis table
  
  kin_numeric$analysis_tbl <- kin_globals$analysis_tbl %>% 
    map(select, ID, follow_up, all_of(kin_numeric$variables))

# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  kin_numeric$stats <- kin_numeric$analysis_tbl %>% 
    future_map(function(data) kin_numeric$variables %>% 
                 map(~explore(data[c('ID', 'follow_up', .x)] %>% 
                                complete_cases('ID'), 
                              variables = .x, 
                              split_factor = 'follow_up', 
                              what = 'table', 
                              pub_styled = TRUE)), 
               .options = furrr_options(seed = TRUE))
  
  kin_numeric$stats <- kin_numeric$stats %>% 
    map(map_dfr, reduce, left_join, by = 'variable') %>% 
    compress(names_to = 'severity_class') %>% 
    relocate(severity_class) %>% 
    set_names(c('severity_class', 'variable', 
                levels(kin_numeric$analysis_tbl[[1]]$follow_up)))
  
# Testing ---------
  
  insert_msg('Testing')
  
  kin_numeric$test <- kin_numeric$analysis_tbl %>% 
    future_map(function(data) kin_numeric$variables %>% 
                 map_dfr(~compare_variables(data[c('ID', 'follow_up', .x)] %>% 
                                              complete_cases('ID'), 
                                            variables = .x, 
                                            split_factor = 'follow_up', 
                                            what = 'eff_size', 
                                            types = 'kendall_w', 
                                            exact = FALSE, 
                                            ci = FALSE, 
                                            pub_styled = FALSE)), 
               .options = furrr_options(seed = TRUE))
  
  kin_numeric$test <- kin_numeric$test %>% 
    compress(names_to = 'severity_class') %>% 
    re_adjust %>% 
    mutate(severity_class = factor(severity_class, names(kin_numeric$test)), 
           n = n/4, ## cause each participant was measured at 4 follow-ups, 
           eff_size = paste('W =', signif(estimate, 2)), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = paste(plot_cap, n, sep = ', n = '), 
           strength = interpret_kendalls_w(estimate), 
           strength = stri_replace(strength, 
                                   regex = '\\s{1}agreement$', 
                                   replacement = ''), 
           strength = factor(strength, 
                             c('slight', 'fair', 'moderate', 
                               'substantial', 'almost perfect'))) 
  
  kin_numeric$test <- 
    left_join(kin_numeric$test, 
              kin_numeric$variable_class)
  
# Plotting effect sizes -------
  
  insert_msg('Plotting effect sizes')
  
  ## as a bubble plot
  
  kin_numeric$eff_plots <- 
    list(data = blast(kin_numeric$test, class), 
         plot_title = c('CT parameters, recovery effect', 
                        'LFT paramaters, recovery effect')) %>% 
    pmap(plot_eff_bubble) %>% 
    map(~.x + 
          guides(size = 'none') + 
          scale_x_discrete(labels = function(x) exchange(x, kin_globals$lexicon)) + 
          scale_y_discrete(limits = rev(c('cohort', 
                                          'ambulatory mild', 
                                          'hospitalized moderate', 
                                          'hospitalized severe')), 
                           labels = globals$sev_labels))
  
# Kinetic plots ------
  
  insert_msg('Kinetic plots')
  
  kin_numeric$plots <- 
    list(x = kin_numeric$analysis_tbl, 
         y = blast(kin_numeric$test, severity_class), 
         z = globals$sev_colors[names(kin_numeric$analysis_tbl)], 
         v = globals$sev_labels[names(kin_numeric$analysis_tbl)]) %>% 
    pmap(function(x, y, z, v) list(y_var = kin_numeric$variables, 
                                   plot_title = exchange(kin_numeric$variables, 
                                                         kin_globals$lexicon) %>% 
                                     paste(v, sep = ', '), 
                                   plot_subtitle = y$plot_cap, 
                                   y_lab = exchange(kin_numeric$variables, 
                                                    kin_globals$lexicon, 
                                                    value = 'unit')) %>% 
           pmap(plot_kinetic, 
                data = x, 
                form = 'ribbon', 
                point_size = 0, 
                fill_color = z) %>% 
           map(~.x + scale_x_discrete(labels = globals$fup_labels))) %>% 
    transpose
  
# END ------
  
  plan('sequential')
  
  insert_tail()