# Analysis of kinetic of categorical CT and LFT variables
# statistical significance determined by Cochran Q test 
# (due to non-normality of e.g. AI opacity). 
# Effect size estimated by Kendall's W
#
# The analysis is done for individuals with the complete datasets
# W = chisq/(df * n)

  insert_head()

# container -----

  kin_factor <- list()

# parallel backend -------

  insert_msg('Parallel backend')
  
  plan('multisession')

# Analysis globals -------

  insert_msg('Analysis globals')

  ## formulas
  
  kin_factor$formulas <- kin_globals$variables$factor %>% 
    map(~paste(.x, '~ follow_up | ID')) %>% 
    map(formula) %>% 
    set_names(kin_globals$variables$factor)
  
# Descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  kin_factor$stats <- kin_globals$analysis_tbl %>% 
    future_map(explore, 
               split_factor = 'follow_up', 
               variables = kin_globals$variables$factor, 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE))

  kin_factor$stats <- kin_factor$stats %>% 
    map(reduce, left_join, by = 'variable') %>% 
    compress(names_to = 'severity_class') %>% 
    relocate(severity_class) %>% 
    set_names(c('severity_class', 'variable', 
                levels(kin_globals$analysis_tbl[[1]]$follow_up)))
  
# Testing ------
  
  insert_msg('Testing')
  
  kin_factor$test <- kin_globals$analysis_tbl %>% 
    future_map(function(data) kin_factor$formulas %>% 
                 map_dfr(cochran_qtest, data = data), 
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'severity_class') %>% 
    re_adjust('p')
  
  kin_factor$test <- kin_factor$test %>% 
    mutate(severity_class = factor(severity_class, 
                                   names(kin_globals$analysis_tbl)), 
           variable = .y., 
           estimate = statistic/(n * df), 
           eff_size = paste('W =', signif(estimate, 2)), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = paste(plot_cap, n, sep = ', n = '), 
           strength = interpret_kendalls_w(estimate), 
           strength = stri_replace(strength, 
                                   regex = '\\s{1}agreement$', 
                                   replacement = ''), 
           strength = factor(strength, 
                             c('slight', 'fair', 'moderate', 
                               'substantial', 'almost perfect')),
           class = exchange(variable, 
                            dict = kin_globals$lexicon, 
                            value = 'class'))
  
# Plotting effect sizes --------
  
  insert_msg('Plotting effect sizes')
  
  kin_factor$eff_plots <- 
    list(data = blast(kin_factor$test, class), 
         plot_title = c('CT parameters, recovery effect', 
                        'LFT paramaters, recovery effect', 
                        'Symptoms, recovery effect')) %>% 
    pmap(plot_eff_bubble) %>% 
    map(~.x + 
          guides(size = 'none') + 
          scale_y_discrete(limits = rev(c('cohort', 
                                          'ambulatory mild', 
                                          'hospitalized moderate', 
                                          'hospitalized severe')), 
                           labels = globals$sev_labels) + 
          scale_x_discrete(labels = function(x) exchange(x, kin_globals$lexicon)))
  
# Plotting frequencies -------
  
  insert_msg('Plottng frequencies')
  
  kin_factor$plots <- 
    list(x = kin_globals$analysis_tbl, 
         y = blast(kin_factor$test, severity_class), 
         z = globals$sev_labels[names(kin_globals$analysis_tbl)]) %>% 
    future_pmap(function(x, y, z) list(variable = kin_globals$variables$factor, 
                                       plot_title = exchange(kin_globals$variables$factor, 
                                                             kin_globals$lexicon) %>% 
                                         paste(z, sep = ', '), 
                                       plot_subtitle = y$plot_cap) %>% 
                  pmap(function(variable, plot_title, plot_subtitle) plot_variable(x,
                                                                                   variable = variable, 
                                                                                   split_factor = 'follow_up', 
                                                                                   type = 'stack', 
                                                                                   scale = 'percent', 
                                                                                   cust_theme = globals$common_theme, 
                                                                                   y_lab = '% of strata', 
                                                                                   x_lab = 'Follow-up after COVID-19', 
                                                                                   plot_title = plot_title, 
                                                                                   plot_subtitle = plot_subtitle)) %>% 
                  map(~.x + 
                        scale_fill_manual(values = c(no = 'steelblue', 
                                                     yes = 'coral3'), 
                                          labels = c(no = 'absent', 
                                                     yes = 'present'), 
                                          name = '') +
                        theme(plot.tag = element_blank())), 
                .options = furrr_options(seed = TRUE)) %>%
    transpose %>% 
    set_names(kin_globals$variables$factor)
  
  
  kin_factor$plots <- kin_factor$plots %>% 
    transpose
  
# END -----
  
  plan('sequential')
  
  insert_tail()
