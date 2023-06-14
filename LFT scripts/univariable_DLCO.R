# Association of explanatory variables with DLCO expressed as percentage 
# of reference value
# Numeric variables: Spearman's correlation
# Categorical variables: comparison of DLCO values between the categories 
# by Mann-Whitney or Kruskal-Wallis test.

  insert_head()

# container -------

  dlco_uni <- list()

# analysis globals -------

insert_msg('Analysis globals')
  
  ## variables and analysis table

  dlco_uni$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)

  dlco_uni$analysis_tbl <- 
    lft_globals$analysis_tbl[c('ID', 'DLCO_percent', dlco_uni$variables)]
  
  ## variable lexicon, appended with the plot type
  ## and type of the statistical test
  
  dlco_uni$lexicon <- lft_globals$lexicon %>% 
    filter(variable %in% dlco_uni$variables)
  
  dlco_uni$lexicon$level_no <- 
    dlco_uni$analysis_tbl[dlco_uni$lexicon$variable] %>% 
    map(levels) %>% 
    map_dbl(length)
  
  dlco_uni$lexicon <- dlco_uni$lexicon %>% 
    mutate(plot_type = ifelse(format == 'numeric', 
                              'box', 'stack'), 
           y_lab = ifelse(format == 'factor', 
                          '% of reference', 
                          table_label), 
           analysis_type = ifelse(format == 'factor', 
                                  'comparison', 'correlation'), 
           analysis_type = factor(analysis_type, 
                                  c('comparison', 'correlation')), 
           test_type = ifelse(level_no == 0, 
                              'spearman', 
                              ifelse(level_no == 2, 
                                     'wilcoxon_r', 'kruskal_etasq')))
  
  dlco_uni$lexicon$variable <- 
    set_names(dlco_uni$lexicon$variable, 
              dlco_uni$lexicon$variable)
  
  dlco_uni$lexicon <- dlco_uni$lexicon %>% 
    blast(analysis_type)

# Comparison -------
  
  insert_msg('Comparison')
  
  ## descriptive stats
  
  dlco_uni$comparison$stats <- dlco_uni$lexicon$comparison$variable %>% 
    map(~explore(data = dlco_uni$analysis_tbl, 
                 split_factor = .x, 
                 variables = 'DLCO_percent', 
                 what = 'table',
                 pub_styled = TRUE)) %>% 
    map(compress, names_to = 'category') %>% 
    compress(names_to = 'split_factor') %>% 
    select(split_factor, category, statistic)
  
  ## test
  
  dlco_uni$comparison$test <- 
    list(split_factor = dlco_uni$lexicon$comparison$variable, 
         types = dlco_uni$lexicon$comparison$test_type) %>% 
    pmap(compare_variables, 
         dlco_uni$analysis_tbl, 
         variables = 'DLCO_percent', 
         what = 'eff_size', 
         exact = FALSE, 
         ci = FALSE, 
         pub_styled = TRUE) %>% 
    compress(names_to = 'split_factor') %>% 
    re_adjust %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           test = stri_replace(test, regex = '\\s{1}test$', replacement = ''))
  
  ## significant effects 
  
  dlco_uni$comparison$significant <- dlco_uni$comparison$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$split_factor
  
# Plots for the comparison ------
  
  insert_msg('Plots for the comparisons')
  
  dlco_uni$comparison$plots <- 
    list(split_factor = dlco_uni$lexicon$comparison$variable, 
         plot_title = capitalize_first_char(dlco_uni$lexicon$comparison$label), 
         plot_subtitle = dlco_uni$comparison$test$plot_cap) %>% 
    pmap(plot_variable, 
         dlco_uni$analysis_tbl, 
         variable = 'DLCO_percent', 
         type = 'box', 
         cust_theme = globals$common_theme, 
         y_lab = 'DLCO, % of reference', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Greens') + 
          theme(legend.position = 'none'))
  
# Result table for comparison --------
  
  insert_msg('Result table for comparison')
  
  dlco_uni$comparison$result_tbl <- 
    left_join(dlco_uni$comparison$stats,
              dlco_uni$comparison$test[c('split_factor', 
                                         'test', 
                                         'significance', 
                                         'eff_size')], 
              by = 'split_factor') %>% 
    set_names(c('variable', 'category', 'statistic', 
                'test', 'significance', 'eff_size')) %>% 
    format_summ_tbl(dict = dlco_uni$lexicon$comparison, 
                    value = 'table_label')
  
# Correlation analysis -------
  
  insert_msg('Correlation analysis')
  
  ## Spearman's rank test
  
  dlco_uni$correlation$test <- dlco_uni$lexicon$correlation$variable %>% 
    map_dfr(~correlate_variables(dlco_uni$analysis_tbl, 
                                 variables = c(.x, 'DLCO_percent'), 
                                 what = 'correlation', 
                                 type = 'spearman', 
                                 ci = TRUE, 
                                 pub_styled = FALSE)) %>% 
    re_adjust %>% 
    mutate(eff_size = paste0('\u03C1 = ', signif(estimate, 2), 
                             ' [', signif(lower_ci, 2), 
                             ' - ', signif(upper_ci, 2), ']'), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = paste(plot_cap, n, sep = ', n = '), 
           strength = interpret_r(estimate, rules = 'cohen1988'), 
           strength = factor(strength, 
                             c('very small', 'small', 'moderate', 'large')))
  
# Scatter plots to visualize correlations -----
  
  insert_msg('Scatter plots')
  
  dlco_uni$correlation$plots <- 
    list(variables = map(dlco_uni$lexicon$correlation$variable, 
                         ~c(.x, 'DLCO_percent')), 
         plot_title = capitalize_first_char(dlco_uni$lexicon$correlation$label), 
         plot_subtitle = dlco_uni$correlation$test$plot_cap, 
         x_lab = paste0(dlco_uni$lexicon$correlation$y_lab, ', rank')) %>% 
    pmap(plot_correlation, 
         dlco_uni$analysis_tbl, 
         y_lab = 'DLCO, % of reference, rank', 
         cust_theme = globals$common_theme)
  
  ## variables as log-transformed ranks
  
  for(i in names(dlco_uni$correlation$plots)) {
    
    dlco_uni$correlation$plots[[i]]$data <- 
      dlco_uni$correlation$plots[[i]]$data %>% 
      map_dfc(rank)
    
  }
  
  dlco_uni$correlation$plots <- dlco_uni$correlation$plots %>% 
    map(~.x + 
          scale_x_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)) + 
          scale_y_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)))
  
# Bubble plots with correlation coefficients -----
  
  insert_msg('Bubble plots with correlation coefficients')
  
  dlco_uni$correlation$eff_plot <- 
    plot_eff_bubble(data = dlco_uni$correlation$test, 
                    x_var = 'variable1', 
                    y_var = 'variable2', 
                    plot_title = 'DLCO, explanatory factors', 
                    plot_subtitle = "Spearman's \u03C1") + 
    scale_x_discrete(labels = function(x) exchange(x, dlco_uni$lexicon$correlation)) + 
    scale_y_discrete(labels = function(x) exchange(x, covild$lft_lexicon)) + 
    scale_size_area(limits = c(0, 1), 
                    max_size = 6, 
                    name = expression('abs(' * rho * ')'))
  
# END -----
  
  rm(i)
  
  insert_tail()
  
  
  
  
    

  
  
