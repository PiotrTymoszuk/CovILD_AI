# Association of explanatory variables with FEV1 expressed as percentage 
# of reference value
# Numeric variables: Spearman's correlation
# Categorical variables: comparison of FEV1 values between the categories 
# by Mann-Whitney or Kruskal-Wallis test.

  insert_head()

# container -------

  fev1_uni <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  ## variables and analysis table

  fev1_uni$variables <- 
    lft_globals[c("ct_variables", 
                  "symptom_variables", 
                  "baseline_variables")] %>% 
    reduce(c)

  fev1_uni$analysis_tbl <- 
    lft_globals$analysis_tbl[c('ID', 'FEV1_percent', fev1_uni$variables)]
  
  ## variable lexicon, appended with the plot type
  ## and type of the statistical test
  
  fev1_uni$lexicon <- lft_globals$lexicon %>% 
    filter(variable %in% fev1_uni$variables)
  
  fev1_uni$lexicon$level_no <- 
    fev1_uni$analysis_tbl[fev1_uni$lexicon$variable] %>% 
    map(levels) %>% 
    map_dbl(length)
  
  fev1_uni$lexicon <- fev1_uni$lexicon %>% 
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
  
  fev1_uni$lexicon$variable <- 
    set_names(fev1_uni$lexicon$variable, 
              fev1_uni$lexicon$variable)
  
  fev1_uni$lexicon <- fev1_uni$lexicon %>% 
    blast(analysis_type)

# Comparison -------
  
  insert_msg('Comparison')
  
  ## descriptive stats
  
  fev1_uni$comparison$stats <- fev1_uni$lexicon$comparison$variable %>% 
    map(~explore(data = fev1_uni$analysis_tbl, 
                 split_factor = .x, 
                 variables = 'FEV1_percent', 
                 what = 'table',
                 pub_styled = TRUE)) %>% 
    map(compress, names_to = 'category') %>% 
    compress(names_to = 'split_factor') %>% 
    select(split_factor, category, statistic)
  
  ## test
  
  fev1_uni$comparison$test <- 
    list(split_factor = fev1_uni$lexicon$comparison$variable, 
         types = fev1_uni$lexicon$comparison$test_type) %>% 
    pmap(compare_variables, 
         fev1_uni$analysis_tbl, 
         variables = 'FEV1_percent', 
         what = 'eff_size', 
         exact = FALSE, 
         ci = FALSE, 
         pub_styled = TRUE) %>% 
    compress(names_to = 'split_factor') %>% 
    re_adjust %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           test = stri_replace(test, regex = '\\s{1}test$', replacement = ''))
  
  ## significant effects 
  
  fev1_uni$comparison$significant <- fev1_uni$comparison$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$split_factor
  
# Plots for the comparison ------
  
  insert_msg('Plots for the comparisons')
  
  fev1_uni$comparison$plots <- 
    list(split_factor = fev1_uni$lexicon$comparison$variable, 
         plot_title = capitalize_first_char(fev1_uni$lexicon$comparison$label), 
         plot_subtitle = fev1_uni$comparison$test$plot_cap) %>% 
    pmap(plot_variable, 
         fev1_uni$analysis_tbl, 
         variable = 'FEV1_percent', 
         type = 'box', 
         cust_theme = globals$common_theme, 
         y_lab = 'FEV1, % of reference', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Greens') + 
          theme(legend.position = 'none'))
  
# Result table for comparison --------
  
  insert_msg('Result table for comparison')
  
  fev1_uni$comparison$result_tbl <- 
    left_join(fev1_uni$comparison$stats,
              fev1_uni$comparison$test[c('split_factor', 
                                         'test', 
                                         'significance', 
                                         'eff_size')], 
              by = 'split_factor') %>% 
    set_names(c('variable', 'category', 'statistic', 
                'test', 'significance', 'eff_size')) %>% 
    format_summ_tbl(dict = fev1_uni$lexicon$comparison, 
                    value = 'table_label')
  
# Correlation analysis -------
  
  insert_msg('Correlation analysis')
  
  ## Spearman's rank test
  
  fev1_uni$correlation$test <- fev1_uni$lexicon$correlation$variable %>% 
    map_dfr(~correlate_variables(fev1_uni$analysis_tbl, 
                                 variables = c(.x, 'FEV1_percent'), 
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
  
  fev1_uni$correlation$plots <- 
    list(variables = map(fev1_uni$lexicon$correlation$variable, 
                         ~c(.x, 'FEV1_percent')), 
         plot_title = capitalize_first_char(fev1_uni$lexicon$correlation$label), 
         plot_subtitle = fev1_uni$correlation$test$plot_cap, 
         x_lab = paste0(fev1_uni$lexicon$correlation$y_lab, ', rank')) %>% 
    pmap(plot_correlation, 
         fev1_uni$analysis_tbl, 
         y_lab = 'DLCO, % of reference, rank', 
         cust_theme = globals$common_theme)
  
  ## variables as log-transformed ranks
  
  for(i in names(fev1_uni$correlation$plots)) {
    
    fev1_uni$correlation$plots[[i]]$data <- 
      fev1_uni$correlation$plots[[i]]$data %>% 
      map_dfc(rank)
    
  }
  
  fev1_uni$correlation$plots <- fev1_uni$correlation$plots %>% 
    map(~.x + 
          scale_x_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)) + 
          scale_y_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)))
  
# Bubble plots with correlation coefficients -----
  
  insert_msg('Bubble plots with correlation coefficients')
  
  fev1_uni$correlation$eff_plot <- 
    plot_eff_bubble(data = fev1_uni$correlation$test, 
                    x_var = 'variable1', 
                    y_var = 'variable2', 
                    plot_title = 'FEV1, explanatory factors', 
                    plot_subtitle = "Spearman's \u03C1") + 
    scale_x_discrete(labels = function(x) exchange(x, dlco_uni$lexicon$correlation)) + 
    scale_y_discrete(labels = function(x) exchange(x, covild$lft_lexicon)) + 
    scale_size_area(limits = c(0, 1), 
                    max_size = 6, 
                    name = expression('abs(' * rho * ')'))
  
# END -----
  
  rm(i)
  
  insert_tail()