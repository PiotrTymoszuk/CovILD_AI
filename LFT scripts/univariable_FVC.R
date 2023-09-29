# Association of explanatory variables with FVC expressed as percentage 
# of reference value
# Numeric variables: Spearman's correlation
# Categorical variables: comparison of FVC values between the categories 
# by Mann-Whitney or Kruskal-Wallis test.

  insert_head()

# container -------

  fvc_uni <- list()

# analysis globals -------

insert_msg('Analysis globals')
  
  ## variables and analysis table

  fvc_uni$variables <- 
    lft_globals[c("ct_variables", 
                  "symptom_variables", 
                  "baseline_variables")] %>% 
    reduce(c)

  fvc_uni$analysis_tbl <- 
    lft_globals$analysis_tbl[c('ID', 'FVC_percent', fvc_uni$variables)]
  
  ## variable lexicon, appended with the plot type
  ## and type of the statistical test
  
  fvc_uni$lexicon <- lft_globals$lexicon %>% 
    filter(variable %in% fvc_uni$variables)
  
  fvc_uni$lexicon$level_no <- 
    fvc_uni$analysis_tbl[fvc_uni$lexicon$variable] %>% 
    map(levels) %>% 
    map_dbl(length)
  
  fvc_uni$lexicon <- fvc_uni$lexicon %>% 
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
  
  fvc_uni$lexicon$variable <- 
    set_names(fvc_uni$lexicon$variable, 
              fvc_uni$lexicon$variable)
  
  fvc_uni$lexicon <- fvc_uni$lexicon %>% 
    blast(analysis_type)

# Comparison -------
  
  insert_msg('Comparison')
  
  ## descriptive stats
  
  fvc_uni$comparison$stats <- fvc_uni$lexicon$comparison$variable %>% 
    map(~explore(data = fvc_uni$analysis_tbl, 
                 split_factor = .x, 
                 variables = 'FVC_percent', 
                 what = 'table',
                 pub_styled = TRUE)) %>% 
    map(compress, names_to = 'category') %>% 
    compress(names_to = 'split_factor') %>% 
    select(split_factor, category, statistic)
  
  ## test
  
  fvc_uni$comparison$test <- 
    list(split_factor = fvc_uni$lexicon$comparison$variable, 
         types = fvc_uni$lexicon$comparison$test_type) %>% 
    pmap(compare_variables, 
         fvc_uni$analysis_tbl, 
         variables = 'FVC_percent', 
         what = 'eff_size', 
         exact = FALSE, 
         ci = FALSE, 
         pub_styled = TRUE) %>% 
    compress(names_to = 'split_factor') %>% 
    re_adjust %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '), 
           test = stri_replace(test, regex = '\\s{1}test$', replacement = ''))
  
  ## significant effects 
  
  fvc_uni$comparison$significant <- fvc_uni$comparison$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$split_factor
  
# Plots for the comparison ------
  
  insert_msg('Plots for the comparisons')
  
  fvc_uni$comparison$plots <- 
    list(split_factor = fvc_uni$lexicon$comparison$variable, 
         plot_title = capitalize_first_char(fvc_uni$lexicon$comparison$label), 
         plot_subtitle = fvc_uni$comparison$test$plot_cap) %>% 
    pmap(plot_variable, 
         fvc_uni$analysis_tbl, 
         variable = 'FVC_percent', 
         type = 'box', 
         cust_theme = globals$common_theme, 
         y_lab = 'FVC, % of reference', 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Greens') + 
          theme(legend.position = 'none'))
  
# Result table for comparison --------
  
  insert_msg('Result table for comparison')
  
  fvc_uni$comparison$result_tbl <- 
    left_join(fvc_uni$comparison$stats,
              fvc_uni$comparison$test[c('split_factor', 
                                         'test', 
                                         'significance', 
                                         'eff_size')], 
              by = 'split_factor') %>% 
    set_names(c('variable', 'category', 'statistic', 
                'test', 'significance', 'eff_size')) %>% 
    format_summ_tbl(dict = fvc_uni$lexicon$comparison, 
                    value = 'table_label')
  
# Correlation analysis -------
  
  insert_msg('Correlation analysis')
  
  ## Spearman's rank test
  
  fvc_uni$correlation$test <- fvc_uni$lexicon$correlation$variable %>% 
    map_dfr(~correlate_variables(fvc_uni$analysis_tbl, 
                                 variables = c(.x, 'FVC_percent'), 
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
  
  fvc_uni$correlation$plots <- 
    list(variables = map(fvc_uni$lexicon$correlation$variable, 
                         ~c(.x, 'FVC_percent')), 
         plot_title = capitalize_first_char(fvc_uni$lexicon$correlation$label), 
         plot_subtitle = fvc_uni$correlation$test$plot_cap, 
         x_lab = paste0(fvc_uni$lexicon$correlation$y_lab, ', rank')) %>% 
    pmap(plot_correlation, 
         fvc_uni$analysis_tbl, 
         y_lab = 'DLCO, % of reference, rank', 
         cust_theme = globals$common_theme)
  
  ## variables as log-transformed ranks
  
  for(i in names(fvc_uni$correlation$plots)) {
    
    fvc_uni$correlation$plots[[i]]$data <- 
      fvc_uni$correlation$plots[[i]]$data %>% 
      map_dfc(rank)
    
  }
  
  fvc_uni$correlation$plots <- fvc_uni$correlation$plots %>% 
    map(~.x + 
          scale_x_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)) + 
          scale_y_continuous(trans = 'log', 
                             labels = function(x) signif(x, 2)))
  
# Bubble plots with correlation coefficients -----
  
  insert_msg('Bubble plots with correlation coefficients')
  
  fvc_uni$correlation$eff_plot <- 
    plot_eff_bubble(data = fvc_uni$correlation$test, 
                    x_var = 'variable1', 
                    y_var = 'variable2', 
                    plot_title = 'FVC, explanatory factors', 
                    plot_subtitle = "Spearman's \u03C1") + 
    scale_x_discrete(labels = function(x) exchange(x, dlco_uni$lexicon$correlation)) + 
    scale_y_discrete(labels = function(x) exchange(x, covild$lft_lexicon)) + 
    scale_size_area(limits = c(0, 1), 
                    max_size = 6, 
                    name = expression('abs(' * rho * ')'))
  
# END -----
  
  rm(i)
  
  insert_tail()
  
  
  
  
    

  
  
