# Comparison of explanatory variables between observations with and without
# FVC abnormalities
# Numeric variables: Mann-Whitney test with r effect size statistic
# Categorical variables: Chi-squared test with Cramer's V effect size statistic

  insert_head()
  
# container -------
  
  fvc_red_uni <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  fvc_red_uni$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fvc_red_uni$lexicon <- lft_globals$lexicon %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 'cramer_v'), 
           plot_type = ifelse(format == 'numeric', 
                              'box', 'stack'), 
           y_lab = ifelse(format == 'numeric', 
                          unit, '% of strata')) %>% 
    filter(variable %in% fvc_red_uni$variables)
  
  fvc_red_uni$analysis_tbl <- 
    lft_globals$analysis_tbl[c('ID', 'FVC_reduced', fvc_red_uni$variables)] %>% 
    mutate(FVC_reduced = car::recode(FVC_reduced, 
                                      "'yes' = 'FVC low'; 'no' = 'FVC high'"), 
           FVC_reduced = factor(FVC_reduced, c('FVC high', 'FVC low')))
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  ## n numbers
  
  fvc_red_uni$n_numbers <- fvc_red_uni$analysis_tbl %>% 
    count(FVC_reduced)
  
  fvc_red_uni$n_caps <- 
    paste0('total: n = ', sum(fvc_red_uni$n_numbers$n), 
           ', events: n = ', fvc_red_uni$n_numbers$n[2])
  
  ## stats
  
  fvc_red_uni$stats <- fvc_red_uni$analysis_tbl %>% 
    explore(split_factor = 'FVC_reduced', 
            variables = fvc_red_uni$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'FVC \u2265 80%', 'FVC < 80%'))
  
# Testing --------
  
  insert_msg('Testing')
  
  fvc_red_uni$test <- fvc_red_uni$analysis_tbl %>% 
    compare_variables(variables = fvc_red_uni$lexicon$variable, 
                      split_factor = 'FVC_reduced', 
                      what = 'eff_size', 
                      types = fvc_red_uni$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = FALSE, 
                      adj_method = 'BH') %>% 
    mutate(eff_size = paste(estimate_name, signif(estimate, 2), sep = ' = '), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           test = ifelse(estimate_name == 'V', 
                         '\u03C7\u00B2', 'Mann-Whitney'))
  
# Significant factors ------
  
  fvc_red_uni$significant <- fvc_red_uni$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Plotting effect sizes -------
  
  insert_msg('Plotting effect sizes')
  
  fvc_red_uni$eff_size_plot <- fvc_red_uni$test %>% 
    mutate(variable = exchange(variable, fvc_red_uni$lexicon)) %>% 
    plot(cust_theme = globals$common_theme, 
         plot_title = 'FVC < 80%, explanatory factors', 
         show_labels = 'signif', 
         plot_subtitle = fvc_red_uni$n_caps, 
         point_alpha = 0.75, 
         point_color = c("gray60", "coral3")) + 
    geom_hline(yintercept = -log10(0.05), 
               linetype = 'dashed') + 
    labs(x = "Effect size, r or Cramer's V", 
         y = expression('-log'[10] * ' pFDR')) + 
    theme(plot.tag = element_blank())
  
# Single plots ------
  
  insert_msg('Single plots')
  
  fvc_red_uni$plots <- 
    list(variable = fvc_red_uni$lexicon$variable, 
         plot_title = capitalize_first_char(fvc_red_uni$lexicon$label), 
         plot_subtitle = fvc_red_uni$test$plot_cap, 
         y_lab = fvc_red_uni$lexicon$y_lab, 
         type = fvc_red_uni$lexicon$plot_type) %>% 
    pmap(plot_variable, 
         fvc_red_uni$analysis_tbl, 
         split_factor = 'FVC_reduced', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Reds')) %>% 
    set_names(fvc_red_uni$lexicon$variable)
  
# Result table --------
  
  insert_msg('Result table')
  
  fvc_red_uni$result_tbl <- 
    left_join(fvc_red_uni$stats, 
              fvc_red_uni$test[c('variable', 'test', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_summ_tbl(rm_n = TRUE, 
                    dict = lft_globals$lexicon, 
                    value = 'table_label') %>% 
    full_rbind(tibble(variable = 'Observations, n', 
                      `FVC ≥ 80%` = fvc_red_uni$n_numbers$n[1], 
                      `FVC < 80%` = fvc_red_uni$n_numbers$n[2]), .)
  
# END ------
  
  insert_tail()