# Comparison of explanatory variables between observations with and without
# LFT abnormalities
# Numeric variables: Mann-Whitney test with r effect size statistic
# Categorical variables: Chi-squared test with Cramer's V effect size statistic

  insert_head()
  
# container -------
  
  lft_uni <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  lft_uni$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  lft_uni$lexicon <- lft_globals$lexicon %>% 
    mutate(test_type = ifelse(format == 'numeric', 
                              'wilcoxon_r', 'cramer_v'), 
           plot_type = ifelse(format == 'numeric', 
                              'box', 'stack'), 
           y_lab = ifelse(format == 'numeric', 
                          unit, '% of strata')) %>% 
    filter(variable %in% lft_uni$variables)
  
  lft_uni$analysis_tbl <- 
    lft_globals$analysis_tbl[c('ID', 'LFT_findings', lft_uni$variables)] %>% 
    mutate(LFT_findings = car::recode(LFT_findings, 
                                      "'yes' = 'LFT+'; 'no' = 'LFT-'"), 
           LFT_findings = factor(LFT_findings, c('LFT-', 'LFT+')))
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  ## n numbers
  
  lft_uni$n_numbers <- lft_uni$analysis_tbl %>% 
    count(LFT_findings)
  
  lft_uni$n_caps <- 
    paste0('total: n = ', sum(lft_uni$n_numbers$n), 
           ', events: n = ', lft_uni$n_numbers$n[2])
  
  ## stats
  
  lft_uni$stats <- lft_uni$analysis_tbl %>% 
    explore(split_factor = 'LFT_findings', 
            variables = lft_uni$lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'LFT findings absent', 'LFT findings present'))
  
# Testing --------
  
  insert_msg('Testing')
  
  lft_uni$test <- lft_uni$analysis_tbl %>% 
    compare_variables(variables = lft_uni$lexicon$variable, 
                      split_factor = 'LFT_findings', 
                      what = 'eff_size', 
                      types = lft_uni$lexicon$test_type, 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = FALSE, 
                      adj_method = 'BH') %>% 
    mutate(eff_size = paste(estimate_name, signif(estimate, 2), sep = ' = '), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           test = ifelse(estimate_name == 'V', 
                         '\u03C7\u00B2', 'Mann-Whitney'))
  
# Significant factors ------
  
  lft_uni$significant <- lft_uni$test %>% 
    filter(p_adjusted < 0.05) %>% 
    .$variable
  
# Plotting effect sizes -------
  
  insert_msg('Plotting effect sizes')
  
  lft_uni$eff_size_plot <- lft_uni$test %>% 
    mutate(variable = exchange(variable, lft_uni$lexicon)) %>% 
    plot(cust_theme = globals$common_theme, 
         plot_title = 'LFT findings, explanatory factors', 
         show_labels = 'signif', 
         plot_subtitle = lft_uni$n_caps, 
         point_alpha = 0.75, 
         point_color = c("gray60", "coral3")) + 
    geom_hline(yintercept = -log10(0.05), 
               linetype = 'dashed') + 
    labs(x = "Effect size, r or Cramer's V", 
         y = expression('-log'[10] * ' pFDR')) + 
    theme(plot.tag = element_blank())
  
# Single plots ------
  
  insert_msg('Single plots')
  
  lft_uni$plots <- 
    list(variable = lft_uni$lexicon$variable, 
         plot_title = capitalize_first_char(lft_uni$lexicon$label), 
         plot_subtitle = lft_uni$test$plot_cap, 
         y_lab = lft_uni$lexicon$y_lab, 
         type = lft_uni$lexicon$plot_type) %>% 
    pmap(plot_variable, 
         lft_uni$analysis_tbl, 
         split_factor = 'LFT_findings', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Reds')) %>% 
    set_names(lft_uni$lexicon$variable)
  
# Result table --------
  
  insert_msg('Result table')
  
  lft_uni$result_tbl <- 
    left_join(lft_uni$stats, 
              lft_uni$test[c('variable', 'test', 'significance', 'eff_size')], 
              by = 'variable') %>% 
    format_summ_tbl(rm_n = TRUE, 
                    dict = lft_globals$lexicon, 
                    value = 'table_label') %>% 
    full_rbind(tibble(variable = 'Observations, n', 
                      `LFT findings absent` = lft_uni$n_numbers$n[1], 
                      `LFT findings present` = lft_uni$n_numbers$n[2]), .)
  
# END ------
  
  insert_tail()