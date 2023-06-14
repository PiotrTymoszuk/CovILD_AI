# Permutation importance of explanatory variables for regression 
# Random Forest models

  insert_head()
  
# container -------
  
  reg_importance <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## optimal tuning parameters needed to re-construct Random Forest models
  
  reg_importance$optima <- 
    c(dlco_rf$optima, 
      fvc_rf$optima, 
      fev1_rf$optima) %>% 
    set_names(c('dlco_matched', 
                'dlco_unmatched', 
                'fvc_matched', 
                'fvc_unmatched', 
                'fev1_matched', 
                'fev1_unmatched')) %>% 
    compress(names_to = 'model')
  
  ## model formulas
  
  reg_importance$formulas <- 
    list('dlco_matched' = lft_globals$formulas$DLCO_percent, 
         'dlco_unmatched' = lft_globals$formulas$DLCO_percent, 
         'fvc_matched' = lft_globals$formulas$FVC_percent, 
         'fvc_unmatched' = lft_globals$formulas$FVC_percent, 
         'fev1_matched' = lft_globals$formulas$FEV1_percent, 
         'fev1_unmatched' = lft_globals$formulas$FEV1_percent)
  
  ## lexicon
  
  reg_importance$lexicon <- 
    c('dlco_matched' = 'DLCO, matched', 
      'dlco_unmatched' = 'DLCO, unmatched', 
      'fvc_matched' = 'FVC, matched', 
      'fvc_unmatched' = 'FVC, unmatched', 
      'fev1_matched' = 'FEV1, matched', 
      'fev1_unmatched' = 'FEV1, unmatched') %>% 
    compress(names_to = 'variable', 
             values_to = 'label') %>% 
    mutate(label_short = stri_replace(label, regex = ',.*', replacement = ''))
  
# Ranger models -----
  
  insert_msg('Ranger models')
  
  reg_importance$models <- 
    list(formula = reg_importance$formulas, 
         mtry = reg_importance$optima$mtry, 
         splitrule = reg_importance$optima$splitrule, 
         min.node.size = reg_importance$optima$min.node.size) %>% 
    pmap(ranger, 
         data = lft_globals$analysis_tbl, 
         importance = 'permutation')
  
# Extraction of the importance stats -----
  
  insert_msg('Extraction of the importance stats')
  
  reg_importance$stats <- reg_importance$models %>% 
    map(importance) %>% 
    map(compress, 
        names_to = 'variable', 
        values_to = 'importance')
  
# Permutation test ------
  
  insert_msg('Testing for significance of the importance stats')
  
  set.seed(12345)
  
  reg_importance$test <- 
    list(x = reg_importance$models, 
         formula = reg_importance$formulas) %>% 
    future_pmap(importance_pvalues, 
                method = 'altmann', 
                num.permutations = 100, 
                data = lft_globals$analysis_tbl, 
                .options = furrr_options(seed = TRUE))
  
  ## formatting: tibbles
  
  reg_importance$test <- reg_importance$test %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(as_tibble) %>% 
    map(re_adjust, 
        p_variable = 'pvalue', 
        method = 'none')
  
# Plotting the importance stats -------
  
  insert_msg('Plots, Altmann test results')
  
  reg_importance$plots <- 
    list(data = reg_importance$test, 
         plot_title = names(reg_importance$test) %>% 
           exchange(reg_importance$lexicon)) %>% 
    pmap(plot_importance, 
         plot_subtitle = 'Permutation test, n = 100 draws', 
         adj_method = 'none') %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) exchange(x, lft_globals$lexicon)))
  
# Result table ------
  
  insert_msg('Result table')
  
  reg_importance$result_tbl <- reg_importance$test %>% 
    map(arrange, -importance) %>% 
    compress(names_to = 'model') %>% 
    mutate(model_type = stri_extract(model, regex = 'matched|unmatched'),  
           model = exchange(model, 
                            reg_importance$lexicon, 
                            value = 'label_short'), 
           variable = exchange(variable, 
                               lft_globals$lexicon), 
           importance = signif(importance, 2)) %>% 
    select(model, model_type, variable, importance, significance) %>% 
    set_names(c('Response', 
                'Participant matching', 
                'Explanatory variable', 
                'Importance statsitic', 
                'Significance'))
  
# END ------
  
  insert_tail()