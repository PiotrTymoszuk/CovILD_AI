# Permutation importance, binary classifiers

  insert_head()
  
# container -----
  
  bin_importance <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## optimal tuning parameters needed to re-construct Random Forest models
  
  bin_importance$optima <- 
    c(lft_rf$optima, 
      dlco_red_rf$optima, 
      fvc_red_rf$optima, 
      fev1_red_rf$optima) %>% 
    set_names(c('lft_matched', 
                'lft_unmatched', 
                'dlco_matched', 
                'dlco_unmatched', 
                'fvc_matched', 
                'fvc_unmatched', 
                'fev1_matched', 
                'fev1_unmatched')) %>% 
    compress(names_to = 'model')
  
  ## model formulas
  
  bin_importance$formulas <- 
    list('lft_matched' = lft_globals$formulas$LFT_findings, 
         'lft_unmatched' = lft_globals$formulas$LFT_findings, 
         'dlco_matched' = lft_globals$formulas$DLCO_reduced, 
         'dlco_unmatched' = lft_globals$formulas$DLCO_reduced, 
         'fvc_matched' = lft_globals$formulas$FVC_reduced, 
         'fvc_unmatched' = lft_globals$formulas$FVC_reduced, 
         'fev1_matched' = lft_globals$formulas$FEV1_reduced, 
         'fev1_unmatched' = lft_globals$formulas$FEV1_reduced)
  
  ## lexicon
  
  bin_importance$lexicon <- 
    c('lft_matched' = 'LFT findings, matched', 
      'lft_unmatched' = 'LFT findings, unmatched', 
      'dlco_matched' = 'DLCO < 80%, matched', 
      'dlco_unmatched' = 'DLCO < 80%, unmatched', 
      'fvc_matched' = 'FVC < 80%, matched', 
      'fvc_unmatched' = 'FVC < 80%, unmatched', 
      'fev1_matched' = 'FEV1 < 80%, matched', 
      'fev1_unmatched' = 'FEV1 < 80%, unmatched') %>% 
    compress(names_to = 'variable', 
             values_to = 'label') %>% 
    mutate(label_short = stri_replace(label, regex = ',.*', replacement = ''))
  
# Ranger models -------
  
  insert_msg('Ranger models')
  
  bin_importance$models <- 
    list(formula = bin_importance$formulas, 
         mtry = bin_importance$optima$mtry, 
         splitrule = bin_importance$optima$splitrule, 
         min.node.size = bin_importance$optima$min.node.size) %>% 
    pmap(ranger, 
         data = lft_globals$analysis_tbl, 
         importance = 'permutation')
  
# Extraction of the importance stats ------
  
  insert_msg('Extracting the importance stats')
  
  bin_importance$stats <- bin_importance$models %>% 
    map(importance) %>% 
    map(compress, 
        names_to = 'variable', 
        values_to = 'importance')
  
# Testing for significance with the Altmann's permutation test ------
  
  insert_msg('Testing for significance of the importance stats')
  
  set.seed(12345)
  
  bin_importance$test <- 
    list(x = bin_importance$models, 
         formula = bin_importance$formulas) %>% 
    future_pmap(importance_pvalues, 
                method = 'altmann', 
                num.permutations = 100, 
                data = lft_globals$analysis_tbl, 
                .options = furrr_options(seed = TRUE))
  
  ## formatting: tibbles
  
  bin_importance$test <- bin_importance$test %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, 'variable') %>% 
    map(as_tibble) %>% 
    map(re_adjust, 
        p_variable = 'pvalue', 
        method = 'none')
  
# Plotting the importance stats -------
  
  insert_msg('Plots, Altmann test results')
  
  bin_importance$plots <- 
    list(data = bin_importance$test, 
         plot_title = names(bin_importance$test) %>% 
           exchange(bin_importance$lexicon)) %>% 
    pmap(plot_importance, 
         plot_subtitle = 'Permutation test, n = 100 draws', 
         adj_method = 'none') %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) exchange(x, lft_globals$lexicon)))
  
# Result table ------
  
  insert_msg('Result table')
  
  bin_importance$result_tbl <- bin_importance$test %>% 
    map(arrange, -importance) %>% 
    compress(names_to = 'model') %>% 
    mutate(model_type = stri_extract(model, regex = 'matched|unmatched'),  
           model = exchange(model, 
                            bin_importance$lexicon, 
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

# END -----
  
  plan('sequential')
  
  insert_tail()
    