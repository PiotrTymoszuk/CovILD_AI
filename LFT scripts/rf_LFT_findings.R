# Modeling of the LF findings with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of the LFT findings
# Unmatched CV folds: modeling simple presence/absence of the LFT findings

  insert_head()
  
# container ------
  
  lft_rf <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  lft_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  lft_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(LFT_findings, all_of(lft_rf$variables))
  
  lft_rf$formula <- lft_globals$formulas$LFT_findings
  
  lft_rf$train_control <- lft_globals$train_control
  
# tuning grid ------
  
  insert_msg('Tuning grids')
  
  lft_rf$tune_grid <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(lft_rf$variables), by = 3), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)
  
# modeling ------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  lft_rf$models <- lft_rf$train_control %>% 
    map(~train(form = lft_rf$formula, 
               data = lft_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'Kappa', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = lft_rf$tune_grid))
  
  stopImplicitCluster()
  
  lft_rf$models <- lft_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------
  
  insert_msg('Extracting tuning results')
  
  lft_rf$stats <- lft_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  lft_rf$optima <- lft_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting tuning results ------
  
  insert_msg('Plotting tuning results')
  
  lft_rf$plots <- 
    list(data = lft_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_binary_tuning)

# Caching the results -------
  
  insert_msg('Caching the results')
  
  save(lft_rf, file = './cache/lft_rf.RData')
  
# END -----
  
  insert_tail()