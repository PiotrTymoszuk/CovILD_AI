# Modeling of the FEV1 findings with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of the FEV1 findings
# Unmatched CV folds: modeling simple presence/absence of the LFT findings

  insert_head()
  
# container ------
  
  fev1_red_rf <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  fev1_red_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fev1_red_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FEV1_reduced, all_of(fev1_red_rf$variables))
  
  fev1_red_rf$formula <- lft_globals$formulas$FEV1_reduced
  
  fev1_red_rf$train_control <- lft_globals$train_control
  
# tuning grid ------
  
  insert_msg('Tuning grids')
  
  fev1_red_rf$tune_grid <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(fev1_red_rf$variables), by = 3), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)
  
# modeling ------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fev1_red_rf$models <- fev1_red_rf$train_control %>% 
    map(~train(form = fev1_red_rf$formula, 
               data = fev1_red_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'Kappa', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = fev1_red_rf$tune_grid))
  
  stopImplicitCluster()
  
  fev1_red_rf$models <- fev1_red_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------
  
  insert_msg('Extracting tuning results')
  
  fev1_red_rf$stats <- fev1_red_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  fev1_red_rf$optima <- fev1_red_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting tuning results ------
  
  insert_msg('Plotting tuning results')
  
  fev1_red_rf$plots <- 
    list(data = fev1_red_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_binary_tuning)

# Caching the results -------
  
  insert_msg('Caching the results')
  
  save(fev1_red_rf, file = './cache/fev1_red_rf.RData')
  
# END -----
  
  insert_tail()