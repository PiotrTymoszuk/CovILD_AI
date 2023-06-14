# Modeling of the FVC findings with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of the FVC findings
# Unmatched CV folds: modeling simple presence/absence of the LFT findings

  insert_head()
  
# container ------
  
  fvc_red_rf <- list()
  
# analysis globals -------
  
  insert_msg('Analysis globals')
  
  fvc_red_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fvc_red_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FVC_reduced, all_of(fvc_red_rf$variables))
  
  fvc_red_rf$formula <- lft_globals$formulas$FVC_reduced
  
  fvc_red_rf$train_control <- lft_globals$train_control
  
# tuning grid ------
  
  insert_msg('Tuning grids')
  
  fvc_red_rf$tune_grid <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(fvc_red_rf$variables), by = 3), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)
  
# modeling ------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fvc_red_rf$models <- fvc_red_rf$train_control %>% 
    map(~train(form = fvc_red_rf$formula, 
               data = fvc_red_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'Kappa', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = fvc_red_rf$tune_grid))
  
  stopImplicitCluster()
  
  fvc_red_rf$models <- fvc_red_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------
  
  insert_msg('Extracting tuning results')
  
  fvc_red_rf$stats <- fvc_red_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  fvc_red_rf$optima <- fvc_red_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)
  
# Plotting tuning results ------
  
  insert_msg('Plotting tuning results')
  
  fvc_red_rf$plots <- 
    list(data = fvc_red_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_binary_tuning)

# Caching the results -------
  
  insert_msg('Caching the results')
  
  save(fvc_red_rf, file = './cache/fvc_red_rf.RData')
  
# END -----
  
  insert_tail()