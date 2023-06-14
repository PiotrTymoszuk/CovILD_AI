# Modeling of FEV1 with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of FEV1
# Unmatched CV folds: modeling simple presence/absence of FEV1

  insert_head()

# container ------

  fev1_rf <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  fev1_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fev1_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FEV1_percent, all_of(fev1_rf$variables))
  
  fev1_rf$formula <- lft_globals$formulas$FEV1_percent
  
  fev1_rf$train_control <- lft_globals$train_control

# tuning grid ------

  insert_msg('Tuning grids')
  
  fev1_rf$tune_grid <- 
    expand.grid(splitrule = c('variance', 'extratrees', 'maxstat'), 
                mtry = seq(2, length(fev1_rf$variables), by = 3), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)

# modeling ------

  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fev1_rf$models <- fev1_rf$train_control %>% 
    map(~train(form = fev1_rf$formula, 
               data = fev1_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'RMSE', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = fev1_rf$tune_grid))
  
  stopImplicitCluster()
  
  fev1_rf$models <- fev1_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------

  insert_msg('Extracting tuning results')
  
  fev1_rf$stats <- fev1_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  fev1_rf$optima <- fev1_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting tuning results ------

  insert_msg('Plotting tuning results')
  
  fev1_rf$plots <- 
    list(data = fev1_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_regression_tuning)

# Caching the results -------

  insert_msg('Caching the results')
  
  save(fev1_rf, file = './cache/fev1_rf.RData')

# END -----

insert_tail()