# Modeling of DLCO with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of DLCO
# Unmatched CV folds: modeling simple presence/absence of DLCO

  insert_head()

# container ------

  dlco_rf <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  dlco_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  dlco_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(DLCO_percent, all_of(dlco_rf$variables))
  
  dlco_rf$formula <- lft_globals$formulas$DLCO_percent
  
  dlco_rf$train_control <- lft_globals$train_control

# tuning grid ------

  insert_msg('Tuning grids')
  
  dlco_rf$tune_grid <- 
    expand.grid(splitrule = c('variance', 'extratrees', 'maxstat'), 
                mtry = seq(2, length(dlco_rf$variables), by = 3), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)

# modeling ------

  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  dlco_rf$models <- dlco_rf$train_control %>% 
    map(~train(form = dlco_rf$formula, 
               data = dlco_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'RMSE', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = dlco_rf$tune_grid))
  
  stopImplicitCluster()
  
  dlco_rf$models <- dlco_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------

  insert_msg('Extracting tuning results')
  
  dlco_rf$stats <- dlco_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  dlco_rf$optima <- dlco_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting tuning results ------

  insert_msg('Plotting tuning results')
  
  dlco_rf$plots <- 
    list(data = dlco_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_regression_tuning)

# Caching the results -------

  insert_msg('Caching the results')
  
  save(dlco_rf, file = './cache/dlco_rf.RData')

# END -----

insert_tail()