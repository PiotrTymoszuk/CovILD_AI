# Modeling of FVC with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of FVC
# Unmatched CV folds: modeling simple presence/absence of FVC

  insert_head()

# container ------

  fvc_rf <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  fvc_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fvc_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FVC_percent, all_of(fvc_rf$variables))
  
  fvc_rf$formula <- lft_globals$formulas$FVC_percent
  
  fvc_rf$train_control <- lft_globals$train_control

# tuning grid ------

  insert_msg('Tuning grids')
  
  fvc_rf$tune_grid <- 
    expand.grid(splitrule = c('variance', 'extratrees', 'maxstat'), 
                mtry = seq(2, length(fvc_rf$variables), by = 3), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)

# modeling ------

  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fvc_rf$models <- fvc_rf$train_control %>% 
    map(~train(form = fvc_rf$formula, 
               data = fvc_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'RMSE', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = fvc_rf$tune_grid))
  
  stopImplicitCluster()
  
  fvc_rf$models <- fvc_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------

  insert_msg('Extracting tuning results')
  
  fvc_rf$stats <- fvc_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  fvc_rf$optima <- fvc_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting tuning results ------

  insert_msg('Plotting tuning results')
  
  fvc_rf$plots <- 
    list(data = fvc_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_regression_tuning)

# Caching the results -------

  insert_msg('Caching the results')
  
  save(fvc_rf, file = './cache/fvc_rf.RData')

# END -----

insert_tail()