# Modeling of the DLCO findings with Random Forests (Ranger)
# Matched CV folds: modeling trajectories of the DLCO findings
# Unmatched CV folds: modeling simple presence/absence of the DLCO findings

  insert_head()

# container ------

  dlco_red_rf <- list()

# analysis globals -------

  insert_msg('Analysis globals')
  
  dlco_red_rf$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  dlco_red_rf$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(DLCO_reduced, all_of(dlco_red_rf$variables))
  
  dlco_red_rf$formula <- lft_globals$formulas$DLCO_reduced
  
  dlco_red_rf$train_control <- lft_globals$train_control

# tuning grid ------

  insert_msg('Tuning grids')
  
  dlco_red_rf$tune_grid <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(dlco_red_rf$variables), by = 3), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)

# modeling ------

  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  dlco_red_rf$models <- dlco_red_rf$train_control %>% 
    map(~train(form = dlco_red_rf$formula, 
               data = dlco_red_rf$analysis_tbl, 
               method = 'ranger', 
               metric = 'Kappa', 
               trControl = .x, 
               num.trees = 1000, 
               tuneGrid = dlco_red_rf$tune_grid))
  
  stopImplicitCluster()
  
  dlco_red_rf$models <- dlco_red_rf$models %>% 
    map(as_caretx)

# Tuning results (kappa) ------

  insert_msg('Extracting tuning results')
  
  dlco_red_rf$stats <- dlco_red_rf$models %>% 
    map(~.x$results) %>% 
    map(as_tibble)
  
  dlco_red_rf$optima <- dlco_red_rf$models %>% 
    map(~.x$bestTune) %>% 
    map(as_tibble)

# Plotting tuning results ------

  insert_msg('Plotting tuning results')
  
  dlco_red_rf$plots <- 
    list(data = dlco_red_rf$stats, 
         plot_title = c('ID-matched models', 
                        'Unmatched models')) %>% 
    pmap(plot_binary_tuning)

# Caching the results -------

  insert_msg('Caching the results')
  
  save(dlco_red_rf, file = './cache/dlco_red_rf.RData')

# END -----

insert_tail()