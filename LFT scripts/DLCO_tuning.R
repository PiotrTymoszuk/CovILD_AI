# Modeling of the DLCO percentage of reference with 
# 1) Random Forests (Ranger), 
# 2) NNet: neural network with a single hidden layer
# 3) support vector machines with radial Kernel

  insert_head()
  
# container -------
  
  dlco_tune <- list()
  
# analysis globals, formula and train control object -----
  
  insert_msg('Analysis globals')
  
  dlco_tune$variables <- 
    lft_globals[c("ct_variables", 
                  "symptom_variables", 
                  "baseline_variables")] %>% 
    reduce(c)
  
  dlco_tune$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(DLCO_percent, all_of(dlco_tune$variables))
  
  dlco_tune$formula <- lft_globals$formulas$DLCO_percent
  
  dlco_tune$train_control <- lft_globals$train_control
  
  dlco_tune$train_control$summaryFunction <- caret::defaultSummary
  
# tuning grids -------
  
  insert_msg('Tuning grids')
  
  ## Ranger
  
  dlco_tune$tune_grids$ranger <- 
    expand.grid(splitrule = c('variance', 'extratrees'), 
                mtry = seq(2, length(dlco_tune$variables)/2, by = 2), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)
  
  ## nnet
  
  dlco_tune$tune_grids$nnet <- 
    expand.grid(size = seq(1, length(dlco_tune$variables)/2, by = 2), 
                decay = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1), 
                stringsAsFactors = FALSE)
  
  ## SVM radial
  
  dlco_tune$tune_grids$svmRadial <-
    expand.grid(sigma = 0.01691549, 
                C = seq(0.1, 2, by = 0.1))
  
  ## GBM
  
  dlco_tune$tune_grids$gbm <-
    expand.grid(n.trees = c(50, 100, 150, 200), 
                interaction.depth = 1:4, 
                shrinkage = 0.1, 
                n.minobsinnode = c(5, 10, 15))
  
# Modeling --------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  dlco_tune$models$ranger <- 
    train(form = dlco_tune$formula, 
          data = dlco_tune$analysis_tbl, 
          method = 'ranger', 
          metric = 'MAE', 
          trControl = dlco_tune$train_control, 
          tuneGrid = dlco_tune$tune_grids$ranger, 
          num.trees = 1000)
  
  dlco_tune$models$nnet <- 
    train(form = dlco_tune$formula, 
          data = dlco_tune$analysis_tbl, 
          method = 'nnet', 
          metric = 'MAE', 
          trControl = dlco_tune$train_control, 
          tuneGrid = dlco_tune$tune_grids$nnet, 
          linout = TRUE, 
          maxit = 500)
  
  dlco_tune$models$svmRadial <- 
    train(form = dlco_tune$formula, 
          data = dlco_tune$analysis_tbl, 
          method = 'svmRadial', 
          metric = 'MAE', 
          trControl = dlco_tune$train_control, 
          tuneGrid = dlco_tune$tune_grids$svmRadial)
  
  dlco_tune$models$gbm <- 
    train(form = dlco_tune$formula, 
          data = dlco_tune$analysis_tbl, 
          method = 'gbm', 
          metric = 'MAE', 
          trControl = dlco_tune$train_control, 
          tuneGrid = dlco_tune$tune_grids$gbm)

  stopImplicitCluster()
  
  dlco_tune$models <- dlco_tune$models %>% 
    map(as_caretx)
  
# Best tune parameters --------
  
  insert_msg('Best tune paramaters')
  
  dlco_tune$best_tunes <- dlco_tune$models %>% 
    map(~.x$bestTune)
  
# Plots for the tuning process ------
  
  insert_msg('Plots for the tuning process')
  
  dlco_tune$tuning_plots <- 
    list(x = dlco_tune$models, 
         y = list(plot_ranger_tuning, 
                  plot_nnet_tuning, 
                  plot_svm_tuning, 
                  plot_gbm_tuning),
         z = paste('Tuning: DLCO,', 
                   globals$algo_labs[names(dlco_tune$models)])) %>% 
    pmap(function(x, y, z) y(x, plot_title = z))

# Plotting performance in the resamples ------
  
  insert_msg('Plotting kappa in the resamples')
  
  dlco_tune$resample_plots <- 
    list(model = dlco_tune$models, 
         plot_title = paste('Resample performance:', 
                            globals$algo_labs[names(dlco_tune$models)]), 
         color = globals$algo_colors[names(dlco_tune$models)]) %>% 
    pmap(plot_resample_kappa) %>% 
    map(~.x +
          geom_hline(yintercept = 0, 
                     linetype = 'dashed'))

# Caching the results -------  
  
  insert_msg('Caching the results')
  
  save(dlco_tune, file = './cache/dlco_tune.RData')
  
# END ------
  
  insert_tail()