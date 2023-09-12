# Modeling of the FEV1 percentage of reference with 
# 1) Random Forests (Ranger), 
# 2) NNet: neural network with a single hidden layer
# 3) support vector machines with radial Kernel

  insert_head()
  
# container -------
  
  fev1_tune <- list()
  
# analysis globals, formula and train control object -----
  
  insert_msg('Analysis globals')
  
  fev1_tune$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fev1_tune$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FEV1_percent, all_of(fev1_tune$variables))
  
  fev1_tune$formula <- lft_globals$formulas$FEV1_percent
  
  fev1_tune$train_control <- lft_globals$train_control
  
# tuning grids -------
  
  insert_msg('Tuning grids')
  
  ## Ranger
  
  fev1_tune$tune_grids$ranger <- 
    expand.grid(splitrule = c('variance', 'extratrees'), 
                mtry = seq(2, length(fev1_tune$variables)/2, by = 2), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)
  
  ## nnet
  
  fev1_tune$tune_grids$nnet <- 
    expand.grid(size = seq(1, length(fev1_tune$variables)/2, by = 2), 
                decay = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1), 
                stringsAsFactors = FALSE)
  
  ## SVM radial
  
  fev1_tune$tune_grids$svmRadial <-
    expand.grid(sigma = 0.01691549, 
                C = seq(0.1, 2, by = 0.1))
  
  ## GBM
  
  fev1_tune$tune_grids$gbm <-
    expand.grid(n.trees = c(50, 100, 150, 200), 
                interaction.depth = 1:4, 
                shrinkage = 0.1, 
                n.minobsinnode = c(5, 10, 15))
  
# Modeling --------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fev1_tune$models$ranger <- 
    train(form = fev1_tune$formula, 
          data = fev1_tune$analysis_tbl, 
          method = 'ranger', 
          metric = 'MAE', 
          trControl = fev1_tune$train_control, 
          tuneGrid = fev1_tune$tune_grids$ranger, 
          num.trees = 1000)
  
  fev1_tune$models$nnet <- 
    train(form = fev1_tune$formula, 
          data = fev1_tune$analysis_tbl, 
          method = 'nnet', 
          metric = 'MAE', 
          trControl = fev1_tune$train_control, 
          tuneGrid = fev1_tune$tune_grids$nnet, 
          linout = TRUE, 
          maxit = 500)
  
  fev1_tune$models$svmRadial <- 
    train(form = fev1_tune$formula, 
          data = fev1_tune$analysis_tbl, 
          method = 'svmRadial', 
          metric = 'MAE', 
          trControl = fev1_tune$train_control, 
          tuneGrid = fev1_tune$tune_grids$svmRadial)
  
  fev1_tune$models$gbm <- 
    train(form = fev1_tune$formula, 
          data = fev1_tune$analysis_tbl, 
          method = 'gbm', 
          metric = 'MAE', 
          trControl = fev1_tune$train_control, 
          tuneGrid = fev1_tune$tune_grids$gbm)

  stopImplicitCluster()
  
  fev1_tune$models <- fev1_tune$models %>% 
    map(as_caretx)
  
# Best tune parameters --------
  
  insert_msg('Best tune paramaters')
  
  fev1_tune$best_tunes <- fev1_tune$models %>% 
    map(~.x$bestTune)
  
# Plots for the tuning process ------
  
  insert_msg('Plots for the tuning process')
  
  fev1_tune$tuning_plots <- 
    list(x = fev1_tune$models, 
         y = list(plot_ranger_tuning, 
                  plot_nnet_tuning, 
                  plot_svm_tuning, 
                  plot_gbm_tuning),
         z = paste('Tuning: FEV1,', 
                   globals$algo_labs[names(fev1_tune$models)])) %>% 
    pmap(function(x, y, z) y(x, plot_title = z))

# Plotting performance in the resamples ------
  
  insert_msg('Plotting kappa in the resamples')
  
  fev1_tune$resample_plots <- 
    list(model = fev1_tune$models, 
         plot_title = paste('Resample performance:', 
                            globals$algo_labs[names(fev1_tune$models)]), 
         color = globals$algo_colors[names(fev1_tune$models)]) %>% 
    pmap(plot_resample_kappa) %>% 
    map(~.x +
          geom_hline(yintercept = 0, 
                     linetype = 'dashed'))

# Caching the results -------  
  
  insert_msg('Caching the results')
  
  save(fev1_tune, file = './cache/fev1_tune.RData')
  
# END ------
  
  insert_tail()