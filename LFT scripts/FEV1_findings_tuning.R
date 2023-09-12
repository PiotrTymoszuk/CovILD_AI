# Modeling of the FEV1 findings with 
# 1) Random Forests (Ranger), 
# 2) NNet: neural network with a single hidden layer
# 3) support vector machines with radial Kernel

  insert_head()
  
# container -------
  
  fev1_red_tune <- list()
  
# analysis globals, formula and train control object -----
  
  insert_msg('Analysis globals')
  
  fev1_red_tune$variables <- 
    lft_globals[c("ct_variables", "baseline_variables")] %>% 
    reduce(c)
  
  fev1_red_tune$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FEV1_reduced, all_of(fev1_red_tune$variables))
  
  fev1_red_tune$formula <- lft_globals$formulas$FEV1_reduced
  
  fev1_red_tune$train_control <- lft_globals$train_control
  
# tuning grids -------
  
  insert_msg('Tuning grids')
  
  ## Ranger
  
  fev1_red_tune$tune_grids$ranger <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(fev1_red_tune$variables)/2, by = 2), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)
  
  ## nnet
  
  fev1_red_tune$tune_grids$nnet <- 
    expand.grid(size = seq(1, length(fev1_red_tune$variables)/2, by = 2), 
                decay = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1), 
                stringsAsFactors = FALSE)
  
  ## SVM radial
  
  fev1_red_tune$tune_grids$svmRadial <-
    expand.grid(sigma = 0.01529346, 
                C = seq(0.1, 2, by = 0.1))
  
  ## GBM
  
  fev1_red_tune$tune_grids$gbm <-
    expand.grid(n.trees = c(50, 100, 150, 200), 
                interaction.depth = 1:4, 
                shrinkage = 0.1, 
                n.minobsinnode = c(5, 10, 15))
  
# Modeling --------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fev1_red_tune$models$ranger <- 
    train(form = fev1_red_tune$formula, 
          data = fev1_red_tune$analysis_tbl, 
          method = 'ranger', 
          metric = 'J', 
          trControl = fev1_red_tune$train_control, 
          tuneGrid = fev1_red_tune$tune_grids$ranger, 
          num.trees = 1000)
  
  fev1_red_tune$models$nnet <- 
    train(form = fev1_red_tune$formula, 
          data = fev1_red_tune$analysis_tbl, 
          method = 'nnet', 
          metric = 'J', 
          trControl = fev1_red_tune$train_control, 
          tuneGrid = fev1_red_tune$tune_grids$nnet, 
          maxit = 500)
  
  fev1_red_tune$models$svmRadial <- 
    train(form = fev1_red_tune$formula, 
          data = fev1_red_tune$analysis_tbl, 
          method = 'svmRadial', 
          metric = 'J', 
          trControl = fev1_red_tune$train_control, 
          tuneGrid = fev1_red_tune$tune_grids$svmRadial)
  
  fev1_red_tune$models$gbm <- 
    train(form = fev1_red_tune$formula, 
          data = fev1_red_tune$analysis_tbl, 
          method = 'gbm', 
          metric = 'J', 
          trControl = fev1_red_tune$train_control, 
          tuneGrid = fev1_red_tune$tune_grids$gbm)

  stopImplicitCluster()
  
  fev1_red_tune$models <- fev1_red_tune$models %>% 
    map(as_caretx)
  
# Best tune parameters --------
  
  insert_msg('Best tune paramaters')
  
  fev1_red_tune$best_tunes <- fev1_red_tune$models %>% 
    map(~.x$bestTune)
  
# Plots for the tuning process ------
  
  insert_msg('Plots for the tuning process')
  
  fev1_red_tune$tuning_plots <- 
    list(x = fev1_red_tune$models, 
         y = list(plot_ranger_tuning, 
                  plot_nnet_tuning, 
                  plot_svm_tuning, 
                  plot_gbm_tuning),
         z = paste('Tuning: reduced FEV1,', 
                   globals$algo_labs[names(fev1_red_tune$models)])) %>% 
    pmap(function(x, y, z) y(x, plot_title = z))

# Plotting performance in the resamples ------
  
  insert_msg('Plotting kappa in the resamples')
  
  fev1_red_tune$resample_plots <- 
    list(model = fev1_red_tune$models, 
         plot_title = paste('Resample performance:', 
                            globals$algo_labs[names(fev1_red_tune$models)]), 
         color = globals$algo_colors[names(fev1_red_tune$models)]) %>% 
    pmap(plot_resample_kappa) %>% 
    map(~.x +
          geom_hline(yintercept = 0, 
                     linetype = 'dashed'))

# Caching the results -------  
  
  insert_msg('Caching the results')
  
  save(fev1_red_tune, file = './cache/fev1_red_tune.RData')
  
# END ------
  
  insert_tail()