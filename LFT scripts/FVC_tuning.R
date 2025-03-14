# Modeling of the FVC percentage of reference with 
# 1) Random Forests (Ranger), 
# 2) NNet: neural network with a single hidden layer
# 3) support vector machines with radial Kernel
# 4) Gradient Boosted Machines
#
# for SVM: the optimal value of sigma (width of the Gaussian kernel) is 
# determined via a test run of caret's train, i.e. internally by calling 
# `kernlab::sigest()`

  insert_head()
  
# container -------
  
  fvc_tune <- list()
  
# analysis globals, formula and train control object -----
  
  insert_msg('Analysis globals')
  
  fvc_tune$variables <- 
    lft_globals[c("ct_variables", 
                  "symptom_variables", 
                  "baseline_variables")] %>% 
    reduce(c)
  
  fvc_tune$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(FVC_percent, all_of(fvc_tune$variables))
  
  fvc_tune$formula <- lft_globals$formulas$FVC_percent
  
  fvc_tune$train_control <- lft_globals$train_control
  
  fvc_tune$train_control$summaryFunction <- caret::defaultSummary
  
# tuning grids -------
  
  insert_msg('Tuning grids')
  
  ## Ranger
  
  fvc_tune$tune_grids$ranger <- 
    expand.grid(splitrule = c('variance', 'extratrees'), 
                mtry = seq(2, length(fvc_tune$variables)/2, by = 2), 
                min.node.size = c(3, 5, 7), 
                stringsAsFactors = FALSE)
  
  ## nnet
  
  fvc_tune$tune_grids$nnet <- 
    expand.grid(size = seq(1, length(fvc_tune$variables)/2, by = 2), 
                decay = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1), 
                stringsAsFactors = FALSE)
  
  ## SVM radial
  
  fvc_tune$tune_grids$svmRadial <-
    expand.grid(sigma = 0.01691549, 
                C = seq(0.1, 2, by = 0.1))
  
  ## GBM
  
  fvc_tune$tune_grids$gbm <-
    expand.grid(n.trees = c(50, 100, 150, 200), 
                interaction.depth = 1:4, 
                shrinkage = 0.1, 
                n.minobsinnode = c(5, 10, 15))
  
# Modeling --------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  fvc_tune$models$ranger <- 
    train(form = fvc_tune$formula, 
          data = fvc_tune$analysis_tbl, 
          method = 'ranger', 
          metric = 'MAE', 
          trControl = fvc_tune$train_control, 
          tuneGrid = fvc_tune$tune_grids$ranger, 
          num.trees = 1000)
  
  fvc_tune$models$nnet <- 
    train(form = fvc_tune$formula, 
          data = fvc_tune$analysis_tbl, 
          method = 'nnet', 
          metric = 'MAE', 
          trControl = fvc_tune$train_control, 
          tuneGrid = fvc_tune$tune_grids$nnet, 
          linout = TRUE, 
          maxit = 500)
  
  fvc_tune$models$svmRadial <- 
    train(form = fvc_tune$formula, 
          data = fvc_tune$analysis_tbl, 
          method = 'svmRadial', 
          metric = 'MAE', 
          trControl = fvc_tune$train_control, 
          tuneGrid = fvc_tune$tune_grids$svmRadial)
  
  fvc_tune$models$gbm <- 
    train(form = fvc_tune$formula, 
          data = fvc_tune$analysis_tbl, 
          method = 'gbm', 
          metric = 'MAE', 
          trControl = fvc_tune$train_control, 
          tuneGrid = fvc_tune$tune_grids$gbm)

  stopImplicitCluster()
  
  fvc_tune$models <- fvc_tune$models %>% 
    map(as_caretx)
  
# Best tune parameters --------
  
  insert_msg('Best tune paramaters')
  
  fvc_tune$best_tunes <- fvc_tune$models %>% 
    map(~.x$bestTune)
  
# Plots for the tuning process ------
  
  insert_msg('Plots for the tuning process')
  
  fvc_tune$tuning_plots <- 
    list(x = fvc_tune$models, 
         y = list(plot_ranger_tuning, 
                  plot_nnet_tuning, 
                  plot_svm_tuning, 
                  plot_gbm_tuning),
         z = paste('Tuning: FVC,', 
                   globals$algo_labs[names(fvc_tune$models)])) %>% 
    pmap(function(x, y, z) y(x, plot_title = z))

# Plotting performance in the resamples ------
  
  insert_msg('Plotting kappa in the resamples')
  
  fvc_tune$resample_plots <- 
    list(model = fvc_tune$models, 
         plot_title = paste('Resample performance:', 
                            globals$algo_labs[names(fvc_tune$models)]), 
         color = globals$algo_colors[names(fvc_tune$models)]) %>% 
    pmap(plot_resample_kappa) %>% 
    map(~.x +
          geom_hline(yintercept = 0, 
                     linetype = 'dashed'))

# Caching the results -------  
  
  insert_msg('Caching the results')
  
  save(fvc_tune, file = './cache/fvc_tune.RData')
  
# END ------
  
  insert_tail()