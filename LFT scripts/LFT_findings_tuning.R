# Modeling of the LF findings with 
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
  
  lft_tune <- list()
  
# analysis globals, formula and train control object -----
  
  insert_msg('Analysis globals')
  
  lft_tune$variables <- 
    lft_globals[c("ct_variables", 
                  "symptom_variables", 
                  "baseline_variables")] %>% 
    reduce(c)
  
  lft_tune$analysis_tbl <- lft_globals$analysis_tbl %>% 
    select(LFT_findings, all_of(lft_tune$variables))
  
  lft_tune$formula <- lft_globals$formulas$LFT_findings
  
  lft_tune$train_control <- lft_globals$train_control
  
# tuning grids -------
  
  insert_msg('Tuning grids')
  
  ## Ranger
  
  lft_tune$tune_grids$ranger <- 
    expand.grid(splitrule = c('gini', 'extratrees'), 
                mtry = seq(2, length(lft_tune$variables)/2, by = 2), 
                min.node.size = c(1, 3), 
                stringsAsFactors = FALSE)
  
  ## nnet
  
  lft_tune$tune_grids$nnet <- 
    expand.grid(size = seq(1, length(lft_tune$variables)/2, by = 2), 
                decay = c(1e-5, 1e-4, 1e-3, 1e-2, 1e-1), 
                stringsAsFactors = FALSE)
  
  ## SVM radial
  
  lft_tune$tune_grids$svmRadial <-
    expand.grid(sigma = 0.01529346, 
                C = seq(0.1, 2, by = 0.1))
  
  ## GBM
  
  lft_tune$tune_grids$gbm <-
    expand.grid(n.trees = c(50, 100, 150, 200), 
                interaction.depth = 1:4, 
                shrinkage = 0.1, 
                n.minobsinnode = c(5, 10, 15))
  
# Modeling --------
  
  insert_msg('Modeling')
  
  registerDoParallel(cores = 7)
  
  set.seed(12345)
  
  lft_tune$models$ranger <- 
    train(form = lft_tune$formula, 
          data = lft_tune$analysis_tbl, 
          method = 'ranger', 
          metric = 'J', 
          trControl = lft_tune$train_control, 
          tuneGrid = lft_tune$tune_grids$ranger, 
          num.trees = 1000)
  
  lft_tune$models$nnet <- 
    train(form = lft_tune$formula, 
          data = lft_tune$analysis_tbl, 
          method = 'nnet', 
          metric = 'J', 
          trControl = lft_tune$train_control, 
          tuneGrid = lft_tune$tune_grids$nnet, 
          maxit = 500)
  
  lft_tune$models$svmRadial <- 
    train(form = lft_tune$formula, 
          data = lft_tune$analysis_tbl, 
          method = 'svmRadial', 
          metric = 'J', 
          trControl = lft_tune$train_control, 
          tuneGrid = lft_tune$tune_grids$svmRadial)
  
  lft_tune$models$gbm <- 
    train(form = lft_tune$formula, 
          data = lft_tune$analysis_tbl, 
          method = 'gbm', 
          metric = 'J', 
          trControl = lft_tune$train_control, 
          tuneGrid = lft_tune$tune_grids$gbm)

  stopImplicitCluster()
  
  lft_tune$models <- lft_tune$models %>% 
    map(as_caretx)
  
# Best tune parameters --------
  
  insert_msg('Best tune paramaters')
  
  lft_tune$best_tunes <- lft_tune$models %>% 
    map(~.x$bestTune)
  
# Plots for the tuning process ------
  
  insert_msg('Plots for the tuning process')
  
  lft_tune$tuning_plots <- 
    list(x = lft_tune$models, 
         y = list(plot_ranger_tuning, 
                  plot_nnet_tuning, 
                  plot_svm_tuning, 
                  plot_gbm_tuning),
         z = paste('Tuning: any LFT findings,', 
                   globals$algo_labs[names(lft_tune$models)])) %>% 
    pmap(function(x, y, z) y(x, plot_title = z))

# Plotting performance in the resamples ------
  
  insert_msg('Plotting kappa in the resamples')
  
  lft_tune$resample_plots <- 
    list(model = lft_tune$models, 
         plot_title = paste('Resample performance:', 
                            globals$algo_labs[names(lft_tune$models)]), 
         color = globals$algo_colors[names(lft_tune$models)]) %>% 
    pmap(plot_resample_kappa) %>% 
    map(~.x +
          geom_hline(yintercept = 0, 
                     linetype = 'dashed'))

# Caching the results -------  
  
  insert_msg('Caching the results')
  
  save(lft_tune, file = './cache/lft_tune.RData')
  
# END ------
  
  insert_tail()