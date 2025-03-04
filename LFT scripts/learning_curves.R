# Learning curves for models of reduced and DLCO and DLCO percent of reference. 
# 
# The general principle: testing the quality of model fit for increasing sizes 
# of the training data; the cost function values are computed for the training 
# data sets, hold-oiut (25% of the training data size), and 10-repeats 10-fold 
# cross-validation. 
#
# Working with the `caret`-native `learning_curve_dat()` function. 
# Unfortunately, the computation crashes for some algorithms (like SVM). 
# Additionally, there's no way to implement the patient-blocked hold-out and 
# and cross-validation designs. Hence, the cost values are likely too optimistic. 

  insert_head()
  
# container --------
  
  lft_learn <- list()
  
# analysis globals ---------
  
  insert_msg('Analysis globals')

  ## the train control objects
  
  lft_learn$train_control$DLCO_reduced <- 
    trainControl(method = 'repeatedcv', 
                 number = 10, 
                 repeats = 10, 
                 summaryFunction = multiClassSummary)
  
  lft_learn$train_control$DLCO_percent <- 
    trainControl(method = 'repeatedcv', 
                 number = 10, 
                 repeats = 10)
  
  ## the models of DLCO < 80% of the reference, 
  ## and DLCO as reference's percentage
  
  lft_learn$models <- 
    list(DLCO_reduced = bin_models$models$DLCO_reduced, 
         DLCO_percent = reg_models$models$DLCO_percent)
  
  ## the best tunes and analysis data
  
  lft_learn$best_tunes <- lft_learn$models %>% 
    map(map, ~.x$bestTune)
  
  lft_learn$data <- lft_learn$models %>% 
    map(map, ~.x$trainingData)
  
  ## algorithm names
  
  lft_learn$algos <- lft_learn$models %>% 
    map(names)
  
  lft_learn$algos <- lft_learn$algos %>% 
    map(~set_names(.x, .x))

# computation of the cross-validated learning curves ------
  
  insert_msg('computation of the learning curves')
  
  for(i in names(lft_learn$data)) {
    
    registerDoParallel(cores = 8)
    
    ## safely: computation crashes for some algorithms
    
    lft_learn$curves[[i]] <- lft_learn$algos[[i]] %>% 
      map(~safely(learning_curve_dat)(dat = lft_learn$data[[i]][[.x]], 
                                      outcome = '.outcome', 
                                      test_prop = 1/4, 
                                      proportion = (2:10)/10, 
                                      method = .x, 
                                      tuneGrid = lft_learn$best_tunes[[i]][[.x]], 
                                      trControl = lft_learn$train_control[[i]]))
      
    stopImplicitCluster()
      
  }
  
# Caching -------
  
  insert_msg('Caching')
  
  lft_learn$curves <- lft_learn$curves %>% 
    map(map, ~.x$result) %>% 
    map(compact) %>% 
    map(map, as_tibble)
  
  lft_learn <- lft_learn['curves']
  
  save(lft_learn, file = './cache/lft_learn.RData')

# END -------
  
  insert_tail()