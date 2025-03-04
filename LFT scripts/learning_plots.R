# Plots of the learning curves: statistics of performance of machine learning 
# models as a function of size of the training data set. Performance in the 
# training data portion, a hold-out test subset and 10-repeats 10 fold CV 
# is evaluated.

  insert_head()
  
# container -------
  
  lft_lcurves <- list()
  
# analysis globals: learning curve data -------
  
  insert_msg('Analysis globals')

  ## metrics of performance and evaluation data sets
  
  lft_lcurves$metrics <- 
    list(DLCO_reduced = c('Accuracy', 'Kappa'), 
         DLCO_percent = c('MAE', 'Rsquared'))
  
  lft_lcurves$metric_labels <- 
    c(Accuracy = 'Accuracy', 
      Kappa = "Cohen's \u03BA", 
      MAE = 'MAE', 
      Rsquared = 'R\u00B2')
  
  lft_lcurves$sets <- c('Training', 'Testing', 'Resampling')
  
  lft_lcurves$set_labels <- 
    c(Training = 'training subset', 
      Testing = 'test subset', 
      Resampling = 'cross-validation')
  
  lft_lcurves$set_colors <- 
    c(Training = globals$dataset_colors[["train"]], 
      Testing = 'aquamarine3', 
      Resampling = globals$dataset_colors[["cv"]])
  
  ## learning curve data
  
  lft_lcurves$analysis_tbl <- lft_learn$curves %>% 
    map(map, 
        mutate, 
        Data = factor(Data, lft_lcurves$sets))
  
# The plots --------
  
  insert_msg('The plots')
  
  for(i in names(lft_lcurves$analysis_tbl)) {
    
    lft_lcurves$plots[[i]] <- 
      list(data = lft_lcurves$analysis_tbl[[i]], 
           plot_title = exchange(i, lft_globals$lexicon) %>% 
             paste(globals$algo_labs[names(lft_lcurves$analysis_tbl[[i]])], 
                   sep = ', ')) %>% 
      pmap(plot_learn_curves, 
           metrics = lft_lcurves$metrics[[i]], 
           metric_labels = lft_lcurves$metric_labels,
           data_labels = lft_lcurves$set_labels, 
           data_colors = lft_lcurves$set_colors, 
           method = 'loess', 
           span = 0.8, 
           show.legend = FALSE, 
           point_shape = 21)
    
  }

# END ------
  
  lft_lcurves$analysis_tbl <- NULL
  
  lft_lcurves <- compact(lft_lcurves)
  
  insert_tail()