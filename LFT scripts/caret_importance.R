# Model-specific variable importance computed with the build-in caret function

  insert_head()
  
# container -----
  
  caret_imp <- list()
  
# models --------
  
  insert_msg('Models')
  
  caret_imp$models <- 
    c(bin_models$models, 
      reg_models$models)
  
  ## re-fitting the Ranger models to obtain the permutation importance
  ## measures. train control is not really important here, since I'm not using 
  ## the models for predictions
  
  registerDoParallel(cores = 7)
  
  for(i in names(caret_imp$models)) {
    
    caret_imp$models[[i]]$ranger <- caret_imp$models[[i]]$ranger %>% 
      refit_ranger(train_control = trainControl())
    
  }
  
  stopImplicitCluster()
  
# importance measures -----
  
  insert_msg('Global variance importance stats')
  
  caret_imp$stats <- caret_imp$models %>% 
    map(map, varImp) %>% 
    map(map, format_importance)
  
# Bar plots of importance --------
  
  insert_msg('Bar plots of the importance metrics')
  
  ## plotting the top 10 most influential variables per outcome
  ## and algorithm
  
  caret_imp$plots <- 
    list(stat_lst = caret_imp$stats, 
         title_prefix = exchange(names(caret_imp$stats), 
                                 lft_globals$lexicon)) %>% 
    pmap(plot_caret_importance)
  
  for(i in c('DLCO_percent', 'FVC_percent', 'FEV1_percent')) {
    
    caret_imp$plots[[i]]$svmRadial <- caret_imp$plots[[i]]$svmRadial + 
      labs(x = expression('LOESS R'^2  * ' importance'))
    
  }
  
# END ------
  
  insert_tail()