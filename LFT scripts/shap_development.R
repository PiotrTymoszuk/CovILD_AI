# SHAP local importance of explanatory variables at modeling DLCO findings
# Development of the SHAP objects, this a time consuming step.
#
# As a background, I'm using a single observation with the explanatory 
# variables set to their minimum, i.e. to the minimal numeric variable value 
# or, for qualitative features, to the baseline level.

  insert_head()
  
# container -------
  
  shap_devel <- list()
  
# models -------
  
  insert_msg('Caret models')
  
  shap_devel$models <- 
    list(DLCO_reduced = bin_models$models[["DLCO_reduced"]],
         DLCO_percent = reg_models$models[["DLCO_percent"]])

# X data frames with observations to be explained ------
  
  insert_msg('X data frames')
  
  ## X data frames
  
  shap_devel$x_frames <- shap_devel$models %>% 
    map2(., names(.), 
         ~select(model.frame(.x[[1]]), -.data[[.y]]))
  
# background observations and prediction functions -------
  
  insert_msg('Background observations and predictions')
  
  shap_devel$bg_x <- shap_devel$x_frames %>% 
    map(shap_background, quant = 0.25)
  
  shap_devel$pred_funs <- 
    list(DLCO_reduced = pred_binary, 
         DLCO_percent = pred_reg)
  
# SHAP objects ------
  
  insert_msg('SHAP objects')
  
  shap_devel$objects <- list()
  
  for(i in names(shap_devel$models)) {
    
    shap_devel$objects[[i]] <- shap_devel$models[[i]] %>% 
      map(~kernelshap(.x, 
                      X = shap_devel$x_frames[[i]], 
                      bg_X = shap_devel$bg_x[[i]], 
                      pred_fun = shap_devel$pred_funs[[i]], 
                      parallel = FALSE))
    
  }
  
# Caching the results ------
  
  insert_msg('Caching the results')
  
  save(shap_devel, file = './cache/shap_devel.RData')
  
# END -----
  
  rm(i)