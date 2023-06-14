# Trial to establish a function to link opacity and high opacity
# with CTSS

  insert_head()
  
# container -------
  
  ctss_fun <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## explanatory variables
  
  ctss_fun$responses <- c('opacity_percent', 'high_opacity_percent')
  
  ## analysis table

  ctss_fun$analysis_tbl <- ctss_globals$analysis_tbl %>% 
    filter(severity_class == 'cohort') %>% 
    select(ID, CTSS, all_of(ctss_fun$responses)) %>% 
    filter(complete.cases(.))

# Modeling AI opacity -------
  
  insert_msg('Modeling AI opacity')
  
  ctss_fun$models$opacity_lm_poly <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(opacity_percent) ~ CTSS + I(CTSS^2) + I(CTSS^3) + I(CTSS^4), 
            mod_fun = lm, 
            family = NULL)
  
  ctss_fun$models$opacity_rlm_poly <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(opacity_percent) ~ CTSS + I(CTSS^2) + I(CTSS^3) + I(CTSS^4), 
            mod_fun = rlm, 
            family = NULL)
  
  ctss_fun$models$opacity_gam_tp <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(opacity_percent) ~ CTSS + s(CTSS, bs = 'tp', k = 8), 
            mod_fun = gam, 
            family = NULL)

# Modeling AI high opacity -------
  
  insert_msg('Modeling AI high opacity')
  
  ctss_fun$models$high_opacity_lm_poly <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(high_opacity_percent) ~ CTSS + I(CTSS^2) + I(CTSS^3) + I(CTSS^4), 
            mod_fun = lm, 
            family = NULL)
  
  ctss_fun$models$high_opacity_rlm_poly <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(high_opacity_percent) ~ CTSS + I(CTSS^2) + I(CTSS^3) + I(CTSS^4), 
            mod_fun = rlm, 
            family = NULL)
  
  ctss_fun$models$high_opacity_gam_tp <- 
    make_lm(data = ctss_fun$analysis_tbl, 
            formula = sqrt(high_opacity_percent) ~ CTSS + s(CTSS, bs = 'tp', k = 8), 
            mod_fun = gam, 
            family = NULL)
  
# Model assumptions and diagnostic plots --------
  
  insert_msg('Model assumptions')
  
  ctss_fun$assumptions <- ctss_fun$models %>% 
    map(summary, 
        type = 'assumptions', 
        type.predict = 'response') %>% 
    compress(names_to = 'method') %>% 
    mutate(response = stri_extract(method, 
                                   regex = 'high_opacity|opacity'), 
           method = stri_replace(method, 
                                 regex = 'high_opacity_|opacity_', 
                                 replacement = ''))
  
  ctss_fun$resid_plots <- ctss_fun$models %>% 
    map(plot, 
        type.predict = 'response', 
        cust_theme = globals$common_theme)
  
# Fit stats ------
  
  insert_msg('Fit stats')
  
  ## calculating stats by hand, since the 'summary' method for rlm
  ## crashes
  
  ctss_fun$predictions <- ctss_fun$models %>% 
    map(resid, 
        type.predict = 'response')
  
  ## computing also median absolute error, RMSE is not proper for rlm!
  
  ctss_fun$stats <- ctss_fun$predictions %>% 
    map(~tibble(RMSE = sqrt(mean(.x$.resid^2)), 
                MSE = mean(.x$.resid^2), 
                MAE = mean(abs(.x$.resid)), 
                MedAE = median(abs(.x$.resid)), 
                raw_rsq = 1 - mean(.x$.resid^2)/var(.x[[1]]), 
                n_complete = nrow(.x))) %>% 
    compress(names_to = 'method') %>% 
    mutate(response = stri_extract(method, 
                                   regex = 'high_opacity|opacity'), 
           method = stri_replace(method, 
                                 regex = 'high_opacity_|opacity_', 
                                 replacement = ''), 
           plot_cap = paste0('R\u00B2 = ', signif(raw_rsq, 2), 
                             ', MedAE = ', signif(MedAE, 2), 
                             ', n = ', n_complete))
  
# Plotting the relationship ------
  
  insert_msg('Plotting the the relationship')
  
  ctss_fun$predictions <- ctss_fun$predictions %>% 
    map2(., c(rep('opacity_percent', 3), 
              rep('high_opacity_percent', 3)), 
         ~mutate(.x, !!.y := .x[[1]]^2))
  
  ctss_fun$fit_plots <- 
    list(data = ctss_fun$predictions, 
         y_var = c(rep('opacity_percent', 3), 
                   rep('high_opacity_percent', 3)), 
         point_color = c(rep('coral3', 3), 
                         rep('coral4', 3)), 
         plot_title = c('AI opacity, polynomial fit', 
                        'AI opacity, polynomial robust fit', 
                        'AI opacity, GAM TP fit', 
                        'AI high opacity, polynomial fit', 
                        'AI high opacity, polynomial robust fit', 
                        'AI high opacity, GAM TP fit'), 
         plot_subtitle = ctss_fun$stats$plot_cap, 
         y_lab = c(rep('AI opacity, % of lung', 3), 
                   rep('AI high opacity, % of lung', 3))) %>% 
    pmap(scatter_plot, 
         x_var = 'CTSS', 
         point_wjitter = 0.1, 
         point_alpha = 0.75) %>% 
    map(~.x + 
          geom_ribbon(aes(ymin = .fitted - 2*.se.fit, 
                          ymax = .fitted + 2*.se.fit), 
                      color = 'gray60', 
                      alpha = 0.3, 
                      color = NA) + 
          geom_line(aes(y = .fitted), 
                    color = 'steelblue', 
                    linewidth = 0.75) + 
          scale_y_continuous(trans = 'sqrt'))
  
# END ------
  
  insert_tail()