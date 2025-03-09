# Figures for the reviewers

  insert_head()
  
# container --------
  
  rev_figs <- list()
  
# analysis globals -------
  
  insert_msg('Analyis globals')
  
  ## range of observed DLCO: to be used as axis limits
  
  dlco_range <- range(lft_globals$analysis_tbl$DLCO_percent)

# Correlation of the observed and predicted DLCO for all models --------
  
  insert_msg('Correlation of the predicted and observed DLCO, all models')
  
  ## and common scales
  
  rev_figs$dlco_correlations <- 
    reg_models$scatter_plots$DLCO_percent$cv %>% 
    map(~.x + 
          scale_x_continuous(limits = dlco_range) + 
          scale_y_continuous(limits = dlco_range)) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv') %>% 
    as_figure(label = 'dlco_predicted_observed_correlations', 
              ref_name = 'dlco_correlations', 
              caption = paste('Correlation of predicted and observed diffusion', 
                              'capacity for carbon monoxide during COVID-19', 
                              'convalescence'), 
              w = 180, 
              h = 180)
  
# Saving figures on the disc -------
  
  insert_msg('Saving figures on the disc')
  
  rev_figs %>% 
    number_figures(prefix = 'reviewer_figure_') %>% 
    walk(pickle, 
         path = './paper/reviewer figures', 
         device = cairo_pdf)
  
# END ------
  
  rm(dlco_range)
  
  insert_tail()