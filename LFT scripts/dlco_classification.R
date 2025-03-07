# Analysis of accuracy and confidence (Brier's squares) of prediction of 
# DLCO < 80% as a function of DLCO values expressed as percentage of the 
# patient's reference

  insert_head()
  
# container -------
  
  dlco_class <- list()
  
# analysis globals: DLCO, observed and predicted DLCO insufficiency -------
  
  insert_msg('Analysis globals')
  
  ## models and predictions in the training and cross-validation 
  ## setting. 
  ## for cross-validation, the resamples are collapsed
  
  dlco_class$data <- bin_models$models$DLCO_reduced %>% 
    map(predict) %>% 
    map(compact) %>% 
    map(map, components.predx, 'data') %>% 
    transpose
  
  dlco_class$data$cv <- dlco_class$data$cv %>% 
    map(group_by, .observation) %>% 
    map(summarise, 
        .outcome = .outcome[1], 
        yes = mean(yes)) %>% 
    map(mutate, 
        .fitted = ifelse(yes > 0.5, 'yes', 'no'), 
        .fitted = factor(.fitted, c('no', 'yes')))
  
  dlco_class$data <- dlco_class$data %>% 
    transpose %>% 
    map(map, 
        transmute, 
        index = .observation, 
        DLCO_reduced = .fitted, 
        p_value = yes) %>% 
    map(compress, names_to = 'data_set')
  
  ## observed DLCO and reduced DLCO
  
  dlco_class$observed <- lft_globals$analysis_tbl %>% 
    transmute(index = 1:nrow(.), 
              DLCO_percent = DLCO_percent, 
              DLCO_reduced = DLCO_reduced, 
              p_value = 1, 
              data_set = 'observed')
  
  dlco_class$data <- dlco_class$data %>% 
    map(left_join, 
        dlco_class$observed[c('index', 'DLCO_percent')], 
        by = 'index') %>% 
    map(rbind, dlco_class$observed) %>% 
    map(mutate, 
        data_set = factor(data_set, c('observed', 'train', 'cv')))
  
  ## DLCO cuts: used later for construction of confusion matrices
  ## for intervals of DLCO
  
  dlco_class$data <- dlco_class$data %>% 
    map(mutate, 
        dlco_interval = cut(DLCO_percent, 
                            c(-Inf, 60, 80, 100, 120, Inf), 
                            c('< 60%', 
                              '60% - 80%', 
                              '80% - 100%', 
                              '100% - 120%', 
                              '> 120%')))
  
  ## wide-format data used for computation of confusion matrices
  ## an moving accuracy and moving squared distances
  
  dlco_class$wide_data$class <- dlco_class$data %>% 
    map(pivot_wider, 
        id_cols = all_of(c('index', 
                           'dlco_interval', 
                           'DLCO_percent')), 
        names_from = 'data_set', 
        values_from = 'DLCO_reduced')
  
  dlco_class$wide_data$p_value <- dlco_class$data %>% 
    map(filter, data_set %in% c('train', 'cv')) %>% 
    map(pivot_wider, 
        id_cols = 'index', 
        names_from = 'data_set', 
        values_from = 'p_value') %>% 
    map(set_names, c('index', 'sq_train', 'sq_cv'))
  
  dlco_class$wide_data <- dlco_class$wide_data %>% 
    transpose %>% 
    map(reduce, left_join, by = 'index')
  
  dlco_class$wide_data <- dlco_class$wide_data %>% 
    map(mutate, 
        sq_train = (as.numeric(observed) - 1 - sq_train)^2, 
        sq_cv = (as.numeric(observed) - 1 - sq_cv)^2) %>% 
    map(arrange, DLCO_percent)
  
  ## data set labels
  
  dlco_class$data_labels <- 
    c('observed' = 'observed', 
      'train' = 'training', 
      'cv' = 'CV')
  
# confusion matrices for intervals of DLCO, CV --------
  
  insert_msg('Confusion matrices for intervals of DLCO, CV')
  
  ## confusion matrices for the CV setting
  
  dlco_class$cv_confusion_mtx <- dlco_class$wide_data %>% 
    map(blast, dlco_interval) %>% 
    map(map, 
        ~table(observed = .x$observed, 
               predicted = .x$cv))
  
# Sliding window accuracy and Brier squares ---------
  
  insert_msg('Sliding window accuracy and Brier squares')
  
  dlco_class$wide_data <- dlco_class$wide_data %>% 
    map(transmute, 
        DLCO_percent = DLCO_percent, 
        train = as.numeric(train == observed), 
        cv = as.numeric(cv == observed), 
        sq_train = sq_train, 
        sq_cv = sq_cv) %>% 
    map(map_dfc, rollmean, k = 20)
  
  ## long format for plotting
  
  dlco_class$moving_data$accuracy <- dlco_class$wide_data %>% 
    map(select, DLCO_percent, train, cv) %>% 
    map(pivot_longer, 
        cols = all_of(c('train', 'cv')), 
        names_to = 'data_set', 
        values_to = 'accuracy')
  
  dlco_class$moving_data$sqaure <- dlco_class$wide_data %>% 
    map(select, DLCO_percent, sq_train, sq_cv) %>% 
    map(set_names, c('DLCO_percent', 'train', 'cv')) %>% 
    map(pivot_longer, 
        cols = all_of(c('train', 'cv')), 
        names_to = 'data_set', 
        values_to = 'sqaure')
  
  dlco_class$moving_data <- dlco_class$moving_data %>% 
    transpose %>% 
    map(reduce, 
        left_join, by = c('DLCO_percent', 'data_set')) %>% 
    map(mutate, 
        data_set = factor(data_set, c('train', 'cv')))
  
# Visualizations: DLCO values and deficiency ---------
  
  insert_msg('Plots')
  
  ## point color codes for class assignment
  
  dlco_class$class_plots$class <- 
    list(x = dlco_class$data %>% 
           map(filter, data_set %in% c('train', 'cv')), 
         y = globals$algo_labs[names(dlco_class$data)]) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(y = factor(data_set, rev(levels(data_set))), 
                      x = DLCO_percent, 
                      fill = DLCO_reduced)) + 
           geom_violin(color = 'gray30', 
                       fill = NA) + 
           geom_point(shape = 21, 
                      size = 2, 
                      alpha = 0.75, 
                      position = position_jitter(h = 0.12, w = 0)) + 
           geom_vline(xintercept = 80, 
                      linetype = 'dashed') + 
           scale_fill_manual(values = c(no = 'cornsilk', 
                                        yes = 'orangered3'), 
                             labels = c(no = 'DLCO \u2265 80%', 
                                        yes = 'DLCO < 80%'), 
                             name = 'prediction') + 
           scale_y_discrete(labels = dlco_class$data_labels) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'DLCO, % of reference'))
  
  ## point color codes for class assignment p value
  
  dlco_class$class_plots$p_value <- 
    list(x = dlco_class$data %>% 
           map(filter, data_set %in% c('train', 'cv')), 
         y = globals$algo_labs[names(dlco_class$data)]) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(y = factor(data_set, rev(levels(data_set))), 
                      x = DLCO_percent, 
                      color = p_value)) + 
           geom_violin(color = 'gray30') + 
           geom_point(shape = 16, 
                      position = position_jitter(h = 0.12, 
                                                 w = 0)) + 
           geom_vline(xintercept = 80, 
                      linetype = 'dashed') + 
           scale_color_gradient2(low = 'steelblue', 
                                 high = 'firebrick', 
                                 mid = 'gray70', 
                                 midpoint = 0.5, 
                                 name = 'assignment\np value') + 
           scale_y_discrete(labels = dlco_class$data_labels) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                x = 'DLCO, % of reference'))
  
# visualizations: moving averages of DLCO, accuracy, and Brier squeres -------
  
  insert_msg('Visualizations of moving accuracy and Brier square')
  
  dlco_class$moving_plots$accuracy <- 
    list(x = dlco_class$moving_data, 
         y = globals$algo_labs[names(dlco_class$moving_data)]) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = DLCO_percent, 
                      y = accuracy, 
                      color = data_set)) + 
           geom_path() + 
           geom_vline(xintercept = 80, 
                      linetype = 'dashed') + 
           scale_color_manual(values = globals$dataset_colors, 
                              labels = dlco_class$data_labels, 
                              name = '') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'DLCO, % of reference', 
                y = 'accuracy, moving average'))
  
  dlco_class$moving_plots$square <- 
    list(x = dlco_class$moving_data, 
         y = globals$algo_labs[names(dlco_class$moving_data)]) %>% 
    pmap(function(x, y) x %>% 
           ggplot(aes(x = DLCO_percent, 
                      y = sqaure, 
                      color = data_set)) + 
           geom_path() + 
           geom_vline(xintercept = 80, 
                      linetype = 'dashed') + 
           scale_color_manual(values = globals$dataset_colors, 
                              labels = dlco_class$data_labels, 
                              name = '') + 
           globals$common_theme + 
           labs(title = y, 
                x = 'DLCO, % of reference', 
                y = 'square distance to outcome, moving average'))
  
  dlco_class$moving_plots <- transpose(dlco_class$moving_plots)
  
# END -------
  
  dlco_class$observed <- NULL
  dlco_class$data <- NULL
  dlco_class$wide_data <- NULL
  
  dlco_class <- compact(dlco_class)

  insert_tail()