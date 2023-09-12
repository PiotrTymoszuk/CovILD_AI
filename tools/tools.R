# Functions for the project

  library(tidyverse)
  library(trafo)
  library(rlang)
  library(stringi)
  library(OptimalCutpoints)
  library(plotROC)
  library(caret)
  library(furrr)
  library(coxed)
  library(soucer)
  
# Lexicon handling ------
  
  make_lexicon <- function(label_vector, 
                           unit_vector, 
                           lexicon, 
                           data) {
    
    ## merges user-provided labels and unit for variables
    ## with an existing lexicon
    
    new_lex <- list(label_vector, 
                    unit_vector) %>% 
      map2(., c('label', 'unit'), 
           ~compress(.x, 
                     names_to = 'variable', 
                     values_to = .y)) %>% 
      reduce(left_join, by = 'variable') %>% 
      inner_join(lexicon %>% 
                   transmute(variable = Variable, 
                             description = Description), 
                 by = 'variable')
    
    ## information about the variable format
    
    new_lex$format <- 
      data[new_lex$variable] %>% 
      map_lgl(is.numeric) %>% 
      ifelse('numeric', 'factor')
    
    ## table label
    
    new_lex %>% 
      mutate(table_label = ifelse(is.na(unit), 
                                  label, paste(label, unit, sep = ', ')))
    
  }
  
# Text functions -----
  
  capitalize_first_char <- function(x) {
    
    ## capitalizes the first character of a string
    
    stri_trans_totitle(stri_sub(x, 1, 1)) %s+% stri_sub(x, 2)
    
  }
  
# Formatting of result tables, multiple-testing adjustment ---------
  
  format_summ_tbl <- function(data, 
                              rm_n = FALSE, 
                              rm_mean = TRUE, 
                              dict = NULL, 
                              key = 'variable', 
                              value = 'table_label') {
    
    ## formats a summary table with descriptive stats
    
    data <- data %>% 
      map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
      map_dfc(stri_replace, regex = '\\nno:.*$', replacement = '') %>% 
      map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
      map_dfc(stri_replace, fixed = 'Median =', replacement = 'median:') %>% 
      map_dfc(stri_replace, fixed = 'Mean =', replacement = 'mean:') %>% 
      map_dfc(stri_replace, fixed = 'Range', replacement = 'range') %>% 
      map_dfc(stri_replace, fixed = 'Complete', replacement = 'complete')
    
    if(!is.null(dict)) {
      
      data <- data %>% 
        mutate(variable = exchange(variable, 
                                   key = key, 
                                   value = value, 
                                   dict = dict))
      
    }
    
    if(rm_n) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '\\ncompl.*$', replacement = '')
      
    }
    
    if(rm_mean) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = 'mean.*\\n', replacement = '') %>% 
        map_dfc(stri_replace, fixed = 'median: ', replacement = '')
      
    }
    
    data
    
  }
  
  re_adjust <- function(data, 
                        p_variable = 'p_value', 
                        method = 'BH') {
    
    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method
    
    if(method != 'none') {
      
      data[['p_adjusted']] <- 
        p.adjust(data[[p_variable]], method = method)
      
    } else {
      
      data[['p_adjusted']] <- data[[p_variable]]
      
    }
    
    data %>% 
      mutate(significance = ifelse(p_adjusted < 0.001, 
                                   'p < 0.001', 
                                   ifelse(p_adjusted >= 0.05, 
                                          paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                          paste('p =', signif(p_adjusted, 2)))))
    
  }
  
# Plots for explorative analysis ------
  
  plot_explo_numeric <- function(data, 
                                 outcome, 
                                 time_variable = 'follow_up', 
                                 severity_variable = 'cohort_split', 
                                 plot_title = NULL, 
                                 plot_subtitle = NULL, 
                                 x_lab = 'Time after COVID-19', 
                                 y_lab = outcome, 
                                 plot_tag = NULL, 
                                 shape_alpha = 0.25, 
                                 point_alpha = 0.75, 
                                 dodge_w = 0.85, 
                                 show_n = FALSE, 
                                 n_adjust = TRUE, 
                                 n_offset = 0,
                                 n_vjust = 0,
                                 n_hjust = -0.2, 
                                 n_size = 2.75, 
                                 n_angle = 90, 
                                 facet = TRUE) {
    
    ## plots a numeric variable split by the timepoint and COVID-19 severity
    
    plot <- data %>% 
      ggplot(aes(x = .data[[time_variable]], 
                 y = .data[[outcome]], 
                 fill = .data[[severity_variable]])) + 
      geom_boxplot(outlier.color = NA, 
                   position = position_dodge(dodge_w), 
                   alpha = shape_alpha, 
                   show.legend = FALSE) + 
      geom_point(size = 2, 
                 shape = 21, 
                 alpha = point_alpha, 
                 position = position_jitterdodge(jitter.width = 0.1, 
                                                 jitter.height = 0, 
                                                 dodge.width = dodge_w)) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           y = y_lab, 
           x = x_lab, 
           tag = plot_tag)
    
    if(show_n) {
      
      ## displays n numbers per timepoint
      
      n_numbers <- data %>% 
        select(all_of(c(time_variable, severity_variable, outcome))) %>% 
        blast(all_of(c(time_variable, severity_variable))) %>% 
        #blast(.data[[time_variable]], .data[[severity_variable]]) %>% 
        map(~filter(.x, complete.cases(.x))) %>% 
        map_dbl(nrow) %>% 
        compress(names_to = 'strata', 
                 value = 'n') %>% 
        mutate(!!severity_variable := stri_split_fixed(strata, 
                                                       pattern = '.', 
                                                       simplify = TRUE)[, 2], 
               !!time_variable := stri_split_fixed(strata, 
                                                   pattern = '.', 
                                                   simplify = TRUE)[, 1], 
               !!severity_variable := factor(.data[[severity_variable]], 
                                             levels(data[[severity_variable]])), 
               !!time_variable := factor(.data[[time_variable]], 
                                         levels(data[[time_variable]])), 
               n_lab = paste('n =', n))
      
      if(n_adjust) {
        
        y_positions <- data %>% 
          select(all_of(c(time_variable, severity_variable, outcome))) %>% 
          group_by(.data[[time_variable]], .data[[severity_variable]]) %>% 
          summarise(y_pos = max(.data[[outcome]], na.rm = TRUE) * (1 + n_offset))
        
        n_numbers <- left_join(n_numbers, 
                               y_positions, 
                               by = c(time_variable, severity_variable))
        
      } else {
        
        y_positions <- data %>% 
          select(all_of(c(time_variable, outcome))) %>% 
          group_by(.data[[time_variable]]) %>% 
          summarise(y_pos = max(.data[[outcome]], na.rm = TRUE) * (1 + n_offset))
        
        n_numbers <- left_join(n_numbers, 
                               y_positions, by = time_variable)
        
      }
      
      plot <- plot + 
        geom_text(data = n_numbers, 
                  aes(x = .data[[time_variable]], 
                      y = y_pos, 
                      label = n_lab, 
                      color = .data[[severity_variable]]),
                  size = n_size, 
                  angle = n_angle, 
                  position = position_dodge(dodge_w), 
                  vjust = n_vjust, 
                  hjust = n_hjust)
      
    }
    
    if(facet) {
      
      facet_formula <- paste('. ~', severity_variable) %>% 
        as.formula
      
      plot <- plot + 
        facet_grid(facet_formula)
      
    }
    
    plot
    
  }
  
  plot_explo_stack <- function(data, 
                               outcome, 
                               time_variable = 'follow_up', 
                               severity_variable = 'cohort_split', 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               x_lab = 'Time after COVID-19', 
                               y_lab = '% of strata', 
                               plot_tag = NULL, 
                               txt_form = c('none', 'text', 'label'), 
                               txt_size = 2.75, 
                               txt_color = 'black', 
                               show_n = FALSE, 
                               n_offset = 0,
                               n_vjust = 0,
                               n_hjust = -0.2, 
                               n_size = 2.75, 
                               n_angle = 90) {
    
    ## draws a stack plot faceted by COVID-19 severity
    
    txt_form <- match.arg(txt_form[1], 
                          c('none', 'text', 'label'))
    
    ## plotting data
    
    data <- data %>% 
      count(.data[[time_variable]], 
            .data[[severity_variable]], 
            .data[[outcome]]) %>% 
      filter(complete.cases(.)) %>% 
      blast(all_of(c(time_variable, severity_variable))) %>% 
      map(arrange, desc(.data[[outcome]])) %>% 
      map(mutate, 
          n_complete = sum(n), 
          percent = n/sum(n) * 100, 
          y_pos = cumsum(percent) - 0.5 * percent) %>% 
      compress(names_to = 'strata') %>% 
      mutate(!!severity_variable := stri_split_fixed(strata, 
                                                     pattern = '.', 
                                                     simplify = TRUE)[, 2], 
             !!time_variable := stri_split_fixed(strata, 
                                                 pattern = '.', 
                                                 simplify = TRUE)[, 1], 
             !!severity_variable := factor(.data[[severity_variable]], 
                                           levels(data[[severity_variable]])), 
             !!time_variable := factor(.data[[time_variable]], 
                                       levels(data[[time_variable]])), 
             perc_lab = paste0(signif(percent, 2), '%'))
    
    ## faceting formula
    
    facet_formula <- paste('. ~', severity_variable) %>% 
      as.formula
    
    ## stack plot
    
    plot <- data %>% 
      ggplot(aes(x = .data[[time_variable]], 
                 y = percent, 
                 fill = .data[[outcome]])) + 
      geom_bar(stat = 'identity', 
               color = 'black') + 
      scale_y_continuous(breaks = seq(0, 100, by = 25)) +
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           y = y_lab, 
           x = x_lab, 
           tag = plot_tag) + 
      facet_grid(facet_formula)
    
    if(txt_form == 'label') {
      
      plot <- plot + 
        geom_label(aes(y = y_pos, 
                       label = perc_lab), 
                   size = txt_size, 
                   color = txt_color)
      
    } else if(txt_form == 'text') {
      
      plot <- plot +  
        geom_text(aes(y = y_pos, 
                      label = perc_lab), 
                  size = txt_size, 
                  color = txt_color)
      
    }
    
    if(show_n) {
      
      ## displays n numbers at the top of the plot
      
      n_table <- data %>% 
        filter(.data[[outcome]] == levels(data[[outcome]])[1]) %>% 
        mutate(n_lab = paste('n =', n_complete), 
               percent = 100 * (1 + n_offset))
      
      plot <- plot + 
        geom_text(data = n_table, 
                  aes(label = n_lab), 
                  size = n_size, 
                  angle = n_angle, 
                  hjust = n_hjust, 
                  vjust = n_vjust)
      
    }
    
    plot
    
  }
  
# Distribution testing ------
  
  normality_test <- function(x) {
    
    ## tests for normality upon transformations
    ## computes mean and variance as well
    
    test_data <- 
      list(identity = x, 
         log = log(x + 1), 
         sqrt = sqrt(x))
    
    shapiro <- test_data %>% 
      map(shapiro_test) %>% 
      compress(names = 'transformation') %>% 
      relocate(transformation) %>% 
      select(-variable) %>% 
      mutate(best_transf = ifelse(statistic == max(statistic, na.rm = T), 
                                  'yes', 'no'))
    
    stats <- test_data %>% 
      map_dfr(~tibble(mean = mean(.x, na.rm = TRUE), 
                      variance = var(.x, na.rm = TRUE)))
    
    cbind(shapiro, stats) %>% 
      as_tibble
    
  }
  
  test_long_distribution <- function(data, 
                                     variable, 
                                     split_factor = 'analysis_split') {
    
    ## computes the Shapiro-Wilk test for a given variable of a data frame
    ## for the whole variable vector and the vector split by the user-defined
    ## factor
    ## outputs mean and variance as well
    ## if split_factor is NULL, the function returns only the global stats
    
    if(is.null(split_factor)) {
      
      return(normality_test(data[[variable]]))
      
    }
    
    data <- c(list(global = data), 
              blast(data, all_of(split_factor)))
    
    data %>% 
      map(~.x[[variable]]) %>% 
      map(normality_test) %>% 
      compress(names_to = split_factor)
    
  }
  
  
# Optimal cutoff -----
  
  extract_cut <- function(object) {
    
    ## extracts the optimal cutoff and its stats from an object
    ## of the 'optimal.cutpoints'
    
    stopifnot(inherits(object, 'optimal.cutpoints'))
    
    ## statistics for all possible cutoffs
    
    all_stat_names <- c('Se', 'Sp', 'PPV', 'NPV', 
                        'DLR.Positive', 'DLR.Negative')
    
    all_stats <- 
      object[[1]]$Global$measures.acc[all_stat_names] %>% 
      reduce(cbind) %>% 
      set_colnames(all_stat_names) %>% 
      cbind(cutoff = object[[1]]$Global$measures.acc$cutoffs, .) %>% 
      as.data.frame %>% 
      as_tibble %>% 
      mutate(J = Se + Sp - 1)
    
    ## AUC with 95% confidence intervals
    ## numbers of diseased and healthy
    
    auc <- tibble(auc = object[[1]]$Global$measures.acc$AUC['AUC'], 
                  lower_ci = object[[1]]$Global$measures.acc$AUC['ll'], 
                  upper_ci = object[[1]]$Global$measures.acc$AUC['ul'])
    
    n_numbers <- object[[1]]$Global$measures.acc$n %>%
      as_tibble %>% 
      set_names(c('healthy', 'disease'))
    
    ## stats for the optimal cutoff
    
    opt_cutoff <- object[[1]]$Global$optimal.cutoff$cutoff[1]
    
    cut_stats <- all_stats %>% 
      filter(cutoff == opt_cutoff) %>% 
      mutate(TP = n_numbers$disease, 
             TN = n_numbers$healthy, 
             FP = object[[1]]$Global$optimal.cutoff$FP[1], 
             FN = object[[1]]$Global$optimal.cutoff$FN[1]) %>% 
      cbind(auc) %>% 
      as_tibble
    
    ## output
    
    list(n_numbers = n_numbers, 
         auc = auc,
         all_cutoffs = all_stats, 
         optimal_cutoff = cut_stats)
    
  }
  
  plot_cut_search <- function(data, 
                              opt_cutoff = NULL, 
                              plot_title = c('Sensitivity and Specificity', 
                                             'Negative and positive predictive value'), 
                              plot_subtitle = NULL,
                              x_lab = 'AI opacity, %', 
                              x_trans = 'identity', 
                              signif_digits = 2) {
    
    ## plots Sensitivity, Specificity, J, as well as 
    ## PPV and NPV for all possible cutoff values
    
    ## plotting data: long format
    
    plot_data <- list(SeSp = c('Se', 'Sp', 'J'), 
                      PV = c('PPV', 'NPV')) %>% 
      map(pivot_longer, 
          data = data, 
          names_to = 'statistic', 
          values_to = 'value') %>% 
      map(select, cutoff, statistic, value)
    
    ## plots
    
    plots <- plot_data %>% 
      map(~ggplot(.x, 
                  aes(x = cutoff, 
                      y = value, 
                      color = statistic)))
    
    if(!is.null(opt_cutoff)) {
      
      plots <- plots %>% 
        map(~.x + 
              geom_vline(xintercept = opt_cutoff, 
                         linetype = 'dashed') + 
              annotate('text', 
                       label = paste('cutoff =', 
                                     signif(opt_cutoff, signif_digits)), 
                       x = opt_cutoff, 
                       y = 0.1, 
                       hjust = -0.4, 
                       size = 2.75))
      
    }
    
    list(x = plots,
         y = plot_title) %>% 
      pmap(function(x, y) x + 
             geom_path() + 
             scale_color_manual(values = c(Se = 'coral3', 
                                           Sp = 'steelblue', 
                                           J = 'darkolivegreen4', 
                                           PPV = 'coral3', 
                                           NPV = 'steelblue'), 
                                name = '') + 
             scale_x_continuous(trans = x_trans, 
                                labels = function(x) signif(x, signif_digits)) +
             scale_y_continuous(limits = c(0, 1)) + 
             globals$common_theme + 
             labs(title = y, 
                  subtitle = plot_subtitle, 
                  x = x_lab, 
                  y = 'statistic value'))
    
    
  }
  
  plot_roc <- function(data, 
                       cutoff_stats = NULL, 
                       m_variable = 'opacity_percent', 
                       d_variable = 'CT_findings', 
                       color = 'steelblue', 
                       plot_title = 'ROC', 
                       plot_subtitle = NULL, 
                       plot_tag = NULL, 
                       txt_size = 2.75, 
                       x_txt = 0.5, 
                       y_txt = 0.05, 
                       txt_color = color, 
                       signif_digits = 2, ...) {
    
    ## plots ROC
    
    plot <- data %>% 
      mutate(M = .data[[m_variable]], 
             D = .data[[d_variable]]) %>% 
      ggplot(aes(d = D, 
                 m = M))
    
    if(!is.null(cutoff_stats)) {
      
      cutoff_stats <- cutoff_stats %>% 
        mutate(auc_lab = paste0('AUC = ', signif(auc, signif_digits), 
                                ' [', signif(lower_ci, signif_digits), 
                                ' - ', signif(upper_ci, signif_digits), ']'), 
               plot_lab = paste0('Se = ', signif(Se, signif_digits), 
                                 '\nSp = ', signif(Sp, signif_digits), 
                                 '\nJ = ', signif(J, signif_digits), 
                                 '\n', auc_lab))
      
      plot <- plot + 
        geom_roc(labels = TRUE, 
                 labelsize = txt_size, 
                 cutoffs.at = cutoff_stats$cutoff[1], 
                 cutoff.labels = signif(cutoff_stats$cutoff[1], signif_digits), 
                 color = color, 
                 ...) + 
        geom_segment(aes(x = -Inf, 
                         xend = 1 - cutoff_stats$Sp[1], 
                         y = cutoff_stats$Se[1], 
                         yend = cutoff_stats$Se[1]), 
                     linetype = 'dashed') + 
        geom_segment(aes(x = 1 - cutoff_stats$Sp[1], 
                         xend = 1 - cutoff_stats$Sp[1], 
                         y = -Inf, 
                         yend = cutoff_stats$Se[1]), 
                     linetype = 'dashed') + 
        annotate('text', 
                 label = cutoff_stats$plot_lab[1], 
                 size = txt_size, 
                 hjust = 0, 
                 vjust = 0, 
                 x = x_txt,
                 y = y_txt, 
                 color = txt_color)
      
    } else {
      
      plot <- plot +
        geom_roc(color = color, ...)
      
      
    }
    
    plot + 
      style_roc(xlab = '1 - Sp', 
                ylab = 'Se', 
                guide = TRUE) + 
      geom_abline(slope = 1, 
                  intercept = 0, 
                  color = 'gray90') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
  }
  
# Inter-rater stats -----
  
  rater_stats_ <- function(data, d_variable, m_variable, positive = 'yes') {
    
    ## computes inter-rater reliability stats 
    # for the disease and marker variables 
    # (both of them dichotomous and the same levels)
    # For internal use!
    
    # create a confusion matrix
    
    confusion_matrix <- 
      confusionMatrix(reference = data[[d_variable]], 
                      data = data[[m_variable]], 
                      positive = positive)

    # output
    
    tibble(kappa = confusion_matrix$overall['Kappa'],
           Se = confusion_matrix$byClass['Sensitivity'],
           Sp = confusion_matrix$byClass['Specificity'],
           accuracy = confusion_matrix$overall['Accuracy'], 
           n = sum(confusion_matrix$table), 
           n_disease = sum(confusion_matrix$table[, 2]))
  }
  
  bootstraps_tidy <- function(data, B = 1000) {
    
    ## creates B bootstraps of a data frame provided as the data argument
    ## will do in parallel when a backend is registered
    
    1:B %>% 
      set_names(paste0('rep_', 1:B)) %>% 
      future_map(function(x) slice_sample(data, 
                                          n = nrow(data), 
                                          replace = TRUE), 
                 .options = furrr_options(seed = TRUE))

  }
  
  rater_stats <- function(data, 
                          d_variable, 
                          m_variable, 
                          B = 1, 
                          conf.level = 0.95, 
                          positive = 'yes') {
    
    ## computes bootstrapped inter-rater stats
    ## for the disease and marker variables 
    ## (both of them dichotomous and the same levels)
    ## will employ a parallel backend if registered

    ## entry control
    
    stopifnot(is.data.frame(data))
    stopifnot(identical(levels(data[[d_variable]]), 
                        levels(data[[m_variable]])))
    
    stopifnot(is.numeric(B))
    stopifnot(B > 0)

    B <- as.integer(B)
    
    ## inter-rater stats for the original data
    
    data_stats <- rater_stats_(data, d_variable, m_variable)
    
    ## special case: no bootstraps, B = 1
    
    if(B == 1) return(data_stats)
    
    ## benchmarking
    
    start_time <- Sys.time()
    on.exit(message(paste('Elapsed:', Sys.time() - start_time)))
    
    ## bootstraps and confidence intervals
    
    boots <- bootstraps_tidy(data, B)
    
    boot_stats <- boots %>% 
      future_map_dfr(rater_stats_, 
                     d_variable = d_variable, 
                     m_variable = m_variable, 
                     positive = positive)
    
    boot_ci <- boot_stats[c('kappa', 'Se', 'Sp', 'accuracy')] %>% 
      #map(bca, conf.level = conf.level)
      map(quantile, probs = c(0.025, 0.975), na.rm = TRUE)
    
    ## the output tibble
    
    for(i in names(data_stats)) {
      
      data_stats <- data_stats %>% 
        mutate(!!paste0(i, '_lower') := boot_ci[[i]][1], 
               !!paste0(i, '_upper') := boot_ci[[i]][2])
      
      
    }
    
    data_stats
    
  } 
  
  plot_rater_stats <- function(data, 
                               response = 'CT_findings', 
                               split_factor = 'severity_class', 
                               split_palette = globals$sev_colors, 
                               split_labels = globals$sev_labels, 
                               plot_title = exchange(response, 
                                                     dict = covild$ct_lexicon) %>% 
                                 capitalize_first_char %>% 
                                 paste(c("Cohen's \u03BA", 'Sensitivity', 
                                         'Specificity', 'Accuracy'), 
                                       sep = ', '), 
                               plot_subtitle = 'Bootstrapped metrics, B = 2000', 
                               x_lab = list(expression(kappa * ', 95% CI'), 
                                            'Se, 95% CI', 'Sp, 95% CI', 
                                            'Accuracy, 95% CI'), 
                               signif_digits = 2, 
                               txt_size = 2.75, 
                               txt_hjust = 0.2, 
                               txt_vjust = -1.4) {
    
    ## generates a list of plots with the inter-rater stats for 
    ## the cohort subsets defined by the split-factor variable
    
    ## plotting data and variables
    
    data <- data %>% 
      filter(.data[['response']] == !!response)
    
    plot_vars <- list(kappa = 'kappa', 
                      Se = 'Se', 
                      Sp = 'Sp', 
                      accuracy = 'accuracy') %>% 
      map(~c(.x, paste0(.x, c('_lower', '_upper')), 
             'n', 'n_disease', split_factor))
    
    data <- plot_vars %>% 
      map(~data[.x]) %>% 
      map(set_names, 
          c('value', 'lower_ci', 'upper_ci', 'n', 'n_disease', 'split_factor')) %>% 
      map(mutate, 
          plot_lab = paste0(signif(value, signif_digits), 
                            ' [', signif(lower_ci, signif_digits), 
                            ' - ', signif(upper_ci, signif_digits), ']'), 
          axis_lab = split_labels[as.character(split_factor)], 
          axis_lab = paste0(axis_lab, 
                            '\ntotal: n = ', n, 
                            '\nevents: n = ', n_disease), 
          split_factor := factor(split_factor, rev(levels(split_factor))))
    
    axis_labs <- data %>% 
      map(~set_names(.x$axis_lab, .x$split_factor))
    
    ## plots
    
    list(x = data, 
         v = axis_labs, 
         y = plot_title, 
         z = x_lab) %>% 
      pmap(function(x, v, y, z) x %>% 
             ggplot(aes(x = value, 
                        y = split_factor, 
                        color = split_factor)) + 
             geom_errorbarh(aes(xmin = lower_ci, 
                                xmax = upper_ci), 
                            height = 0) + 
             geom_point(size = 2, 
                        shape = 16) + 
             geom_text(aes(label = plot_lab), 
                       size = txt_size, 
                       hjust = txt_hjust, 
                       vjust = txt_vjust) + 
             scale_color_manual(values = split_palette, 
                                name = '') +
             scale_y_discrete(labels = v) + 
             globals$common_theme + 
             theme(axis.title.y = element_blank()) + 
             labs(title = y, 
                  subtitle = plot_subtitle, 
                  x = z))
    
    
  }
  
# Effect size visualization ------
  
  plot_eff_bubble <- function(data, 
                              x_var = 'variable', 
                              y_var = 'severity_class', 
                              plot_title = NULL, 
                              plot_subtitle = "Friedman Test with Kendall's W effect size", 
                              plot_tag = NULL, 
                              strength = TRUE, 
                              txt_size = 2.75, 
                              txt_vjust = -1.4, 
                              fill_title = 'Effect size', ...) {
    
    ## Plots effect sizes in a bubble plot for
    ## testing results in the entire cohorts and severity strata
    
    data <- data %>% 
      mutate(significant = ifelse(p_adjusted < 0.05, 'yes', 'no'), 
             txt_face = ifelse(p_adjusted < 0.05, 'bold', 'plain'))
    
    if(strength) {
      
      plot <- data %>% 
        ggplot(aes(x = .data[[x_var]], 
                   y = .data[[y_var]], 
                   size = abs(estimate), 
                   fill = strength)) + 
        scale_fill_brewer(palette = 'Reds', 
                          name = fill_title, 
                          limits = levels(data[['strength']]))
      
    } else {
      
      plot <- data %>% 
        ggplot(aes(x = .data[[x_var]], 
                   y = .data[[y_var]], 
                   size = abs(estimate), 
                   fill = estimate)) + 
        scale_fill_gradient2(low = 'steelblue', 
                             mid = 'white', 
                             high = 'firebrick', 
                             midpoint = 0.5, 
                             limits = c(0, 1), 
                             name = fill_title)
      
    }
    
    plot + 
      geom_point(shape = 21) + 
      geom_text(aes(label = signif(estimate, 2),
                    alpha = significant, 
                    fontface = txt_face), 
                size = txt_size, 
                vjust = txt_vjust, ...) + 
      scale_alpha_manual(values = c(no = 0.3, 
                                    yes = 1)) + 
      guides(alpha = 'none') + 
      globals$common_theme + 
      theme(axis.title = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
  }
  
# Plotting kinetic -------
  
  plot_kinetic <- function(data, 
                           time_var = 'follow_up', 
                           y_var = 'CTSS', 
                           group_var = 'ID', 
                           form = c('box', 'ribbon'), 
                           fill_color = 'steelblue', 
                           fill_alpha = 0.4, 
                           median_size = 1, 
                           line_color = 'gray60', 
                           line_alpha = 0.5, 
                           line_width = 0.5, 
                           point_color = 'gray60', 
                           point_size = 2, 
                           point_alpha = 0.5, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           x_lab = 'Follow-up after COVID-19', 
                           y_lab = y_var, 
                           plot_tag = NULL) {
    
    ## plots a box plot with and overlay of single trajectories
    
    form <- match.arg(form, c('box', 'ribbon'))
    
    data <- data %>% 
      complete_cases(group_var)
    
    plot <- data %>% 
      ggplot(aes(x = .data[[time_var]], 
                 y = .data[[y_var]])) +
      geom_line(aes(group = .data[[group_var]]), 
                color = line_color, 
                alpha = line_alpha, 
                linewidth = line_width) + 
      geom_point(shape = 16, 
                 color = point_color, 
                 size = point_size, 
                 alpha = point_alpha)
    
    if(form == 'box') {
      
      plot <- plot + 
        geom_boxplot(outlier.colour = NA, 
                     alpha = fill_alpha, 
                     fill = fill_color)
      
    } else {
      
      med_tbl <- data %>% 
        group_by(.data[[time_var]]) %>% 
        summarise(median = median(.data[[y_var]]), 
                  lower_q = quantile(.data[[y_var]], 0.25), 
                  upper_q = quantile(.data[[y_var]], 0.75)) %>% 
        mutate(group_var = 'average')
      
      plot <- plot + 
        geom_ribbon(data = med_tbl, 
                    aes(y = median, 
                        ymin = lower_q, 
                        ymax = upper_q, 
                        group = group_var), 
                    alpha = fill_alpha, 
                    fill = fill_color, 
                    color = NA) + 
        geom_line(data = med_tbl, 
                  aes(y = median, 
                      group = group_var), 
                  color = fill_color, 
                  linewidth = median_size)
      
    }
    
    plot + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
  }
  
# Tuning functions -------
  
  youden_tuner <- function(data, lev = NULL, model = NULL) {
    
    ## allows for using the Youden's J criterion as a tuning metric
    
    multi_sum <- multiClassSummary(data, lev, model)
    
    multi_sum['J'] <- multi_sum['Sensitivity'] + multi_sum['Specificity']
    
    multi_sum
    
  }
  
# Tuning plots ------
  
  plot_ranger_tuning <- function(model, 
                                 plot_title = NULL) {
    
    ## tuning results ------
    
    tune_res <- as_tibble(model$results)
    
    if('J' %in% names(tune_res)) {
      
      plot_var <- 'J'
      y_lab <- "Youden's J, Se + Sp"
      
    } else if('Kappa' %in% names(tune_res)) {
      
      plot_var <- 'Kappa'
      y_lab <- expression("Cohen's " * kappa)
      
    } else {
      
      plot_var <- 'MAE'
      y_lab <- 'Mean absolute error, MAE'
      
    }
    
    ## tuning plot -------
    
    tune_labeller <- unique(tune_res$min.node.size)
    
    tune_labeller <- paste('min.node size =', tune_labeller) %>% 
      set_names(tune_labeller) %>% 
      as_labeller
    
    best_tune <- model$bestTune
    
    plot_subtitle <- map2_chr(names(best_tune), best_tune, 
                              paste, sep = ' = ') %>% 
      paste(collapse = ', ')
    
    tune_res %>% 
      mutate(min.node.size = factor(min.node.size)) %>% 
      ggplot(aes(x = mtry, 
                 y = .data[[plot_var]], 
                 color = splitrule)) + 
      facet_grid(. ~ min.node.size, 
                 labeller = tune_labeller) + 
      geom_path() + 
      geom_point(shape = 16, 
                 size = 2) + 
      scale_color_manual(values = c(gini = 'steelblue', 
                                    extratrees = 'firebrick4', 
                                    variance = 'steelblue'), 
                         labels = c(gini = 'Gini', 
                                    extratrees = 'Extra trees'), 
                         name = 'Splitting rule') +
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = 'Number of variable per tree, mtry', 
           y = y_lab)
    
  }
  
  plot_nnet_tuning <- function(model, 
                               plot_title = NULL) {
    
    ## tuning results -----
    
    tune_res <- as_tibble(model$results)
    
    if('J' %in% names(tune_res)) {
      
      plot_var <- 'J'
      y_lab <- "Youden's J, Se + Sp"
      
    } else if('Kappa' %in% names(tune_res)) {
      
      plot_var <- 'Kappa'
      y_lab <- expression("Cohen's " * kappa)
      
    } else {
      
      plot_var <- 'MAE'
      y_lab <- 'Mean absolute error, MAE'
      
    }
    
    ## plot -------
    
    tune_labeller <- unique(tune_res$size)
    
    tune_labeller <- paste('Size =', tune_labeller) %>% 
      set_names(tune_labeller) %>% 
      as_labeller
    
    best_tune <- model$bestTune
    
    plot_subtitle <- map2_chr(names(best_tune), best_tune, 
                              paste, sep = ' = ') %>% 
      paste(collapse = ', ')
    
    tune_res %>% 
      mutate(size = factor(size, sort(unique(size)))) %>%
      ggplot(aes(x = decay, 
                 y = .data[[plot_var]], 
                 color = size)) + 
      facet_grid(size ~ ., 
                 labeller = tune_labeller) + 
      geom_path() +
      geom_point(shape = 16,
                 size = 2) + 
      scale_color_viridis_d() + 
      scale_x_continuous(trans = 'log10') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           y = y_lab,
           x = 'Decay')
    
  }
  
  plot_svm_tuning <- function(model, 
                              plot_title = NULL) {
    
    ## tuning results ------
    
    tune_res <- as_tibble(model$results)
    
    if('J' %in% names(tune_res)) {
      
      plot_var <- 'J'
      y_lab <- "Youden's J, Se + Sp"
      
    } else if('Kappa' %in% names(tune_res)) {
      
      plot_var <- 'Kappa'
      y_lab <- expression("Cohen's " * kappa)
      
    } else {
      
      plot_var <- 'MAE'
      y_lab <- 'Mean absolute error, MAE'
      
    }
    
    ## plot -------
    
    best_tune <- model$bestTune
    
    plot_subtitle <- map2_chr(names(best_tune), best_tune, 
                              paste, sep = ' = ') %>% 
      paste(collapse = ', ')
    
    tune_res %>% 
      ggplot(aes(x = C, 
                 y = .data[[plot_var]])) + 
      geom_path(color = 'steelblue') + 
      geom_point(shape = 16, 
                 size = 2, 
                 color = 'steelblue') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = 'Cost paramater, C', 
           y = y_lab)
    
  }
  
  plot_gbm_tuning <- function(model, 
                              plot_title = NULL) {
    
    ## tuning results ------
    
    tune_res <- as_tibble(model$results)
    
    if('J' %in% names(tune_res)) {
      
      plot_var <- 'J'
      y_lab <- "Youden's J, Se + Sp"
      
    } else if('Kappa' %in% names(tune_res)) {
      
      plot_var <- 'Kappa'
      y_lab <- expression("Cohen's " * kappa)
      
    } else {
      
      plot_var <- 'MAE'
      y_lab <- 'Mean absolute error, MAE'
      
    }
    
    ## plot -------
    
    best_tune <- model$bestTune
    
    plot_subtitle <- map2_chr(names(best_tune), best_tune, 
                              paste, sep = ' = ') %>% 
      paste(collapse = ', ')
    
    y_labs <- unique(tune_res$shrinkage)
    
    y_labs <- paste('shrinkage =', y_labs) %>% 
      set_names(y_labs)
    
    x_labs <- unique(tune_res$interaction.depth)
    
    x_labs <- paste('int =', x_labs) %>% 
      set_names(x_labs)
    
    tune_res %>% 
      ggplot(aes(x = n.trees, 
                 y = .data[[plot_var]],
                 color = factor(n.minobsinnode))) + 
      facet_grid(shrinkage ~ interaction.depth, 
                 labeller = labeller(.rows = as_labeller(y_labs), 
                                     .cols = as_labeller(x_labs))) + 
      geom_path() + 
      geom_point(shape = 16, 
                 size = 2) + 
      scale_color_manual(values = c('5' = 'steelblue', 
                                    '10' = 'indianred2', 
                                    '15' = 'orangered4'), 
                         name = 'Minimal observations per node') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = 'Number of trees, n.trees', 
           y = y_lab)
    
  }
  
  plot_resample_kappa <- function(model, 
                                  plot_title = NULL, 
                                  color = 'steelblue') {
    
    ## plots Cohen's kappa or MAE in the resamples
    
    resamp_data <- as_tibble(model$resample)
    
    if('J' %in% names(resamp_data)) {
      
      plot_var <- 'J'
      x_lab <- 'Data resample, sorted by J'
      y_lab <- "Youden's J, Se + Sp"
      
    } else if('Kappa' %in% names(resamp_data)) {
      
      plot_var <- 'Kappa'
      x_lab <- expression('Data resample, sorted by ' * kappa)
      y_lab <- expression("Cohen's " * kappa)
  
    } else {
      
      plot_var <- 'MAE'
      x_lab <- 'Data resample, sorted by MAE'
      y_lab <- 'Mean absolute error, MAE'
      
    }
    
    resamp_data <- resamp_data %>% 
      arrange(.data[[plot_var]]) %>% 
      mutate(order = 1:nrow(.))
    
    resamp_data %>% 
      ggplot(aes(x = order, 
                 y = .data[[plot_var]])) + 
      geom_path(color = color) + 
      geom_point(shape = 16, 
                 size = 2, 
                 color = color) + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = '10-repeats 10-fold cross-validation', 
           x = x_lab,
           y = y_lab)
    
  }
  
# Model performance ------

  format_ml_summary <- function(summary_lst) {
    
   summary_lst %>% 
      map(transpose) %>% 
      map(map, map, select, statistic, estimate) %>% 
      map(map, map, column_to_rownames, 'statistic') %>% 
      map(map, map, t) %>% 
      map(map, map, as.data.frame) %>% 
      map(map, compress, names_to = 'algorithm') %>% 
      map(compress, names_to = 'dataset') %>% 
      map(as_tibble) %>% 
      map(relocate, algorithm) %>% 
      map(relocate, dataset)
    
  }
  
  format_strata_summary <- function(summary_lst, strata_name = 'strata') {
    
    summary_lst %>% 
      map(~.x[c('statistic', 'estimate')]) %>% 
      map(column_to_rownames, 'statistic') %>% 
      map(t) %>% 
      map(as.data.frame) %>% 
      compress(names_to = strata_name) %>% 
      relocate(.data[[strata_name]]) %>% 
      as_tibble
    
  }
  
  plot_binary_performance <- function(stats, 
                                      model, 
                                      plot_title = NULL, 
                                      label_accuracy = TRUE, ...) {
    
    ## plots Brier score versus Cohen's kappa
    ## total and number of events in the training dataset 
    ## are displayed in the plot captions
    
    n_numbers <- ngroups(model)$train
    
    plot_subtitle <- paste0('total: n = ', sum(n_numbers$n_outcome), 
                            ', events: n = ', n_numbers$n_outcome[2])
    
    ## plotting 
    
    stat_plot <- stats %>% 
      ggplot(aes(x = kappa, 
                 y = 1 - brier_score, 
                 color = algorithm, 
                 fill = algorithm, 
                 shape = dataset, 
                 size = correct_rate)) + 
      geom_hline(yintercept = 0.75,
                 linetype = 'dashed') + 
      geom_vline(xintercept = 0, 
                 linetype = 'dashed') + 
      geom_point() + 
      scale_color_manual(values = globals$algo_colors, 
                         labels = globals$algo_labs, 
                         name = 'Algorithm') + 
      scale_fill_manual(values = globals$algo_colors, 
                        labels = globals$algo_labs, 
                        name = 'Algorithm') + 
      scale_shape_manual(values = c(train = 1, 
                                    cv = 21), 
                         labels = globals$dataset_lab, 
                         name = 'Data set') + 
      scale_radius(name = 'Overalll accuracy') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle,
           x = expression("Cohen's " * kappa), 
           y = '1 - Brier score')
    
    if(!label_accuracy) return(stat_plot)
    
    stat_plot + 
      geom_text_repel(aes(label = signif(correct_rate, 2)),
                      size = 2.75, 
                      show.legend = FALSE, ...)
    
  }
  
  plot_confusion_hm <- function(train_predictions, 
                                cv_predictions, 
                                title_prefix, ...) {
    
    ## draws a heat map representation of the confusion matrices
    
    ## determining the fill scale limits and n numbers
    
    train_limits <- train_predictions %>% 
      map(confusion, scale = 'percent') %>% 
      unlist %>% 
      range
    
    train_n <- stats::nobs(train_predictions[[1]])
    
    ## training plots 
    
    train_plots <- 
      list(x = train_predictions, 
           plot_title = paste(title_prefix, 
                              globals$algo_labs[names(train_predictions)], 
                              'training', 
                              sep = ', ')) %>% 
      pmap(plot, 
           type = 'confusion', 
           scale = 'percent', 
           cust_theme = globals$common_theme)
    
    ## CV plots
    
    cv_plots <- 
      list(x = cv_predictions, 
           plot_title = paste(title_prefix, 
                              globals$algo_labs[names(cv_predictions)], 
                              'CV', 
                              sep = ', ')) %>% 
      pmap(plot, 
           type = 'confusion', 
           scale = 'percent', 
           cust_theme = globals$common_theme)
    
    ## common formats
    
    out_lst <- list(train = train_plots, 
                    cv = cv_plots)
    
    for(i in names(out_lst)) {
      
      out_lst[[i]] <- out_lst[[i]] %>% 
        map(~.x + 
              scale_fill_gradient2(low = 'steelblue', 
                                   mid = 'white', 
                                   high = 'firebrick', 
                                   limits = train_limits, 
                                   midpoint = mean(train_limits), 
                                   name = '% of observations') + 
              labs(subtitle = paste(.x$labels$subtitle, 
                                    train_n, 
                                    sep = ', n = '), 
                   x = 'Observed percentage', 
                   y = 'Predicted percentage'))
      
    }
    
    out_lst
    
  }
  
  plot_ml_roc <- function(train_predictions, 
                          cv_predictions, 
                          stats, 
                          title_prefix, 
                          annot_y = 0.1, 
                          annot_x = 0.4, 
                          increment = 0.05, 
                          txt_size = 2.75) {
    
    ## ROC curves for algorithms in the training and test data
    
    ## prediction data
    
    pred_data <- list(train = train_predictions, 
                      cv = cv_predictions) %>% 
      map(map, model.frame) %>% 
      map(compress, names_to = 'algorithm') %>% 
      map(mutate, algorithm = factor(algorithm, names(globals$algo_labs)))
    
    ## number of events in the training data
    
    n_numbers <- ngroups(train_predictions[[1]])[['n_outcome']]
    
    plot_subtitle <- paste0('total: n = ', sum(n_numbers), 
                            ', events: n = ', n_numbers[2])
    
    ## ROC stats to be displayed in the plot: AUC, Se and Sp
    
    stats <- stats %>% 
      mutate(dataset = factor(dataset, c('train', 'cv')), 
             algorithm = factor(algorithm, names(globals$algo_labs)), 
             plot_lab = paste0('AUC = ', signif(AUC, 2), 
                               ', Se = ', signif(Se, 2),
                               ', Sp = ', signif(Sp, 2), 
                               ', J = ', signif(J, 2))) %>% 
      arrange(dataset, algorithm) %>% 
      blast(dataset) %>% 
      map(~set_names(.x$plot_lab, .x$algorithm))
    
    ## ROC plots 
    
    roc_plots <- 
      list(x = pred_data, 
           y = paste(title_prefix, 
                     c('training', 'CV'), 
                     sep = ', ')) %>% 
      pmap(function(x, y) x %>% 
             ggplot(aes(d = .outcome, 
                        m = yes, 
                        color = algorithm)) + 
             geom_roc(cutoffs.at = 0.5, 
                      labels = FALSE, 
                      pointsize = 1) + 
             scale_color_manual(values = globals$algo_colors, 
                                labels = globals$algo_labs, 
                                name = 'Algorithm') + 
             style_roc(xlab = '1 - Sp', 
                       ylab = 'Se', 
                       guide = TRUE) + 
             geom_abline(slope = 1, 
                         intercept = 0, 
                         color = 'gray90') + 
             globals$common_theme + 
             labs(title = y, 
                  subtitle = plot_subtitle))
    
    for(i in names(roc_plots)) {
      
      y_pos <- annot_y
      
      for(j in rev(names(stats[[i]]))) {
        
        roc_plots[[i]] <- roc_plots[[i]] + 
          annotate('text', 
                   label = stats[[i]][[j]], 
                   x = annot_x, 
                   y = y_pos, 
                   color = globals$algo_colors[[j]], 
                   hjust = 0, 
                   vjust = 0, 
                   size = txt_size)
        
        y_pos <- y_pos + increment
        
      }
      
    }
    
    roc_plots
    
  } 
  
  plot_binary_strata_stats <- function(stats, 
                                       strata = 'severity_class', 
                                       plot_title = 'DLCO < 80%, COVID-19 severity', 
                                       x_lab = 'COVID-19 severity') {
    
    stats <- stats %>% 
      mutate(brier_score = 1 - brier_score)
    
    metrics <- c('AUC', 'Se', 'Sp', 'J', 
                 'kappa', 'brier_score')
    
    list(x = metrics, 
         y = list('AUC', 'Se', 'Sp', "Youden's J", 
                  expression("Cohen's " * kappa), '1 - Brier score')) %>% 
      pmap(function(x, y) stats %>% 
             ggplot(aes(x = .data[[strata]], 
                        y = .data[[x]], 
                        color = algorithm)) + 
             geom_path(aes(group = algorithm)) + 
             geom_point(size = 2, 
                        shape = 16) + 
             scale_color_manual(values = globals$algo_colors, 
                                labels = globals$algo_labs, 
                                name = 'Algorithm') + 
             globals$common_theme + 
             labs(title = plot_title,
                  subtitle = 'Performance in cross-validation', 
                  x = x_lab,
                  y = y)) %>% 
      set_names(metrics)
    
  }
  
  plot_binary_course <- function(caretx_model, 
                                 title_prefix = 'Random Forest', 
                                 y_lab = 'DLCO < 80%, % of cohort strata', 
                                 se_multiplier = 1, 
                                 hide_training = TRUE, 
                                 line_width = 1) {
    
    ## plots the actual and fitted (training and CV) frequency
    ## of a LFT finding in the entire cohort and the CoV severity strata
    
    ## plotting data -------
    
    plot_data <- caretx_model %>% 
      augment
    
    plot_data$train <- plot_data$train %>% 
      mutate(outcome = as.numeric(.outcome) - 1,
             train = as.numeric(.fitted) - 1) %>%
      select(.observation, outcome, train, 
             follow_up, severity_class)
    
    plot_data$cv <- plot_data$cv %>% 
      mutate(cv = as.numeric(.fitted) - 1) %>% 
      select(.observation, cv) %>% 
      summarise(cv = mean(cv), .by = .observation)
    
    plot_data <- plot_data %>% 
      reduce(left_join, by = '.observation')
    
    plot_data <- rbind(mutate(plot_data, 
                              severity_class = 'cohort'), 
                       plot_data) %>% 
      mutate(severity_class = factor(severity_class, 
                                     c('cohort', levels(plot_data$severity_class))))
    
    plot_data <- blast(plot_data, severity_class) %>% 
      map(select, -severity_class)
    
    n_numbers <- map_dbl(plot_data, nrow)
    
    ## summarising the data: mean with n times SEM --------
    
    plot_data <- plot_data %>% 
      map(pivot_longer, 
          cols = all_of(c('outcome', 'train', 'cv')), 
          names_to = 'dataset', 
          values_to = 'positive')
    
    plot_data <- plot_data %>% 
      map(group_by, follow_up, dataset) %>% 
      map(~summarise(.x, 
                     mean_pos = mean(positive), 
                     lower_lim = mean_pos - se_multiplier * se(positive), 
                     upper_lim = mean_pos + se_multiplier * se(positive))) %>% 
      map(ungroup) %>% 
      map(mutate, 
          lower_lim = ifelse(lower_lim < 0, 0, lower_lim), 
          dataset = factor(dataset, 
                           c('outcome', 'train', 'cv')))
    
    if(hide_training) {
      
      plot_data <- plot_data %>% 
        map(filter, dataset != 'train') %>% 
        map(mutate, dataset = droplevels(dataset))
      
    }
    
    ## plots -------
    
    title_suff <- globals$sev_labels[names(plot_data)]
    
    title_suff <- ifelse(stri_detect(title_suff, fixed = 'cohort'), 
                         title_suff, paste(title_suff, 'COVID-19'))
    
    list(x = plot_data, 
         y = paste(title_prefix, title_suff, sep = ', '), 
         z = paste('n =', n_numbers)) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = follow_up, 
                        y = mean_pos * 100, 
                        color = dataset, 
                        fill = dataset)) + 
             geom_ribbon(aes(group = dataset, 
                             ymin = lower_lim * 100, 
                             ymax = upper_lim * 100), 
                         alpha = 0.25, 
                         color = NA) + 
             geom_path(aes(group = dataset), 
                       linewidth = line_width) + 
             scale_color_manual(values = globals$dataset_colors, 
                                labels = globals$dataset_lab, 
                                name = 'Dataset') + 
             scale_fill_manual(values = globals$dataset_colors, 
                               labels = globals$dataset_lab, 
                               name = 'Dataset') + 
             scale_x_discrete(labels = globals$fup_labels) + 
             globals$common_theme + 
             labs(title = y, 
                  subtitle = z, 
                  x = 'Follow-up post COVID-19', 
                  y = y_lab))
    
  }
  
  plot_reg_performance <- function(stats, 
                                   model, 
                                   rsq_type = c('pseudo', 'caret'), 
                                   plot_title = NULL, 
                                   invert_mae = TRUE, 
                                   label_correlation = TRUE, ...) {
    
    ## plots MAE and pseudo R-square for the training and CV datasets
    
    rsq_type <- match.arg(rsq_type[1], c('pseudo', 'caret'))
    
    ## n numbers to be presented in the plot caption
    
    plot_subtitle <- paste('n =', stats::nobs(model))
    
    ## plotting
    
    r_var <- switch(rsq_type, 
                    pseudo = 'rsq', 
                    caret = 'caret_rsq')
    
    stats <- stats %>% 
      mutate(rsq = ifelse(rsq < 0, 0, rsq), 
             caret_rsq = ifelse(is.na(caret_rsq), 0, caret_rsq))
    
    stats[c('caret_rsq', 'pearson', 'spearman', 'kendall')] <- 
      stats[c('caret_rsq', 'pearson', 'spearman', 'kendall')] %>% 
      map(~ifelse(is.na(.x), 0, .x))
    
    if(!invert_mae) {
      
      stat_plot <- stats %>% 
        ggplot(aes(x = .data[[r_var]], 
                   y = MAE, 
                   size = spearman, 
                   color = algorithm, 
                   fill = algorithm, 
                   shape = dataset))
      
    } else {
      
      stat_plot <- stats %>% 
        ggplot(aes(x = .data[[r_var]], 
                   y = 1/MAE, 
                   size = spearman, 
                   color = algorithm, 
                   fill = algorithm, 
                   shape = dataset))
      
    }
    
    stat_plot <- stat_plot + 
      geom_point() + 
      scale_color_manual(values = globals$algo_colors, 
                         labels = globals$algo_labs, 
                         name = 'Algorithm') + 
      scale_fill_manual(values = globals$algo_colors, 
                        labels = globals$algo_labs, 
                        name = 'Algorithm') + 
      scale_shape_manual(values = c(train = 1, 
                                    cv = 21), 
                         labels = globals$dataset_lab, 
                         name = 'Data set') + 
      scale_radius(name = 'Overall accuracy') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle,
           x = expression('R'^2), 
           y = if(invert_mae) expression('MAE'^{-1}) else 'MAE')
    
    if(!label_correlation) return(stat_plot)
    
    stat_plot + 
      geom_text_repel(aes(label = signif(spearman, 2)),
                      size = 2.75, 
                      show.legend = FALSE, ...)
    
  }
  
  plot_fit_observed <- function(train_predictions, 
                                cv_predictions, 
                                stats, 
                                title_prefix, 
                                show_trend = TRUE, ...) {
    
    ## plots fitted and observed values
    ## displays Spearman's rho and the number of complete observations
    ## in the plot caption
    
    ## plot meta -------
    
    train_n <- stats::nobs(train_predictions[[1]])
    
    stats <- stats %>% 
      mutate(dataset = factor(dataset, c('train', 'cv')), 
             algorithm = factor(algorithm, names(train_predictions)), 
             plot_subtitle = paste0("Spearman's \u03C1 = ", 
                                    signif(spearman, 2), 
                                    ', n = ', train_n))
    
    plot_subtitles <- stats %>% 
      blast(dataset) %>% 
      map(~.x$plot_subtitle)
    
    ## plotting data -------
    
    plot_data <- list(train = train_predictions, 
                      cv = cv_predictions) %>% 
      map(map, model.frame)
    
    ## training plots -------
    
    train_plots <- 
      list(x = plot_data$train, 
           y = paste(title_prefix, 
                     globals$algo_labs[names(train_predictions)], 
                     'training', 
                     sep = ', '), 
           z = plot_subtitles$train) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = .outcome, 
                        y = .fitted)) + 
             geom_abline(slope = 1, 
                         intercept = 0, 
                         linetype = 'dashed') + 
             geom_point(size = 2, 
                        shape = 21, 
                        alpha = 0.5, 
                        fill = globals$dataset_colors["train"]) + 
             labs(title = y, 
                  subtitle = z))
    
    ## CV plots -------
    
    cv_plots <- 
      list(x = plot_data$cv, 
           y = paste(title_prefix, 
                     globals$algo_labs[names(train_predictions)], 
                     'CV', 
                     sep = ', '), 
           z = plot_subtitles$cv) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = .outcome, 
                        y = .fitted)) + 
             geom_abline(slope = 1, 
                         intercept = 0, 
                         linetype = 'dashed') + 
             geom_point(size = 2, 
                        shape = 21, 
                        alpha = 0.5, 
                        fill = globals$dataset_colors["cv"]) + 
             labs(title = y, 
                  subtitle = z))
    
    ## common formats ------
    
    out_lst <- list(train = train_plots, 
                    cv = cv_plots)
    
    for(i in names(out_lst)) {
      
      out_lst[[i]] <- out_lst[[i]] %>% 
        map(~.x + 
              globals$common_theme + 
              labs(x = 'Observed % of reference value', 
                   y = 'Predicted % of reference value'))
      
      if(show_trend) {
        
        out_lst[[i]] <- out_lst[[i]] %>% 
          map(~.x + 
                geom_smooth(method = 'gam', 
                            formula = y ~ s(x, bs = "tp")))
        
      }
      
    }
    
    out_lst

  }
  
  plot_reg_strata_stats <-  function(stats, 
                                     strata = 'severity_class', 
                                     plot_title = 'DLCO, COVID-19 severity', 
                                     x_lab = 'COVID-19 severity', 
                                     invert_mae = TRUE) {
    
    ## Plots MAE, pseudo R-squared, caret's R-squared and Spearman's rho
    ## in the subsets of the dataset
    
    metrics <- c('MAE', 'rsq', 'caret_rsq', 'spearman')
    
    if(invert_mae) {
      
      stats <- stats %>% 
        mutate(MAE = 1/MAE)
      
      metric_labs <- list(expression('MAE'^{-1}), 
                          expression('R'^2), 
                          expression('corr R'^2), 
                          expression("Spearman's " * rho))
      
    } else {
      
      metric_labs <- list('MAE', 
                          expression('R'^2), 
                          expression('corr R'^2), 
                          expression("Spearman's " * rho))
      
    }
    
    list(x = metrics, 
         y = metric_labs) %>% 
      pmap(function(x, y) stats %>% 
             ggplot(aes(x = .data[[strata]], 
                        y = .data[[x]], 
                        color = algorithm)) + 
             geom_path(aes(group = algorithm)) + 
             geom_point(size = 2, 
                        shape = 16) + 
             scale_color_manual(values = globals$algo_colors, 
                                labels = globals$algo_labs, 
                                name = 'Algorithm') + 
             globals$common_theme + 
             labs(title = plot_title,
                  subtitle = 'Performance in cross-validation', 
                  x = x_lab,
                  y = y)) %>% 
      set_names(metrics)
    
  }
  
  plot_reg_course <- function(caretx_model, 
                              title_prefix = 'Random Forest', 
                              y_lab = 'DLCO < 80%, % of cohort strata', 
                              se_multiplier = 1, 
                              hide_training = TRUE, 
                              line_width = 1) {
    
    ## plotting data -----
    
    plot_data <- caretx_model %>% 
      augment
    
    plot_data$train <- plot_data$train %>% 
      transmute(.observation = .observation, 
                outcome = .outcome, 
                train = .fitted, 
                severity_class = severity_class, 
                follow_up = follow_up)
    
    plot_data$cv <- plot_data$cv %>%
      group_by(.observation) %>% 
      summarise(cv = mean(.fitted)) %>% 
      ungroup
    
    plot_data <- reduce(plot_data, left_join, by = '.observation')
    
    plot_data <- rbind(mutate(plot_data, 
                              severity_class = 'cohort'), 
                       plot_data) %>% 
      mutate(severity_class = factor(severity_class, 
                                     c('cohort', levels(plot_data$severity_class))))
    
    plot_data <- blast(plot_data, severity_class) %>% 
      map(select, -severity_class)
    
    n_numbers <- map_dbl(plot_data, nrow)
    
    ## summarising the data: mean with n times SEM --------
    
    plot_data <- plot_data %>% 
      map(pivot_longer, 
          cols = all_of(c('outcome', 'train', 'cv')), 
          names_to = 'dataset', 
          values_to = 'percent')
    
    plot_data <- plot_data %>% 
      map(group_by, follow_up, dataset) %>% 
      map(~summarise(.x, 
                     mean_percent = mean(percent), 
                     lower_lim = mean_percent - se_multiplier * se(percent), 
                     upper_lim = mean_percent + se_multiplier * se(percent))) %>% 
      map(ungroup) %>% 
      map(mutate, 
          lower_lim = ifelse(lower_lim < 0, 0, lower_lim), 
          dataset = factor(dataset, 
                           c('outcome', 'train', 'cv')))
    
    if(hide_training) {
      
      plot_data <- plot_data %>% 
        map(filter, dataset != 'train') %>% 
        map(mutate, dataset = droplevels(dataset))
      
    }
    
    ## plotting --------
    
    title_suff <- globals$sev_labels[names(plot_data)]
    
    title_suff <- ifelse(stri_detect(title_suff, fixed = 'cohort'), 
                         title_suff, paste(title_suff, 'COVID-19'))
    
    list(x = plot_data, 
         y = paste(title_prefix, title_suff, sep = ', '), 
         z = paste('n =', n_numbers)) %>% 
      pmap(function(x, y, z) x %>% 
             ggplot(aes(x = follow_up, 
                        y = mean_percent, 
                        color = dataset, 
                        fill = dataset)) + 
             geom_ribbon(aes(group = dataset, 
                             ymin = lower_lim, 
                             ymax = upper_lim), 
                         alpha = 0.25, 
                         color = NA) + 
             geom_path(aes(group = dataset), 
                       linewidth = line_width) + 
             scale_color_manual(values = globals$dataset_colors, 
                                labels = globals$dataset_lab, 
                                name = 'Data set') + 
             scale_fill_manual(values = globals$dataset_colors, 
                               labels = globals$dataset_lab, 
                               name = 'Data set') + 
             scale_x_discrete(labels = globals$fup_labels) + 
             globals$common_theme + 
             labs(title = y, 
                  subtitle = z, 
                  x = 'Follow-up post COVID-19', 
                  y = y_lab))
    
  }
  
# Variable importance -------
  
  refit_ranger <- function(model, 
                           train_control = model$control) {
    
    ## re-fits a Ranger model to obtain permutation variance importance
    
    caret::train(form = formula(model), 
                 data = model.frame(model), 
                 method = 'ranger', 
                 metric = model$metric, 
                 tuneGrid = model$bestTune, 
                 importance = 'permutation', 
                 trControl = train_control, 
                 num.trees = 1000)
    
  }
  
  format_importance <- function(stats, 
                                dict = lft_globals$lexicon) {
    
    ## formats the output of the varImp function
    ## to extract the variable name and the variable level
    
    ext_regex <- sort(dict$variable, decreasing = TRUE) %>% 
      paste(collapse = '|')
    
    stats <- stats$importance %>% 
      as.data.frame %>% 
      rownames_to_column('parameter') %>% 
      as_tibble
    
    if(!'Overall' %in% names(stats)) {
      
      stats <- stats %>% 
        mutate(Overall = yes)
      
    }
    
    stats %>% 
      mutate(variable = stri_extract(parameter, regex = ext_regex), 
             level = stri_replace(parameter, 
                                  regex = ext_regex, 
                                  replacement = ''), 
             var_label = exchange(variable, 
                                  dict = dict), 
             plot_lab = ifelse(level == '', 
                               var_label, 
                               ifelse(level == 'yes', 
                                      var_label, 
                                      paste(var_label, level, sep = ': '))))
    
    
  }
  
  plot_caret_importance <- function(stat_lst, 
                                    title_prefix, 
                                    top_vars = 10) {
    
    ## plots the algorithm-specific feature importance metric 
    ## as a list of bar plots, each for the Ranger, NNet and SVM model
    
    metric_names <- 
      c(ranger = 'Permutation importance', 
        nnet = 'Weight importance', 
        svmRadial = 'ROC importance', 
        gbm = 'Tree importance')
    
    plot_titles <- paste(title_prefix, 
                         globals$algo_labs[names(stat_lst)], 
                         sep = ', ')
    
    stat_lst <- stat_lst %>% 
      map(top_n, n = top_vars, Overall)
    
    list(x = stat_lst, 
         y = plot_titles, 
         v = globals$algo_colors[names(stat_lst)], 
         z = metric_names[names(stat_lst)]) %>% 
      pmap(function(x, y, v, z) x %>% 
             ggplot(aes(x = Overall, 
                        y = reorder(plot_lab, Overall))) + 
             geom_bar(stat = 'identity', 
                      color = 'black', 
                      fill  = v) + 
             globals$common_theme +
             theme(axis.title.y = element_blank()) + 
             labs(title = y, 
                  x = z))
    
  }
  
# SHAP -----
  
  shap_background <- function(data, quant = 0.25) {
    
    ## generates a synthetic 'background' observation with 
    ## all explanatory variables set to their minimum (numeric) or 
    ## the baseline levels (factors)
    
    num_ex <- function(x) quantile(x, quant, na.rm = TRUE)
    
    fct_ex <- function(x) factor(levels(x)[1], levels(x))
    
    map_dfc(data, function(x) if(is.numeric(x)) num_ex(x) else fct_ex(x)) %>%
      as.data.frame
    
  }
  
  pred_binary <- function(object, X) {
    
    caret::predict.train(object, newdata = X, type = 'prob')[['yes']]
        
  }
  
  pred_reg <- function(object, X) caret::predict.train(object, newdata = X)
  
# General plotting tools ----
  
  scatter_plot <- function(data, 
                           x_var, 
                           y_var, 
                           fill_var = NULL, 
                           point_color = 'steelblue',
                           point_shape = 21, 
                           point_alpha = 1,
                           point_size = 2, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           x_lab = x_var, 
                           y_lab = y_var, 
                           plot_tag = element_blank(), 
                           point_hjitter = 0, 
                           point_wjitter = 0, ...) {
    
    if(is.null(fill_var)) {
      
      plot <- ggplot(data, 
                     aes(x = .data[[x_var]], 
                         y = .data[[y_var]])) + 
        geom_point(shape = point_shape, 
                   size = point_size, 
                   alpha = point_alpha, 
                   fill = point_color, 
                   position = position_jitter(width = point_wjitter, 
                                              height = point_hjitter), ...)
      
    } else {
      
      plot <- ggplot(data, 
                     aes(x = .data[[x_var]], 
                         y = .data[[y_var]], 
                         fill = .data[[fill_var]], 
                         color = .data[[fill_var]])) + 
        geom_point(shape = point_shape, 
                   size = point_size, 
                   alpha = point_alpha, 
                   position = position_jitter(width = point_wjitter, 
                                              height = point_hjitter), ...)
      
    }
    
    plot + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
  }
  
  same_scale_calib <- function(plot) {
    
    ## Sets the same numeric scale for the X and Y axis
    ## of a caretExtra's calibration plot
    
    ## Get the limits of the x and y axes
    
    limits <- range(c(plot$data[['.outcome']], 
                      plot$data[['.fitted']]))
    
    ## Set the limits of both axes to be the same
    
    plot + 
      expand_limits(x = limits, 
                    y = limits)

  }
  
# Cache access -------
  
  access_cache <- function(cache_path, 
                           script_path, 
                           message = 'Loading cached results') {
    
    if(file.exists(cache_path)) {
      
      insert_msg(message)
      
      load(cache_path, envir = .GlobalEnv)

    } else {
      
      source_all(script_path, 
                 message = TRUE, 
                 crash = TRUE)
      
    }
    
  }
  
# END -----