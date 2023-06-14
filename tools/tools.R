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
        mutate(n_lab = paste('n =', n), 
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
                                 paste(c("Cohen's kapps", 'Sensitivity', 
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
                            ', events: n = ', n_disease), 
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
  
# Tuning plots ------
  
  plot_binary_tuning <- function(data, 
                                 plot_title = 'Random Forest', 
                                 plot_subtitle = '5-times repeated 10-fold CV', 
                                 plot_tag = NULL, 
                                 show_sd = FALSE, ...) {
    
    data <- data %>% 
      mutate(node_lab = paste('min.node.size = ', min.node.size))
    
    plots <- c('Accuracy', 'Kappa') %>% 
      map(~ggplot(data, 
                  aes(x = mtry, 
                      y = .data[[.x]], 
                      color = splitrule, 
                      fill = splitrule)) + 
            facet_grid(. ~ node_lab, 
                       space = 'free', 
                       scales = 'free'))
    
    if(show_sd) {
      
      plots <- 
        list(x = plots, 
             y = c('Accuracy', 'Kappa'), 
             z = c('AccuracySD', 'KappaSD'), 
             v = c("Accuracy, SD", 
                   "Cohen's \u03BA, SD")) %>% 
        pmap(function(x, y, z) x + 
               geom_ribbon(aes(ymin = .data[[y]] - .data[[z]], 
                               ymax = .data[[y]] + .data[[z]]), 
                           alpha = 0.3, 
                           color = NA) + 
               labs(y = v))
      
    } else {
      
      plots <- 
        map2(plots, 
             c("Accuracy", 
               "Cohen's \u03BA"), 
             ~.x + 
               labs(y = .y))
      
    }
    
    map2(plots, 
         paste(plot_title, c('accuracy', 'reliability')), 
         ~.x + 
           geom_line() + 
           labs(title = .y, 
                subtitle = plot_subtitle, 
                tag = plot_tag) + 
           globals$common_theme) %>% 
      set_names(c('accuracy', 'kappa'))
    
  }
  
  plot_regression_tuning <- function(data, 
                                 plot_title = 'Random Forest', 
                                 plot_subtitle = '5-times repeated 10-fold CV', 
                                 plot_tag = NULL, 
                                 show_sd = FALSE, ...) {
    
    data <- data %>% 
      mutate(node_lab = paste('min.node.size = ', min.node.size))
    
    plots <- c('RMSE', 'Rsquared') %>% 
      map(~ggplot(data, 
                  aes(x = mtry, 
                      y = .data[[.x]], 
                      color = splitrule, 
                      fill = splitrule)) + 
            facet_grid(. ~ node_lab, 
                       space = 'free', 
                       scales = 'free'))
    
    if(show_sd) {
      
      plots <- 
        list(x = plots, 
             y = c('RMSE', 'Rsquared'), 
             z = c('RMSESD', 'RsquaredSD'), 
             v = c("RMSE, SD", 
                   "R\u00B2, SD")) %>% 
        pmap(function(x, y, z) x + 
               geom_ribbon(aes(ymin = .data[[y]] - .data[[z]], 
                               ymax = .data[[y]] + .data[[z]]), 
                           alpha = 0.3, 
                           color = NA) + 
               labs(y = v))
      
    } else {
      
      plots <- 
        map2(plots, 
             c("RMSE", 
               "R\u00B2"), 
             ~.x + 
               labs(y = .y))
      
    }
    
    map2(plots, 
         paste(plot_title, c('fit error', 'explanatory value')), 
         ~.x + 
           geom_line() + 
           labs(title = .y, 
                subtitle = plot_subtitle, 
                tag = plot_tag) + 
           globals$common_theme) %>% 
      set_names(c('rmse', 'rsq'))
    
  }
  
# Binary model performance and variable importance ------
  
  bin_model_squares <- function(predx_object) {
    
    ## computes square errors for a binary model
    
    data <- predx_object$data
    
    data <- data %>% 
      mutate(.outcome_numeric = as.numeric(.outcome) - 1, 
             square_error = (.data[[levels(data[['.outcome']])[2]]] - .outcome_numeric)^2)
    
    ## Brier score: for CV, BS is computed in a resample-manner
    
    if('.resample' %in% names(data)) {
      
      bs <- data %>% 
        summarise(BS = mean(square_error), 
                  .by = .resample)
      
      bs <- 
        tibble(statistic = 'BS', 
               estimate = mean(bs$BS), 
               lower_ci = quantile(bs$BS, probs = 0.025), 
               upper_ci = quantile(bs$BS, probs = 0.975))
      
    } else {
      
      bs <- tibble(statistic = 'BS', 
                   estimate = mean(data$square_error), 
                   lower_ci = quantile(data$square_error, 0.025), 
                   upper_ci = quantile(data$square_error, 0.975))
      
    }
    
    list(square_errors = data, 
         stats = bs)
    
  }
  
  reg_model_squares <- function(predx_object, normalize = TRUE) {
    
    ## squared errors
    
    if(normalize) {
      
      data <- predx_object$data %>% 
        mutate(square_error = scale(.outcome - .fitted)[, 1]^2)
      
    } else {
      
      data <- predx_object$data %>% 
        mutate(square_error = (.outcome - .fitted)^2)
      
    }
    
    ## summary stats: MSE and RMSE
    ## in CV, means and CI are computed in a fold-wise manner
    
    if(normalize) {
      
      stat_names <- paste0(c('MSE', 'RMSE'), 
                           '_normalized')
      
    } else {
      
      stat_names <- c('MSE', 'RMSE')
      
    }
    
    if('.resample' %in% names(data)) {
      
      stats <- data %>% 
        summarise(MSE = mean(square_error), 
                  RMSE = sqrt(mean(square_error)), 
                  .by = .resample)
      
      stats <- 
        tibble(statistic = stat_names, 
               estimate = c(mean(stats$MSE), 
                            mean(stats$RMSE)), 
               lower_ci = map_dbl(stats[c('MSE', 'RMSE')], 
                                  quantile, 
                                  probs = 0.025), 
               upper_ci = map_dbl(stats[c('MSE', 'RMSE')], 
                                  quantile, 
                                  probs = 0.975))
    
    } else {
      
      stats <- 
        tibble(statistic = stat_names, 
               estimate = c(mean(data$square_error), 
                            sqrt(mean(data$square_error))), 
               lower_ci = c(NA, NA), 
               upper_ci = c(NA, NA))
      
    }
    
    list(square_errors = data, 
         stats = stats)
    
  }
  
  plot_bin_error <- function(train_errors, 
                             cv_errors, 
                             plot_title = NULL, 
                             plot_subtitle = 'Square error for observations', 
                             x_lab = if(sort) 'Fraction of observations' else 'Observation number', 
                             y_lab = 'Square error', 
                             sort = FALSE, ...) {
    
    ## plots sorted or unsorted square errors
    
    data <- list(training = train_errors, 
                 CV = cv_errors)
    
    if(sort) {
      
      data <- data %>% 
        map(arrange, square_error) %>% 
        map(~mutate(.x, 
                    .observation = (1:nrow(.x))/nrow(.x)))
      
    }
    
    data <- data %>% 
      compress(names_to = 'model_type')
    
    scatter_plot(data, 
                 x_var = '.observation', 
                 y_var = 'square_error', 
                 fill_var = 'model_type', 
                 plot_title = plot_title, 
                 plot_subtitle = plot_subtitle, 
                 x_lab = x_lab, 
                 y_lab = y_lab, ...) + 
      scale_fill_manual(values = c(training = 'steelblue', 
                                   CV = 'coral3'), 
                        name = '') + 
      scale_color_manual(values = c(training = 'steelblue', 
                                    CV = 'coral3'), 
                         name = '')
    
  }
  
  plot_binary_stats <- function(data, 
                                plot_subtitle = 'Training', ...) {
    
    ## generates the following scatter plots:
    ## 1) Se vs Sp
    ## 2) 1-BS vs C-index
    ## 3) Kappa vs Accuracy 
    ## 4) Kappa vs AUC 
    
    ## plotting data
    
    plot_vars <- 
      list(Se_Sp = c('Sp', 'Se'), 
           BS_C = c('c_index', 'BS'), 
           kappa_accuracy = c('correct_rate', 'kappa'), 
           kappa_auc = c('AUC', 'kappa'))

    data <- plot_vars %>% 
      map(~filter(data, 
                  statistic %in% .x))
    
    data <- data %>% 
      map(pivot_wider, 
          id_cols = c('model', 'model_name', 'model_type'), 
          names_from = 'statistic', 
          values_from = 'estimate')
    
    data[['BS_C']] <- data[['BS_C']] %>% 
      mutate(BS = 1 - BS)
    
    ## plots
    
    x_var <- c('Sp', 'c_index', 'correct_rate', 'AUC')
    y_var <- c('Se', 'BS', 'kappa', 'kappa')
    
    plot_title <- c('Sensitivity and specificity', 
                    'Model error and concordance', 
                    'Reliability and accuracy', 
                    'Reliability and accuracy')
    
    x_lab <- c('Sp', 'C-index', 'Accuracy', 'AUC')
    y_lab <- c('Se', '1 - Brier Score', '\u03BA', '\u03BA')
    
    list(data = data, 
         x_var = x_var, 
         y_var = y_var, 
         plot_title = plot_title, 
         x_lab = x_lab, 
         y_lab = y_lab) %>% 
      pmap(scatter_plot, 
           plot_subtitle = plot_subtitle, ...) %>% 
      map(~.x + 
            geom_text_repel(aes(label = model_name), 
                            size = 2.75, 
                            show.legend = FALSE))
    
  }
  
  plot_regression_stats <- function(data, 
                                    plot_subtitle = 'Training', ...) {
    
    ## generates the following scatter plots:
    ## 1) Normalized RMSE vs R-squared
    ## 2) Pearson's r vs R-squared
    ## 3) Spearman's rho vs R-squared
    ## 4) Kendall's tau vs R-squared
    
    ## plotting data
    
    plot_vars <- 
      list(RMSE_Rsq = c('RMSE_normalized', 'rsq'), 
           r_rsq = c('pearson', 'rsq'), 
           rho_rsq = c('spearman', 'rsq'), 
           tau_rsq = c('kendall', 'rsq'))
    
    data <- plot_vars %>% 
      map(~filter(data, 
                  statistic %in% .x))
    
    data <- data %>% 
      map(pivot_wider, 
          id_cols = c('model', 'model_name', 'model_type'), 
          names_from = 'statistic', 
          values_from = 'estimate')
    
    ## plots
    
    y_var <- c('RMSE_normalized', 'pearson', 'spearman', 'kendall')
    
    plot_title <- c('Model error and explanatory value', 
                    rep('Model calibration and explanatory value', 3))
    
    y_lab <- c('Normalized RMSE', "Pearson's r", 
               "Spearman's \u03C1", "Kendall's \u03C4")
    
    list(data = data, 
         x_var = 'rsq', 
         y_var = y_var, 
         plot_title = plot_title, 
         x_lab = 'R\u00B2', 
         y_lab = y_lab) %>% 
      pmap(scatter_plot, 
           plot_subtitle = plot_subtitle, ...) %>% 
      map(~.x + 
            geom_text_repel(aes(label = model_name), 
                            size = 2.75, 
                            show.legend = FALSE))
    
  }
  
  make_scatter_caps <- function(data) {
    
    ## generates captions for the scatter plots
    ## of the fitted and outcome values of a regression model
    ## will contain: R-squared, Pearson's r and number of complete observations
    
    data <- data %>% 
      filter(statistic %in% c('rsq', 'pearson')) %>% 
      mutate(statistic = factor(statistic, c('rsq', 'pearson'))) %>% 
      arrange(statistic)
    
    paste0("R\u00B2 = ", signif(data$estimate[1], 2), 
           ", Pearson's r = ", signif(data$estimate[2], 2))
    
  }
  
  plot_importance <- function(data, 
                              top_n = 100, 
                              fill_color = c(significant = 'steelblue', 
                                             ns = 'gray60'), 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              x_lab = 'Importance', 
                              adj_method = 'BH', 
                              significant_only = FALSE) {
    
    ## plots variable importance stats and p values obtained by Altmann's test
    
    ## plotting data
    
    data <- data %>% 
      mutate(p_adjusted = p.adjust(pvalue, method = adj_method), 
             significant = ifelse(p_adjusted < 0.05, 'significant', 'ns'), 
             significant = factor(significant, c('ns', 'significant')))
    
    if(significant_only) {
      
      data <- data %>% 
        filter(significant == 'significant')
      
    }
    
    data <- data %>% 
      top_n(n = top_n, importance)
    
    ## plot
    
    data %>% 
      ggplot(aes(x = importance, 
                 y = reorder(variable, importance), 
                 fill = significant)) + 
      geom_bar(stat = 'identity', 
               color = 'black') +
      scale_fill_manual(values = fill_color, 
                        name = 'Altmann permutation test') + 
      globals$common_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = x_lab)
    
  }
  
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