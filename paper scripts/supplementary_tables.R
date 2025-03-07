# Supplementary tables

  insert_head()
  
# container ------
  
  suppl_tabs <- list()
  
# Numbers of samples and participants per time point -------
  
  insert_msg('Numbers of samples and participants')

  suppl_tabs$n_numbers <- cohort_ct$analysis_tbl %>% 
    blast(cohort_split) %>% 
    map(count, follow_up) %>% 
    reduce(left_join, by = 'follow_up') %>% 
    mutate(follow_up = stri_extract(follow_up, regex = '\\d+-month'), 
           follow_up = ifelse(is.na(follow_up), 
                              'all time points', 
                              follow_up)) %>% 
    set_names(c('Follow-up, months', 
                'Cohort', 
                'Ambulatory, mild COVID-19', 
                'Hospitalized, moderate COVID-19', 
                'Hospitalized, severe COVID-19')) %>%
    mdtable(label = 'observation_n_numbers', 
            ref_name = 'n_numbers', 
            caption = paste('Numbers of observations included in the analysis', 
                            'split by acute COVID-19 severity and the follow-up', 
                            'examination.'))

# Study variables --------
  
  insert_msg('Study variables')
  
  suppl_tabs$study_variables <- 
    list(lft = tibble(variable = lft_globals$responses), 
         ct = tibble(variable = lft_globals$ct_variables), 
         symptoms = tibble(variable = lft_globals$symptom_variables), 
         baseline = tibble(variable = lft_globals$baseline_variables)) %>% 
    compress(names_to = 'category') %>% 
    mutate(modeling = ifelse(category == 'lft', 
                             'response variable', 
                             'explanatory variable'), 
           category = car::recode(category, 
                                  "'lft' = 'Lung function testing'; 
                                   'ct' = 'Chest computed tomography'; 
                                   'symptoms' = 'Persistent symptoms';
                                   'baseline' = 'Baseline characteristic and acute COVID-19'")) %>% 
    left_join(lft_globals$lexicon[c('variable', 'label', 'format', 
                                    'unit', 'description')] %>% 
                filter(!duplicated(variable)), 
              by = 'variable') %>% 
    select(modeling, 
           category, 
           variable, 
           label, 
           description, 
           format, 
           unit) %>% 
    set_names(c('Use in modeling', 
                'Category', 
                'Name in R', 
                'Name in the report', 
                'Description', 
                'Format', 
                'Unit'))
  
  suppl_tabs$study_variables <- suppl_tabs$study_variables %>% 
    mdtable(label = 'study_variables', 
            ref_name = 'study_variables', 
            caption = paste('Study variables. The table is available in a', 
                            'supplementary Excel file.'))
  
# CT, LFT and symptom variables at follow-ups -------
  
  insert_msg('CT, LFT and symptom variables at follow-ups')
  
  suppl_tabs[c("cohort_ct", 
               "cohort_lft", 
               "cohort_symptoms")] <- 
    list(cohort_ct, cohort_lft, cohort_sympt) %>% 
    map(~.x$result_tbl) %>% 
    map(compress, names_to = 'Severity subset') %>% 
    map(relocate, `Severity subset`) %>% 
    map(mutate, 
        `Severity subset` = globals$sev_labels[`Severity subset`])
  
  ## removal of crazy paving from the CT data frame: just few individuals
  ## were affected
  
  suppl_tabs$cohort_ct <- suppl_tabs$cohort_ct %>% 
    filter(Variable != 'crazy paving')
  
  ## restricting the LFT outcomes to the major ones
  
  suppl_tabs$cohort_lft <- suppl_tabs$cohort_lft %>% 
    filter(Variable %in% c('FVC, % of reference', 
                           'FVC < 80%', 
                           'FEV1, % of reference', 
                           'FEV1 < 80%', 
                           'DLCO, % of reference', 
                           'DLCO < 80%'))
  
  suppl_tabs[c("cohort_ct", 
               "cohort_lft", 
               "cohort_symptoms")] <- 
    suppl_tabs[c("cohort_ct", 
                 "cohort_lft", 
                 "cohort_symptoms")] %>% 
    list(x = ., 
         label = c('cohort_ct_variables', 
                   'cohort_lft_variables', 
                   'cohort_symptom_variables'), 
         ref_name = names(.), 
         caption = c(paste('Chest computed tomography variables', 
                           'at consecutive follow-ups.', 
                           'Numeric variables are presented as medians with', 
                           'interquartile ranges (IQR) and ranges.', 
                           'Categorical variables are', 
                           'presented as percentages', 
                           'and counts within the complete observation set.'), 
                     paste('Lung function testing variables', 
                           'at consecutive follow-ups.', 
                           'Numeric variables are presented as medians with', 
                           'interquartile ranges (IQR) and ranges.', 
                           'Categorical variables are', 
                           'presented as percentages', 
                           'and counts within the complete observation set.'), 
                     paste('Presence and rating of symptoms of relevance for',
                           'lung function at consecutive follow-ups.', 
                           'Numeric variables are presented as medians with', 
                           'interquartile ranges (IQR) and ranges.', 
                           'Categorical variables are', 
                           'presented as percentages', 
                           'and counts within the complete observation set.'))) %>% 
    pmap(mdtable)
  
# Tuning of the machine learning models ---------
  
  insert_msg('tuning of the machine learning models')
  
  suppl_tabs$tuning <- c(bin_models$models, 
                         reg_models$models) %>% 
    map(map, ~.x$bestTune) %>% 
    map(map, map_dfc, function(x) if(is.numeric(x)) signif(x, 3) else x) %>% 
    map(map, ~map2_chr(names(.x), .x, paste, sep = ' = ')) %>% 
    map(map_chr, paste, collapse = '\n') %>% 
    map(compress, names_to = 'algorithm', values_to = 'parameter') %>% 
    compress(names_to = 'response') %>% 
    filter(response != 'LFT_findings') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'SVM radial', 
                                    replacement = 'Support vector machines, radial kernel'), 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'GBM', 
                                    replacement = 'Gradient boosted machines')) %>% 
    select(response, algorithm, parameter) %>% 
    set_names(c('Response', 'Algorithm', 'Parameters')) %>% 
    mdtable(label = 'ml_tuning',
            ref_name = 'tuning', 
            caption = paste('Selection of machine learning algorithm', 
                            'hyper-parameters by cross-validation-mediated tuning.'))
  
# Performance of the machine learning classifiers, training ------
  
  insert_msg('Performance of machine learning classifiers, training')
  
  suppl_tabs$bin_classifiers <- bin_models$stats %>% 
    compress(names_to = 'response') %>% 
    filter(dataset == 'train', 
           response != 'LFT_findings') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           response = factor(response, 
                             exchange(names(bin_models$stats), 
                                      lft_globals$lexicon)), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = factor(algorithm, globals$algo_labs)) %>% 
    select(response, algorithm,  
           correct_rate, kappa, brier_score, 
           AUC, Se, Sp) %>% 
    arrange(response, algorithm) %>% 
    set_names(c('Response', 'Algorithm', 
                'Overall accuracy', "\u03BA", 
                'Brier score', 'AUC', 
                'Sensitivity', 'Specificity')) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>%
    mdtable(label = 'binary_classifier_performance', 
            ref_name = 'bin_classifiers', 
            caption = paste('Performance', 
                            'of binary machine learning', 
                            'classifiers at predicting lung function testing', 
                            '(LFT) abnormalities in the entire data set.'))
  
# Train data performance of the regressors ---------
  
  insert_msg('Train data performance of the regressors')
  
  suppl_tabs$reg_models <- reg_models$stats %>% 
    compress(names_to = 'response') %>% 
    filter(dataset == 'train') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           response = factor(response, 
                             exchange(names(reg_models$stats), 
                                      lft_globals$lexicon)), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = factor(algorithm, globals$algo_labs), 
           dataset = globals$dataset_lab[dataset]) %>% 
    select(response, algorithm, dataset, 
           rsq, MAE, spearman) %>% 
    arrange(response, algorithm) %>% 
    set_names(c('Response', 'Algorithm', 'Data set', 
                'pseudo-R\u00B2', 'MAE', 
                "\u03C1")) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    mdtable(label = 'regression_model_performance', 
            ref_name = 'reg_models', 
            caption = paste('Performance of regression machine', 
                            'learning models at predicting values', 
                            'of lung function testing parameters', 
                            'in the entire data set.'))
  
# SHAP importance for DLCO -------
  
  insert_msg('SHAP importance for DLCO')
  
  suppl_tabs$mean_shap <- shap_imp$mean_table %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 3) else x) %>% 
    mdtable(label = 'variable_importance_shap', 
            ref_name = 'mean_shap', 
            caption = paste('Mean values of SHAP', 
                            '(Shapley additive explanations) variable', 
                            'importance statistic for the models of reduced', 
                            'diffusion capacity for carbon monoxide', 
                            'and of diffusion capacity for carboon monoxide', 
                            '< 80% of reference.', 
                            'The table is available as a supplementary Excel', 
                            'file.'))
  
# Correlations between CT readouts of lung damage --------
  
  insert_msg('Correlations between CT readouts of lung damage')
  
  suppl_tabs$ct_correlations <- corr_lft$test$ct %>% 
    transmute(`Variable 1` = exchange(variable1, covild$ct_lexicon), 
              `Variable 2` = exchange(variable2, covild$ct_lexicon), 
              N = n, 
              `Correlation coefficient` = est_lab,
              Significance = significance) %>% 
    mdtable(label = 'ct_correlations', 
            ref_name = 'ct_correlations', 
            caption = paste('Correlations between chest computed tomography', 
                            'severity score, and AI-determined opacity and', 
                            'high opacity.'))
  
# CT readouts in observations with and without GGO and reticulations --------
  
  insert_msg('CT readouts, samples with and without GGO and reticulations')
  
  suppl_tabs$ct_comparisons <- comp_ct$result_tbl %>% 
    filter(`CT abnormality` %in% c('GGO', 'reticulation')) %>% 
    mdtable(label = 'ct_comparisons', 
            ref_name = 'ct_comparisons', 
            caption = paste('Differences in chest computed tomography', 
                            'severity score (CTSS), and AI-determined', 
                            'lung opacity and high opacity', 
                            'in CovILD study participants with and without', 
                            'ground glass opacities (GGO) and reticulation.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR) and ranges.'))
  
# AI and CTSS differences for LFT abnormalities -------
  
  insert_msg('CT opacity and CTSS in LFT abnormalities')
  
  suppl_tabs$lft_binary_uni <- lft_uniboot$binary_result_tbl %>%
    filter(`LFT abnormality` != 'any LFT findings') %>% 
    mdtable(label = 'lft_univariate_binary', 
            ref_name = 'lft_binary_uni', 
            caption = paste('Differences in chest computed tomography', 
                            'severity score (CTSS), and AI-determined', 
                            'lung opacity and high opacity', 
                            'in CovILD study participants with and without', 
                            'lung function testing (LFT) abnormalities.', 
                            'Numeric variables are presented as medians', 
                            'with interquartile ranges (IQR) and ranges.'))
  
# Correlation of opacity and CTSS with LFT variables ------
  
  insert_msg('Correlation of CTSS and opacity with LFT variables')

  suppl_tabs$lft_correlation_uni <- 
    lft_uniboot$cor_result_tbl %>% 
    mdtable(label = 'lft_ct_correlation', 
            ref_name = 'lft_correlation_uni', 
            caption = paste('Correlation of LFT variables with', 
                            'chest computed tomography severity score, and', 
                            'AI-determined opacity and high opacity.'))
  
# Saving the supplementary tables on the disc -------
  
  insert_msg('Saving the supplementary tables')
  
  suppl_tabs %>% 
    save_excel(path = './paper/supplementary tables.xlsx', 
               prefix = 'Supplementary Table S')
  
# END ------
  
  insert_tail()