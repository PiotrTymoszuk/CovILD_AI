# Report tables 

  insert_head()
  
# container -----
  
  tables <- list()
  
# Table 1: study variables --------
  
  insert_msg('Table 1: Study variables')
  
  tables$study_variables <- 
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
  
  tables$study_variables <- tables$study_variables %>% 
    mdtable(label = 'table_1_study_variables', 
            ref_name = 'study_variables', 
            caption = 'Study variables')
  
# Table 2: characteristic of the cohort ------
  
  insert_msg('Table 2: characteristic of the cohort')
  
  tables$cohort <- cohort$result_tbl %>% 
    filter(variable != 'COVID-19 severity') %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_2_cohort_characteristic', 
            ref_name = 'cohort', 
            caption = paste('Baseline characteristics and COVID-19 course', 
                            'in the CovILD cohort.', 
                            'Numeric variables are presented as medians with', 
                            'interquartile ranges (IQR) and ranges.', 
                            'Categorical variables are', 
                            'presented as percentages', 
                            'and counts within the complete observation set.'))
  
# Tables 3 - 5: CT, LFT and symptom variables at follow-ups -------
  
  insert_msg('Tables 3 - 5: CT, LFT and symptom variables at follow-ups')
  
  tables[c("cohort_ct", "cohort_lft", "cohort_symptoms")] <- 
    list(cohort_ct, cohort_lft, cohort_sympt) %>% 
    map(~.x$result_tbl) %>% 
    map(compress, names_to = 'Severity subset') %>% 
    map(relocate, `Severity subset`) %>% 
    map(mutate, 
        `Severity subset` = globals$sev_labels[`Severity subset`])
  
  tables[c("cohort_ct", "cohort_lft", "cohort_symptoms")] <- 
    tables[c("cohort_ct", "cohort_lft", "cohort_symptoms")] %>% 
    list(x = ., 
         label = c('table_3_cohort_ct_variables', 
                    'table_4_cohort_lft_variables', 
                    'table_5_cohort_symptom_variables'), 
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
  
# Table 6: optimal cutoffs for the AI opacity and AI high opacity ------
  
  insert_msg('Table 6: cutoff for opacity and high opacity')
  
  tables$ai_cutoffs <- list(cut_opacity, cut_high) %>% 
    map(~.x$result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'AI marker') %>% 
    relocate(`AI marker`) %>% 
    mdtable(label = 'table_6_ai_cutoffs', 
            ref_name = 'ai_cutoffs', 
            caption = paste('Optimal cutoffs for AI-determined', 
                            'lung opacity and high opacity at detection', 
                            'of chest computed tomography abnormalities.', 
                            'The optimal cutoff was determined', 
                            'by Youden criterion.'))
  
# Table 7: performance of AI parameters, severity classes ------
  
  insert_msg('Table 7: AI cutoff performance, severity classes')
  
  tables$ai_cutoffs_severity <- 
    list(op_strata, high_strata) %>% 
    map(~.x$severity_result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'ai_marker') %>% 
    select(ai_marker, 
           response, 
           severity_class, 
           kappa, Se, Sp, accuracy) %>% 
    set_names(c('AI marker', 'CT abnormality', 
                'COVID-19 severity', '\u03BA', 
                'Sensitivity', 'Specificity', 'Accuracy')) %>% 
    mdtable(label = 'table_7_ai_cutoffs_severity', 
            ref_name = 'ai_cutoffs_severity', 
            caption = paste('Performance of AI-determined lung', 
                            'opacity and high opacity at detection of', 
                            'chest computed tomography abnormalities', 
                            'in the COVID-19 severity subsets.', 
                            'The opacity and high opacity values', 
                            'were stratified by their optimal cutoffs', 
                            'determined by Youden criterion in the entire', 
                            'CovILD cohort.', 
                            'Statistic values are presented', 
                            'with 95% confidence intervals obtained', 
                            'by bootstrap.'))
  
# Table 8: performance of AI parameters, follow-ups -------
  
  insert_msg('Table 8: AI cutoff performance, follow-ups')
  
  tables$ai_cutoffs_fup <- 
    list(op_strata, high_strata) %>% 
    map(~.x$follow_up_result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'ai_marker') %>% 
    select(ai_marker, 
           response, 
           follow_up, 
           kappa, Se, Sp, accuracy) %>% 
    set_names(c('AI marker', 'CT abnormality', 
                'Follow-up', '\u03BA', 
                'Sensitivity', 'Specificity', 'Accuracy')) %>% 
    mdtable(label = 'table_8_ai_cutoffs_follow_up', 
            ref_name = 'ai_cutoffs_fup', 
            caption = paste('Performance of AI-determined lung', 
                            'opacity and high opacity at detection of', 
                            'chest computed tomography abnormalities', 
                            'at the consecutive follow-ups.', 
                            'The opacity and high opacity values', 
                            'were stratified by their optimal cutoffs', 
                            'determined by Youden criterion in the entire', 
                            'CovILD cohort.',
                            'Statistic values are presented', 
                            'with 95% confidence intervals obtained', 
                            'by bootstrap.'))
  
# Table 9: detection of ILD by CTSS --------
  
  insert_msg('Table 9: detection of ILD')
  
  tables$ild_cutoff <- ild_cutoff$result_tbl %>% 
    mdtable(label = 'table_9_ild_cutoff', 
            ref_name = 'ild_cutoff', 
            caption = paste('Detecion of interstitial lung disease', 
                            'defined by lung opacity > 5% by chest computed', 
                            'tomography severity score (CTSS).', 
                            'The optimal cutoff for CTSS was determined', 
                            'by Youden criterion.'))
  
# Tables 10 - 11: performance of CTSS, severity classes and follow-ups -------
  
  insert_msg('Tables 10 - 11: CTSS performance, severity classes and follwo-ups')
  
  tables[c('ild_cutoff_severity', 
           'ild_cutoff_fup')] <- 
    map2(ild_strata[c("severity_result_tbl", "follow_up_result_tbl")], 
         list(c('COVID-19 severity', '\u03BA', 
                'Sensitivity', 'Specificity', 
                'Accuracy'), 
              c('Follow-up', '\u03BA', 
                'Sensitivity', 'Specificity', 
                'Accuracy')), 
         set_names)
  
  tables[c('ild_cutoff_severity', 
           'ild_cutoff_fup')] <- tables[c('ild_cutoff_severity', 
                                          'ild_cutoff_fup')] %>% 
    list(x = ., 
         label = c('table_10_ild_cutoff_severity', 
                   'table_11_ild_cutoff_severity'), 
         ref_name = names(.), 
         caption = c(paste('Detecion of interstitial lung disease', 
                           'defined by lung opacity > 5% by chest computed', 
                           'tomography severity score (CTSS)',
                           'in the COVID-19 severity subsets.', 
                           'The optimal cutoff for CTSS was determined', 
                           'by Youden criterion in the entire', 
                           'CovILD cohort.', 
                           'Statistic values are presented', 
                           'with 95% confidence intervals obtained', 
                           'by bootstrap.'), 
                     paste('Detecion of interstitial lung disease', 
                           'defined by lung opacity > 5% by chest computed', 
                           'tomography severity score (CTSS)',
                           'at the consecutive follow-ups.', 
                           'The optimal cutoff for CTSS was determined', 
                           'by Youden criterion in the entire', 
                           'CovILD cohort.', 
                           'Statistic values are presented', 
                           'with 95% confidence intervals obtained', 
                           'by bootstrap.'))) %>% 
    pmap(mdtable)


# Table 12: machine learning tuning parameters -----  
  
  insert_msg('Table 12: machine learining, tuning')
  
  tables$tuning <- c(bin_models$models, 
                     reg_models$models) %>% 
    map(map, ~.x$bestTune) %>% 
    map(map, ~map2_chr(names(.x), .x, paste, sep = ' = ')) %>% 
    map(map_chr, paste, collapse = '\n') %>% 
    map(compress, names_to = 'algorithm', values_to = 'parameter') %>% 
    compress(names_to = 'response') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'SVM radial', 
                                    replacement = 'Support vector machines, radial kernel'), 
           algorithm = stri_replace(algorithm, 
                                    fixed = 'GBM', 
                                    replacement = 'Gradiant boosted machines')) %>% 
    select(response, algorithm, parameter) %>% 
    set_names(c('Response', 'Algorithm', 'Parameters')) %>% 
    mdtable(label = 'table_12_ml_tuning',
            ref_name = 'tuning', 
            caption = paste('Selection of machine learning algorithm', 
                            'parameters by cross-validation-mediated tuning.'))
  
# Table 13: performance of binary classifiers -------
  
  insert_msg('Table 13: performance of the binary classifiers')
  
  tables$bin_classifiers <- bin_models$stats %>% 
    compress(names_to = 'response') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           response = factor(response, 
                             exchange(names(bin_models$stats), 
                                      lft_globals$lexicon)), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = factor(algorithm, globals$algo_labs), 
           dataset = globals$dataset_lab[dataset]) %>% 
    select(response, algorithm, dataset,  
           correct_rate, kappa, brier_score, 
           AUC, Se, Sp, J) %>% 
    arrange(response, algorithm) %>% 
    set_names(c('Response', 'Algorithm', 'Data set', 
                'Overall accuracy', "\u03BA", 
                'Brier score', 'AUC', 
                'Sensitivity', 'Specificity', "J")) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>%
    mdtable(label = 'table_13_binary_classifier_performance', 
            ref_name = 'bin_classifiers', 
            caption = paste('Performance of binary machine learining', 
                            'classifiers at predicting lung function testing', 
                            '(LFT) abnormalities in the CovILD study', 
                            'participants.'))
  
# Table 14: performance of regression models -----
  
  insert_msg('Table 14: performance of the regression models')
  
  tables$reg_models <- reg_models$stats %>% 
    compress(names_to = 'response') %>% 
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
    mdtable(label = 'table_14_regression_model_performance', 
            ref_name = 'reg_models', 
            caption = paste('Performance of regression machine', 
                            'learning models at predicting lung function', 
                            'testing parameters in the CovILD study', 
                            'participants.'))

# Table 15: AI and CTSS differences for LFT abnormalities -------
  
  insert_msg('Table 15: CT opacity and CTSS in LFT abnormalities')
  
  tables$lft_binary_uni <- 
    list(LFT_findings = lft_uni, 
         DLCO_reduced = dlco_red_uni, 
         FVC_reduced = fvc_red_uni, 
         FEV1_reduced = fev1_red_uni) %>% 
    map(~.x$result_tbl) %>% 
    map(filter, 
        variable %in% c('Observations, n', 
                        'CTSS, points', 
                        'opactity, AI, % of lung', 
                        'high opacity, AI, % of lung')) %>% 
    map(select, -test) %>% 
    map(set_names, 
        c('Variable', 'Absent', 'Present', 
          'Significance', 'Effect size')) %>% 
    compress(names_to = 'LFT abnormality') %>% 
    relocate(`LFT abnormality`) %>% 
    mutate(`LFT abnormality` = exchange(`LFT abnormality`, 
                                        lft_globals$lexicon))
  
  tables$lft_binary_uni <- tables$lft_binary_uni %>% 
    mdtable(label = 'table_15_lft_univariate_binary', 
            ref_name = 'lft_binary_uni', 
            caption = paste('Differences in chest computed tomography', 
                            'severity score (CTSS), and AI-determined', 
                            'lung opacity and high opacity', 
                            'in CovILD study participants with and without', 
                            'lung function testing (LFT) abnormalities.', 
                            'Numeric variariables are presented as medians', 
                            'with interquartile ranges (IQR) and ranges.'))
  
# Table 16: correlation of opacity and CTSS with LFT variables ------
  
  insert_msg('Table 16: correlation of CTSS and opacity with LFT variables')
  
  tables$lft_correlation_uni <- 
    list(DLCO_percent = dlco_uni, 
         FVC_percent = fvc_uni, 
         FEV1_percent = fev1_uni) %>% 
    map(~.x$correlation$test) %>% 
    map_dfr(filter, 
            variable1 %in% c('CTSS', 
                             'opacity_percent', 
                             'high_opacity_percent')) %>%
    mutate(variable1 = exchange(variable1, lft_globals$lexicon), 
           variable2 = exchange(variable2, lft_globals$lexicon)) %>% 
    select(variable2, 
           variable1,
           n, 
           eff_size, 
           significance) %>% 
    set_names(c('LFT variable', 
                'CT variable', 
                'N', 
                'Correlation coefficient', 
                'Significance'))
  
  tables$lft_correlation_uni <- tables$lft_correlation_uni %>% 
    mdtable(label = 'table_16_lft_ct_correlation', 
            ref_name = 'lft_correlation_uni', 
            caption = paste('Correlation of LFT variables with', 
                            'chest computed tomography severity score, and', 
                            'AI-determined opacity and high opactity.'))
    
# Table 17: ROC for LFT abnormalities ------
  
  insert_msg('Table 17: ROC for LFT abnormalities')
  
  tables$lft_roc <- lft_roc$result_tbl %>% 
    mdtable(label = 'table_16_lft_roc', 
            ref_name = 'lft_roc', 
            caption = paste('Detection of lung function testing (LFT)', 
                            'abnormalities by chest computed tomography', 
                            'variables in receiver-operator characteristic', 
                            '(ROC) analysis.'))
  
  
# Table 18: bootstrapped inter-rater reliability and ROC stats for CT markers ------
  
  insert_msg('Table 18: bootstrapped LFT ROC and inter-rater stats')
  
  tables$lft_interrater <- lft_roc$bootstrap_result_tbl %>% 
    set_names(c('LFT response', 'CT marker', 
                '\u03BA', 'Sensitivity', 'Specificity')) %>% 
    mdtable(label = 'table_18_lft_interrater_bootstrap_stats',
            ref_name = 'lft_interrater',
            caption = paste('Inter-rater reliability and', 
                            'receiver operator characteristic metrics for', 
                            'detection of lung function testing abnormalities', 
                            'with lung computed tomography', 
                            'severity markers.', 
                            'Statistic values are presented', 
                            'with 95% confidence intervals obtained', 
                            'by bootstrap.'))
  
# Saving tables on the disc ------
  
  insert_msg('Saving tables on the disc')
  
  tables$cover <- tables %>% 
    map_chr(attr, 'caption') %>% 
    tibble(Table = paste('Table', 1: length(tables)), 
           Caption = .)
  
  tables <- tables[c('cover', names(tables)[names(tables) != 'cover'])]
  
  tables %>% 
    set_names(c('Cover', paste('Table', 1:(length(tables) - 1)))) %>% 
    write_xlsx(path = './report/tables.xlsx')
  
# END ------
  
  insert_tail()
    