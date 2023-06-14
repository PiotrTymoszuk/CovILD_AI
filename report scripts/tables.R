# Report tables 

  insert_head()
  
# container -----
  
  tables <- list()
  
# Table 1: characteristic of the cohort ------
  
  insert_msg('Table 1: characteristic of the cohort')
  
  tables$cohort <- cohort$result_tbl %>% 
    filter(variable != 'COVID-19 severity') %>% 
    set_names(c('Variable', 
                'Cohort', 
                'Ambulatory COVID-19', 
                'Moderate COVID-19', 
                'Severe COVID-19', 
                'Significance', 
                'Effect size')) %>% 
    mdtable(label = 'table_1_cohort_characteristic', 
            ref_name = 'cohort', 
            caption = paste('Baseline characteristics and COVID-19 course', 
                            'in the CovILD cohort.', 
                            'Numeric variables are presented as medians with', 
                            'interquartile ranges (IQR) and ranges.', 
                            'Categorical variables are', 
                            'presented as percentages', 
                            'and counts within the complete observation set.'))
  
# Table 2: CT variables at follow-ups ------
  
  insert_msg('Table 2: CT variables at follow-ups')
  
  tables$cohort_ct <- cohort_ct$result_tbl %>% 
    compress(names_to = 'Severity subset') %>% 
    relocate(`Severity subset`) %>% 
    mutate(`Severity subset` = globals$sev_labels[`Severity subset`])
  
  tables$cohort_ct <- tables$cohort_ct %>% 
    set_colnames(capitalize_first_char(names(tables$cohort_ct))) %>% 
    mdtable(label = 'table_2_cohort_ct_variables',
            ref_name = 'cohort_ct', 
            caption = paste('Chest computed tomography variables', 
                            'at consecutive follow-ups.', 
                            'Numeric variables are presented as medians with', 
                            'interquartile ranges (IQR) and ranges.', 
                            'Categorical variables are', 
                            'presented as percentages', 
                            'and counts within the complete observation set.'))
  
# Table 3: LFT variables at follow-ups ------
  
  insert_msg('Table 3: LFT variables at follow ups')
  
  tables$cohort_lft <- cohort_lft$result_tbl %>% 
    compress(names_to = 'Severity subset') %>% 
    relocate(`Severity subset`) %>% 
    mutate(`Severity subset` = globals$sev_labels[`Severity subset`])
  
  tables$cohort_lft <- tables$cohort_lft %>% 
    set_colnames(capitalize_first_char(names(tables$cohort_lft))) %>% 
    mdtable(label = 'table_3_cohort_lft_variables',
            ref_name = 'cohort_lft', 
            caption = paste('Lung function testing variables', 
                            'at consecutive follow-ups.', 
                            'Numeric variables are presented as medians with', 
                            'interquartile ranges (IQR) and ranges.', 
                            'Categorical variables are', 
                            'presented as percentages', 
                            'and counts within the complete observation set.'))
  
# Table 4: optimal cutoffs for the AI opacity and AI high opacity ------
  
  insert_msg('Table 4: cutoff for opacity and high opacity')
  
  tables$ai_cutoffs <- list(cut_opacity, cut_high) %>% 
    map(~.x$result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'AI marker') %>% 
    relocate(`AI marker`) %>% 
    mdtable(label = 'table_4_ai_cutoffs', 
            ref_name = 'ai_cutoffs', 
            caption = paste('Optimal cutoffs for AI-determined', 
                            'lung opacity and high opacity at detection', 
                            'of chest computed tomography abnormalities.', 
                            'The optimal cutoff was determined', 
                            'by Youden criterion.'))
  
# Table 5: performance of AI parameters, severity classes ------
  
  insert_msg('Table 5: AI cutoff performance, severity classes')
  
  tables$ai_cutoffs_severity <- 
    list(op_strata, high_strata) %>% 
    map(~.x$severity_result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'ai_marker') %>% 
    select(ai_marker, 
           severity_class, 
           response,  
           kappa, Se, Sp, accuracy) %>% 
    set_names(c('AI marker', 'COVID-19 severity', 
                'CT abnormality', '\u03BA', 
                'Sensitivity', 'Specificity', 'Accuracy')) %>% 
    mdtable(label = 'table_5_ai_cutoffs_severity', 
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
  
# Table 6: performance of AI parameters, follow-ups -------
  
  insert_msg('Table 6: AI cutoff performance, follow-ups')
  
  tables$ai_cutoffs_fup <- 
    list(op_strata, high_strata) %>% 
    map(~.x$follow_up_result_tbl) %>% 
    set_names(c('opacity', 'high opacity')) %>% 
    compress(names_to = 'ai_marker') %>% 
    select(ai_marker, 
           follow_up, 
           response, 
           kappa, Se, Sp, accuracy) %>% 
    set_names(c('AI marker', 'Follow-up', 
                'CT abnormality', '\u03BA', 
                'Sensitivity', 'Specificity', 'Accuracy')) %>% 
    mdtable(label = 'table_6_ai_cutoffs_follow_up', 
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
  
# Table 7: detection of ILD by CTSS --------
  
  insert_msg('Table 7: detection of ILD')
  
  tables$ild_cutoff <- ild_cutoff$result_tbl %>% 
    mdtable(label = 'table_7_ild_cutoff', 
            ref_name = 'ild_cutoff', 
            caption = paste('Detecion of interstitial lung disease', 
                            'defined by lung opacity > 5% by chest computed', 
                            'tomography severity score (CTSS).', 
                            'The optimal cutoff for CTSS was determined', 
                            'by Youden criterion.'))
  
# Table 8: performance of CTSS, severity classes -------
  
  insert_msg('Table 8: CTSS performance, severity classes')
  
  tables$ild_cutoff_severity <- ild_strata$severity_result_tbl %>% 
    set_names(c('COVID-19 severity', '\u03BA', 
                'Sensitivity', 'Specificity', 
                'Accuracy')) %>% 
    mdtable(label = 'table_8_ild_cutoff_severity', 
            ref_name = 'ild_cutoff_severity', 
            caption = paste('Detecion of interstitial lung disease', 
                            'defined by lung opacity > 5% by chest computed', 
                            'tomography severity score (CTSS)',
                            'in the COVID-19 severity subsets.', 
                            'The optimal cutoff for CTSS was determined', 
                            'by Youden criterion in the entire', 
                            'CovILD cohort.', 
                            'Statistic values are presented', 
                            'with 95% confidence intervals obtained', 
                            'by bootstrap.'))
  
# Table 9: performance of CTSS, follow-ups ------
  
  insert_msg('Table 9: performance of CTSS, follow-ups')
  
  tables$ild_cutoff_fup <- ild_strata$follow_up_result_tbl %>% 
    set_names(c('Follow-up', '\u03BA', 
                'Sensitivity', 'Specificity', 
                'Accuracy')) %>% 
    mdtable(label = 'table_9_ild_cutoff_severity', 
            ref_name = 'ild_cutoff_fup', 
            caption = paste('Detecion of interstitial lung disease', 
                            'defined by lung opacity > 5% by chest computed', 
                            'tomography severity score (CTSS)',
                            'at the consecutive follow-ups.', 
                            'The optimal cutoff for CTSS was determined', 
                            'by Youden criterion in the entire', 
                            'CovILD cohort.', 
                            'Statistic values are presented', 
                            'with 95% confidence intervals obtained', 
                            'by bootstrap.'))
  
# Table 10: Random forest, tuning parameters -------
  
  insert_msg('Table 10: Random Forest tuning paramaters')
  
  tables$rf_tuning <- 
    list(binary = bin_importance, 
         regression = reg_importance) %>% 
    map(~.x$optima) %>% 
    map2(., list(bin_importance$lexicon, 
                 reg_importance$lexicon), 
         ~mutate(.x, 
                 response = exchange(model, 
                                     dict = .y, 
                                     value = 'label_short'))) %>% 
    compress(names_to = 'model_type') %>% 
    mutate(participant_matching = stri_extract(model, 
                                               regex = 'unmatched|matched')) %>% 
    select(model_type, 
           response, 
           participant_matching, 
           mtry, 
           splitrule, 
           min.node.size) %>% 
    set_names(c('Model type', 
                'LFT variable', 
                'Participant matching', 
                'mtry', 
                'splitrule', 
                'min.node.size'))

  tables$rf_tuning <- tables$rf_tuning %>% 
    mdtable(label = 'table_10_rf_tuning', 
            ref_name = 'rf_tuning', 
            caption = paste('Optimal values of the mtry, splitrule', 
                            'and min.node.size paramaters for Random Forest', 
                            'models of lung function abnormalities', 
                            'and paramaters found by cross-validation tuning.'))
  
# Table 11: performance of the binary RF classifiers ------
  
  insert_msg('Table 11: performance of binary RF classifiers')
  
  tables$bin_models <- bin_models$result_tbl %>%
    set_names(c('LFT variable', 
                'Participant matching', 
                'Data type', 
                'Statistic name', 
                'Statistic value')) %>% 
    mdtable(label = 'table_11_random_forest_binary',
            ref_name = 'bin_models', 
            caption = paste('Performance of Random Forest classifiers', 
                            'at prediction of lung function testing (LFT)', 
                            'abnormalities in the training dataset', 
                            'and cross-validation (CV).', 
                            'CV results for any LFT abnormalities and diffusion', 
                            'capacity for CO (DLCO) < 80% reference', 
                            'are presented.', 
                            'The entire table is available in a supplementary', 
                            'Excel file.'))
  
# Table 12: performance of the regression RF models ------
  
  insert_msg('Table 12: performance of regression RF models')
  
  tables$reg_models <- reg_models$result_tbl %>%
    set_names(c('LFT variable', 
                'Participant matching', 
                'Data type', 
                'Statistic name', 
                'Statistic value')) %>% 
    mdtable(label = 'table_12_random_forest_regression',
            ref_name = 'reg_models', 
            caption = paste('Performance of Random Forest regression models', 
                            'at prediction of lung function testing (LFT)', 
                            'variables in the training dataset', 
                            'and cross-validation (CV).', 
                            'CV results for diffusion capacity for CO (DLCO),', 
                            'forced vital capacity (FVC) and', 
                            'forced expiratory volume in 1 second (FEV1)', 
                            'are presented.', 
                            'The entire table is available in a supplementary', 
                            'Excel file.'))
  
# Table 13: AI and CTSS differences for LFT abnormalities -------
  
  insert_msg('Table 13: CT opacity and CTSS in LFT abnormalities')
  
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
    mdtable(label = 'table_13_lft_univariate_binary', 
            ref_name = 'lft_binary_uni', 
            caption = paste('Differences in chest computed tomography', 
                            'severity score (CTSS), and AI-determined', 
                            'lung opacity and high opacity', 
                            'in CovILD study participants with and without', 
                            'lung function testing (LFT) abnormalities.', 
                            'Numeric variariables are presented as medians', 
                            'with interquartile ranges (IQR) and ranges.'))
  
# Table 14: correlation of opacity and CTSS with LFT variables ------
  
  insert_msg('Table 14: correlation of CTSS and opacity with LFT variables')
  
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
    mdtable(label = 'table_14_lft_ct_correlation', 
            ref_name = 'lft_correlation_uni', 
            caption = paste('Correlation of LFT variables with', 
                            'chest computed tomography severity score, and', 
                            'AI-determined opacity and high opactity.'))
    
# Table 15: ROC for LFT abnormalities ------
  
  insert_msg('Table 15: ROC for LFT abnormalities')
  
  tables$lft_roc <- lft_roc$result_tbl %>% 
    mdtable(label = 'table_14_lft_roc', 
            ref_name = 'lft_roc', 
            caption = paste('Detection of lung function testing (LFT)', 
                            'abnormalities by chest computed tomography', 
                            'variables in receiver-operator characteristic', 
                            '(ROC) analysis.'))
  
  
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
    