# Main paper_tabs

  insert_head()
  
# container ------
  
  paper_tabs <- list()
  
# Table 1: characteristic of the study cohort ------
  
  insert_msg('Table 1: characteristic of the study cohort')
  
  paper_tabs$cohort <- cohort$result_tbl %>% 
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
  
# Table 2: performance of the machine learning classifiers ------
  
  insert_msg('Table 2: performance of machine learning classifiers, CV')
  
  paper_tabs$bin_classifiers <- bin_models$stats %>% 
    compress(names_to = 'response') %>% 
    filter(dataset == 'cv', 
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
    mdtable(label = 'table_2_binary_classifier_performance', 
            ref_name = 'bin_classifiers', 
            caption = paste('Cross-validated performance', 
                            'of binary machine learining', 
                            'classifiers at predicting lung function testing', 
                            '(LFT) abnormalities.'))
  
# Table 3: CV performance of the regressors ---------
  
  insert_msg('Table 3: CV performance of the regressors')

  paper_tabs$reg_models <- reg_models$stats %>% 
    compress(names_to = 'response') %>% 
    filter(dataset == 'cv') %>% 
    mutate(response = exchange(response, lft_globals$lexicon), 
           response = factor(response, 
                             exchange(names(reg_models$stats), 
                                      lft_globals$lexicon)), 
           algorithm = globals$algo_labs[algorithm], 
           algorithm = factor(algorithm, globals$algo_labs)) %>% 
    select(response, algorithm, 
           rsq, MAE, spearman) %>% 
    arrange(response, algorithm) %>% 
    set_names(c('Response', 'Algorithm', 
                'pseudo-R\u00B2', 'MAE', 
                "\u03C1")) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 2) else x) %>% 
    mdtable(label = 'table_14_regression_model_performance', 
            ref_name = 'reg_models', 
            caption = paste('Cross-validated performance of regression machine', 
                            'learning models at predicting values', 
                            'of lung function testing parameters.'))
  
# Table 4: detection of reduced DLCO by single CT parameters -------
  
  insert_msg('Table 4: detection of DLCO abnormality by single CT parameters')

  paper_tabs$ct_roc <- lft_roc$summary_result_tbl %>% 
    filter(`LFT abnormality` == 'DLCO < 80%') %>% 
    select(- `LFT abnormality`) %>% 
    arrange(`CT variable`) %>% 
    mdtable(label = 'table_4_ct_variables_lft_dlco', 
            ref_name = 'ct_roc',
            caption = paste('Detection of reduced diffusion capacity for CO', 
                            'DLCO < 80% reference value) by single CT-derived', 
                            'parameters: AI-determined opacity and high opacity,', 
                            'and human-determined CT severity score.'))
  
# Saving the paper_tabs on the disc -------
  
  insert_msg('Saving the paper_tabs')
  
  paper_tabs$cover <- paper_tabs %>% 
    map_chr(attr, 'caption') %>% 
    tibble(Table = paste('Table', 1: length(paper_tabs)), 
           Caption = .)
  
  paper_tabs <- 
    paper_tabs[c('cover', names(paper_tabs)[names(paper_tabs) != 'cover'])]
  
  paper_tabs %>% 
    set_names(c('Cover', paste('Table', 1:(length(paper_tabs) - 1)))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  
# END -----
  
  insert_tail()