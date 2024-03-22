# Renders the manuscript and supplements

  insert_head()
  
# Bibliography -----
  
  insert_msg('Reading the bibliography')
  
  ct_bib <- read_bib('./paper/markdown/ct.bib') %>% 
    as_mdbib
  
# Rendering the paper ------
  
  insert_msg('Rendering the paper and supplements')
  
  render('./paper/markdown/manuscript.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
           output_dir = './paper')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper')
  
# END ----
  
  insert_tail()