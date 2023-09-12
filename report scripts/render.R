# Renders the report

  insert_head()
  
# Bibliography -----
  
  insert_msg('Reading the bibliography')
  
  tesca_bib <- read_bib('./report/markdown/ct.bib') %>% 
    as_mdbib
  
# Rendering the report ------
  
  insert_msg('Rendering the report')
  
  render('./report/markdown/report.Rmd', 
         output_format = robobook(template = 'default', 
                                  use_bookdown = TRUE, 
                                  number_sections = FALSE, 
                                  toc_depth = 3, 
                                  lightbox = TRUE, 
                                  thumbnails = TRUE, 
                                  gallery = TRUE, 
                                  css = 'styles.css'), 
           output_dir = './report')
  
# END ----
  
  insert_tail()