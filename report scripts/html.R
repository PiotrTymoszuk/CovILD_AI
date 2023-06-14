# Defining specific HTML tags used in the report

  insert_head()
  
# container ------
  
  htmls <- list()
  
# separators -------
  
  insert_msg('Separators')
  
  htmls$minor_sep <- 
    mdhtml('<hr style = "border-top: 1px solid DarkGray" />')
  
  htmls$major_sep <- 
    mdhtml('<hr style = "border-top: 5px solid DarkGray" />')
  
  htmls$section_sep <- 
    mdhtml('<hr style = "border-top: 5px solid #417a8b" />')
  
# END ------
  
  insert_tail()