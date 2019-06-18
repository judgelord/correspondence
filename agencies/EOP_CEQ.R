# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EOP_CEQ" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  #Create ID  
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc.

  data$DATE %<>% as.Date("%d-%b-%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  


  
  
  
  
return(data)  
}