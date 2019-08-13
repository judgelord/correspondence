# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOT_FTA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data from google sheet

  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
 
  data$originalDATE <- data$DATE 
  
  data$DATE <- as.Date(data$DATE, "%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- paste(data$FName, data$LName, sep  = " ")
  data <- extractMemberName(data, members, 'FROM')
  

  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
return(data)  
}

