# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "OSMRE" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name 
  
  
  data$DATE <-  as.Date(data$Received, "%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  # create variable for first and last name
  data$FROM <- data$Author
  data <- extractMemberName(data, members, 'FROM')
  
 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  
  
  
  
  
  
  
}