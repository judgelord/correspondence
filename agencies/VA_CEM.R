# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "VA_CEM" # for testing



clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  names(data)[names(data) == 'Date Received'] <- 'DATE'
  names(data)[names(data) == 'Primary Person'] <- 'FROM'
  names(data)[names(data) == 'Subject'] <- 'SUBJECT'
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
}
