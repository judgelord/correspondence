# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "ABMC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  names(data)[names(data) == 'Date Received'] <- 'DATE'
  names(data)[names(data) == 'Source'] <- 'FROM'
  names(data)[names(data) == 'Title'] <- 'SUBJECT'
  
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  


  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
}
