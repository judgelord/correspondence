# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# 170 out of 190 matching. No first name, state, or chamber information. 

#file.name <- "DOD_DeCA Devin" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #remove unwanted rows
  data <- data[-which(-is.na(data$FROM)& is.na(data$'CNTL NO')),]
  data <- data[-which(is.na(data$FROM)),]
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for  last name
  data$last_name <- formatLastName(data, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  
  
  
  
  
  
  
  
}