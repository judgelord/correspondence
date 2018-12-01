# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "NARA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

  data$ID <- c(1:nrow(data))
  
  colnames(data)[colnames(data) == 'Description'] <- 'SUBJECT'
  
  
  # create agency column
  data$agency <- file.name

  # Format date, year, Congress, member name etc.
  colnames(data)[colnames(data) == 'Date'] <- 'DATE'
  data$DATE %<>% multidate( c("%m-%d-%y", "%m/%d/%Y"))


  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


  colnames(data)[colnames(data) == 'Member of Congress'] <- 'FROM'
  
 
  # create first and last name variables
  data <- extractMemberName(data, members, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(DATE, FROM, SUBJECT, everything())

}






