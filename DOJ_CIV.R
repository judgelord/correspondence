# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



 #file.name <- "DOJ_CIV" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")

  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  
  data$last_name <-  formatLastName(data, "Last Name")
  data$last_name <- gsub("\\*", "", data$last_name)
  
  data$first_name <- formatFirstName(data, 'First Name')
  data$first_name <- gsub("^(\\w+).*", "\\1", data$first_name)
  
  data %<>%
    mutate(first_name = ifelse(data$last_name == "YOUNG", "Bill", data$first_name))  
  data %<>% 
    mutate(first_name = ifelse(data$last_name == "AKIN", "Todd", data$first_name))
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
}
