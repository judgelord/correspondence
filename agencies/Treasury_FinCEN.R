# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Treasury_FinCEN" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create LetterID
  data %<>%
    mutate(LetterID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  #Format date
  data$tempDATE<- data$DATE %>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = ifelse(is.na(tempDATE), `Due Date`, DATE))
 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #Extract Member names
  data %<>%
    extractMemberName2(members = members, col_name = "Summary")
  
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  
  
  
  
  
  return(data)
  
}