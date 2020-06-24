# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


 #170 out of 190 matching. No first name, state, or chamber information. 

 # file.name <- "DOD_DeCA Devin" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #remove unwanted rows
  data <- data[-which(-is.na(data$FROM)& is.na(data$'CNTL NO')),]

  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for  last name
  data$last_name <- formatLastName(data, 'FROM')
  
  # add first name column
  data %<>% add_first()
  
  data  %<>% mutate(FROM = paste(first_name, FROM) %>% str_remove("NA "))
  
  data %<>% extractMemberName(members, 'FROM') 

  
  #Failing observations
  Unfoundnames <- data %>%
    filter(pattern == "404error",
           is.na(ERROR))
  Unfoundnames %>% select(congress, FROM) %>% distinct() %>% kable()
  

  return(data)  
}