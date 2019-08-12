
# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOD_OIG" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data from google sheet
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
  
  data$DATE <- as.Date(data$'Final Date', "%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- gsub("\\d+-\\d+ (\\w.*)","\\1",data$Control)
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
    mutate(FROM = str_split(FROM, "/")) %>%
    unnest(FROM)
  
  data$LetterID <- seq(1:nrow(data))
  ################ 
  
  


  data$last_name <- formatLastName(data, 'FROM')
  data$first_name <- NA

  data$first_name <- addFirst(data$first_name,data$last_name)
  
  
  #formatlastname works way better than extractMemberName
  
  #data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  
  data$SUBJECT <- data$SUBJECT
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
}

