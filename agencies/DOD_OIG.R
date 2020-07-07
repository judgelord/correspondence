
# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOD_OIG Fatima" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data from google sheet
  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$DATE <- as.Date(data$'Final Date', "%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- gsub("\\d+-\\d+ (\\w.*)","\\1",data$Control)
  
  data$last_name <- formatLastName(data, 'FROM') 
  
  data$last_name %<>% str_squish() %>% str_extract("[A-z]*")
  
  #inspect
  paste(data$last_name, data$FROM, data$Control, sep = "<--")
  
  
  # correct typos 
  data$last_name %<>% 
    str_replace("GRASSLE", "GRASSLEY")
  
  # add first name column
  data %<>% add_first()
  
  data %<>% 
    mutate(FROM = paste(first_name, last_name))
  #formatlastname works way better than extractMemberName
  
  data %<>% extractMemberName(members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  
  data$SUBJECT <- data$SUBJECT
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)  
}

