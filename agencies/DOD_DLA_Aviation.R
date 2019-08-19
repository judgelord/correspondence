# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOD_DLA_Aviation" # for testing


clean <- function(file.name) {
  # get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc.
  data$DATE %<>% as.Date("%m/%d/%y")
  data$date.closed %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001

  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep|ep ", FROM), "House", chamber))
  
  
  # create first and last name variables
  data <- extractMemberName(data,members,'FROM')
  
  #specific correction
  # data %<>%
  #   mutate(last_name = ifelse(LetterID == 31, NA, last_name)) %>%
  #   mutate(first_name = ifelse(LetterID == 31, NA, first_name)) %>%
  #   mutate(chamber = ifelse(LetterID == 31, NA, chamber))
   
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)
  
} # end function 

