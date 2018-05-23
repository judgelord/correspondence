# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "PRC" # for testing

clean <- function(file.name) {
  #  get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  # create agency column
  data$agency <- file.name
  
  #create year and congress columns
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen\\.|Sen |Senator ", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep\\.|Rep |Representative ", FROM), "House", chamber))
  
  
  
  
  # format state column
  data$state <- as.character(stateFromLower(data$State))
  
  
  #create last name variable for Sen/Rep
  data %<>%
    mutate(last_name = gsub(
      pattern = ".* |.*\\.",
      replacement = "",
      x = FROM
    ))
  data$last_name <- formatLastName(data, 'last_name')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data %<>% 
    mutate(TYPE =
                     ifelse (!grepl("[0-9]", TYPE) &grepl(
                     "Service", 
                       Category), 
                       1, TYPE))  %>%
    mutate(TYPE =
             ifelse (!grepl("[0-9]", TYPE) &grepl(
               "PO Closing", 
               Issue), 
               1, TYPE)) %>% #Post Office Closing 
    mutate(TYPE =
           ifelse (!grepl("[0-9]", TYPE) &grepl(
             "Rates", 
             Category), 
             2, TYPE)) %>%
    mutate(TYPE =
           ifelse (!grepl("[0-9]", TYPE) & grepl(
             "Lobby Hours", 
             Issue), 
             1, TYPE)) %>%
    mutate(TYPE =
           ifelse (!grepl("[0-9]", TYPE) & grepl(
             "Delayed Mail", 
             Issue), 
             1, TYPE))
  
  data %<>%
     mutate(TYPE =
             ifelse (!grepl("[0-9]", TYPE) &grepl("Undelivered Mail", 
               Sub_Issue), 
               1, TYPE))
    
  
  
} # end function
