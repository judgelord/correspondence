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
    ))%>% 
    mutate(last_name = str_to_upper(last_name))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data %<>% mutate(TYPE =
                     ifelse (grepl(
                       # i.e. if SUBJECT contains:
                       # (& means "AND",  | means "OR")
                       "Service", 
                       Category), 
                       1, TYPE))  # then make it TYPE 1, otherwise keep TYPE
  
} # end function
