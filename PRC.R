# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "PRC" # for testing

clean <- function(file.name) {
  # get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  # create agency column
  data$agency <- file.name
  
  #create year and congress columns
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  #Create variable for position title (Senator or Representative)
  data %<>%
    mutate(title = ifelse (grepl("Sen\\.|Sen |Senator ", FROM), "Senator", NA)) %>%
    mutate(title = ifelse(grepl("Rep\\.|Rep |Representative ", FROM), "Representative", title))
  
  
  
  
  #reformat state column
  data$State %<>% stateFromLower()
  
  
  #create last name variable for Sen/Rep
  data %<>%
    mutate(last_name = gsub(
      pattern = ".* |.*\\.",
      replacement = "",
      x = FROM
    ))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
} # end function
