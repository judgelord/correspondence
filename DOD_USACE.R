# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOD_USACE" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub(" .*","", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl(" REP | REPS | Rep ", SUBJECT), "House", NA)) %>%
    mutate(chamber = ifelse(grepl(" SEN | SENS | Sen ", SUBJECT), "Senate", chamber))
  
  # member name
  data %<>% 
    mutate(SUBJECT = paste(Priority, SUBJECT, Owner)) 
  
  data$last_name <- gsub(".* REP |.* REPS|.* SEN |.* SENS |.* CONGRESSIONAL -|.* CONRESSIONAL - |^Routine ","", data$SUBJECT)
  data$last_name <-  toupper(data$last_name)  %>%
  {gsub("^ |^MR. ", "", .)}  %>%
  {gsub("-.*| .*|,.*|:.*", "", .)}
  

data %<>% select(DATE, SUBJECT, last_name, chamber, everything())  
  
  
}