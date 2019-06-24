
#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Treasury_Mint" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name 
  
  
  #Format Date
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  data %<>%
    mutate(Blank = is.na(FROM) & is.na(SUBJECT)) %>%
    filter(! Blank)
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Congressman|Rep.|Con. |con. "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Sen |Sen."), "Senate", chamber))
  
  
  
  #Extract member names from SUBJECT
  data %<>%
    extractMemberName(members = members, col_name = "SUBJECT")
  
  Unfoundnames<- data %>%
    filter(is.na(last_name))
  

  
  return(data)
}
  