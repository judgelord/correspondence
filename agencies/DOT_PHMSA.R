# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOT_PHMSA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%B %d %Y")
  
  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %<>% select(ID, DATE, FROM, everything())  
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Senate|Senator"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Representative|House"), "House", chamber))
  
  NoChamber <- data %>%
    filter(is.na(chamber))
  
  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Steven Ted", "Stevens, Ted"))
  
  #Extract member names
  data %<>% extractMemberName2(members, "FROM")
  
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  
  
  
  
  
  return(data)
  
}

    