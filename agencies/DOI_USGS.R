# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# matched on last_name perfectly, only last_name and chamber info

 #file.name <- "DOI_USGS" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data$FROM <- data$'Last Name'

  # create variable for last name
  data$last_name <- formatLastName(data, 'FROM')
  
  #formatLast_name works better than extractmembername
  
  #data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(`Last Name`),
           is.na(ERROR))  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", Salutation), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", Salutation), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())
  
  return(data)
}