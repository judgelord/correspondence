# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Finished. All matched on last_name / first_name

#file.name <- "DOT_SLSDC Aaron" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create FROM column
  data$FROM <- paste(data$first_name, " ", data$last_name )
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # chamber variable
  data %<>%
    mutate(chamber = ifelse(grepl("Senate", chamber), "Senate", chamber)) %>% 
    mutate(chamber = ifelse(grepl("House", chamber), "House", chamber))
  
  # create variable for first and last name
  data$first_name <- formatFirstName(data, 'first_name')
  data$first_name <- gsub("(\\w+) .*", "\\1", data$first_name)
  data$last_name <- formatLastName(data, 'last_name')
  data$last_name <- gsub("(\\w+)( |,) .*", "\\1", data$last_name)
  
  data %<>% mutate(FROM = paste(chamber, first_name, last_name))
  
  data %<>% extractMemberName(members, "FROM")
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  return(data)
}

