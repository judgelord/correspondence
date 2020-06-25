# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOI_NIGC Fatima" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

  
  # Rename to standard column names 
  data %<>% 
    mutate(SUBJECT = SUBJECT,
           DATE = DATE,
           FROM = FROM)  %>%
    select(DATE, FROM, SUBJECT, everything())
  
  
  # create agency column
  data %<>% 
    mutate(agency = file.name)
  
  
  # Format date, year, Congress, member name etc.
  data$DATE %<>% as.Date("%m/%d/%y")
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  #Create Chamber Variable
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. "), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. "), "House", chamber)) 
  

  #string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, ";")) %>%
    unnest(FROM)
  

  
  # create first and last name variables
  data %<>% extractMemberName(members, 'FROM')
  
  #Error for nonmembers
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Ken Rooney|Michon Johnson"), "Non members of Congress", ERROR))

  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)
}

