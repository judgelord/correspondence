# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FTC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data

  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #Checking for missing dates
 # NAdate<-data %>%
   # filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


  #No chamber variable in script because chambers may be wrong
  
  #Comments Errors for Pres and Vice Pres
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Pre "), "President", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Vic "), "Vice President", NOTES))
  
  #Filter while working
 # data %<>%
   # filter( ! str_detect(FROM, "Pre |Vic "))
  
  #Removes in FROM to allow matches
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen |Rep "))
  
  #Fixes problems with common name in quotes
  data %<>%
    mutate(FROM = str_replace(FROM, "David  \"Phil\" Roe", "Roe, David")) %>%
    mutate(FROM = str_replace(FROM, "William \"Mo\" Cowan", "Cowan, William")) %>%
    mutate(FROM = str_replace(FROM, "Bonnie Watson Colem2", "Bonnie Watson Coleman")) %>%
    mutate(FROM = str_replace(FROM, "Michelle Bachmann", "Bachmann, Michele")) %>%
    mutate(FROM = str_replace(FROM, "Scott Rigel!", "Scott Rigell")) %>%
    mutate(FROM = str_replace(FROM, "Sean Patrick Malone\\}", "Sean Patrick Malone")) %>%
    mutate(FROM = str_replace(FROM, "Wasserman-S<", "Wasserman Schultz")) %>%
    mutate(FROM = str_replace(FROM, "Neil Shaabercrombie", "Neil Abercrombie")) %>%
    mutate(FROM = str_replace(FROM, "ChristopherBond", "Christopher Bond")) %>%
    mutate(FROM = str_replace(FROM, "ChristopherSmith", "Christopher Smith")) %>%
    mutate(FROM = str_replace(FROM, "ChristopherDodd", "Christopher Dodd"))
  

  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Duncan Hunter") & is.na(last_name) & ! str_detect(congress, "110"), "Wrong Duncan, Duplicate", ERROR))
  
  #Checks for observations still NA
  Unfoundnames <- data %>%
   filter(is.na(last_name), 
   is.na(ERROR),
   !str_detect(FROM, "Kay Bailey Hutchison")) %>% count(FROM, congress, sort = T)
  
  
 # Unmatched <- d %>%
   # filter(is.na(bioname))
 

  return(data)
  
}

