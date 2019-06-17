# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FTC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data

#Create ID
  data %<>%
    mutate(ID = row_number())
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  data %<>% select(ID, DATE, FROM, everything())  

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
    mutate(FROM = str_replace(FROM, "James lnhofe", "Inhofe, James")) %>%
    mutate(FROM = str_replace(FROM, "Scott Rigel!", "Scott Rigell")) %>%
    mutate(FROM = str_replace(FROM, "Sean Patrick Malone\\}", "Sean Patrick Malone"))
  

  data <- getFirstLast.Comma(data, col_name = "FROM")
  
  #Checks for NAs
  notfound <- data %>%
    filter(is.na(last_name))
  
  
  #Extracts member names from NAs in getfirstlast
  Unfoundnames <- data %>%
    filter(is.na(last_name)) %>%
    extractMemberName(members = members, col_name = "FROM")
    
  #Drops duplicate observations  
  data %<>%
    drop_na(last_name)
  
  #Rejoins data
  data %<>%
    full_join(Unfoundnames)
  
  #Checks for observations still NA
  notfound2 <- data %>%
    filter(is.na(last_name))
 

  return(data)
  
  }