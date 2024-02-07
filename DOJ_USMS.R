# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

source("setup.R")
file.name <- "DOJ_USMS" # for testing

clean <- function(file.name) {
  data_raw <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data_raw$LetterID <- 1:nrow(data_raw)
  
  # select distinct observations 
  data_distinct <- data_raw %>% select(-LetterID) %>% distinct()
  
  ##########################################################
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data_raw) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
  
  #removal of surplus columns
  
  data <- data[ -c(23:25) ]
  
  #rename columns
  
  data %<>% 
    rename(FROM = `Primary Contact`)
  
  #date column is the column for date the request was received 
  
  data %<>%
    rename(`DATE`= 'Date Rec')
    
  data$DATE %<>% 
    as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #to lower
  data %<>%
    mutate(FROM = str_to_lower(FROM))
  
    
    #remove "the Honorable" string from names in the FROM column
    
    string <- ("the honorable")
  data$FROM %<>%
    str_remove_all(string)
  
  #Indivdual line/member string changes
  
  data %<>%
    mutate(FROM = str_replace(FROM, "jamie herrera beutler", "jaime herrera beutler")) %>%
    mutate(FROM = str_replace(FROM, "mark w. warner", "mark warner")) %<>%
    mutate(FROM = str_replace(FROM, "lucille roybal- allard", "lucille roybal-allard"))
 

  
    
    library(legislators)
  data %<>% 
    legislators::extractMemberName("FROM",
                                   congress = "congress")
  
  
  return(data)
}

if(F){
  data %>% count(is.na(icpsr)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE) %>% print(n = 194)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()
}

print(distinct(data$FROM))


  