# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "CNCS" # for testing


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
  
  
  #Format Date
  data$tempDATE<- data$DATE %>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = ifelse(is.na(tempDATE), Out, DATE))
  data$DATE %<>% as.Date("%m/%d/%y")
 
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
#String Split for Multiple Members
  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and|Rep. |Sen. |(S), |(CW), |(CM), |AK-2")) %>%
    unnest(FROM)
  
  #Create Chamber Variable
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. |\\(S\\)|Sen |Sens. |"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. |\\(CW\\)|\\(CM\\)|Rep |Reps. "), "House", chamber)) %>%
    mutate(chamber = ifelse(str_detect(Title, "CM|CW"), "House", chamber)) %>%
    mutate(chamber = ifelse(str_detect(Title, "S"), "Senate", chamber))
  
  #Remove in FROM
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen. |\\)")) %>%
    mutate(FROM = str_remove(FROM, "Rep. |\\)|\\(|Reps |Sen | NJ| CM| CW|Senator |\\(CW\\)|\\(CM\\)|Rep |Sens. |Reps. | NY-19| CM"))

  
  data %<>% 
    mutate(FROM = paste(chamber, FROM) %>% 
             str_replace("Senate", "Senator") %>% 
             str_replace("House", "Representative"))
  
  #Typos
  #added misspellings of names into nameMethods
  data %<>%
   mutate(FROM = str_replace(FROM, "Thompson Glen \"GT\"", "Thompson, Glenn")) %>%
   mutate(FROM = str_replace(FROM, "Merkley letter", "Jeff Merkley")) %>%
   mutate(FROM = str_replace(FROM, "Hall NY-19", "Hall")) %>%
   mutate(FROM = str_replace(FROM, "Hodes CM", "Hodes")) %>%
   mutate(FROM = str_replace(FROM, "Markey", "MARKEY, Edward")) %>%
   mutate(FROM = str_replace(FROM, "Gillibrand", "GILLIBRAND, Kirsten")) %>%
   mutate(FROM = str_replace(FROM, "NH, Delegation Sens. Ayotte", "Senator Ayotte")) %>%
   mutate(FROM = str_replace(FROM, "Ryan, P.", "RYAN, Paul")) %>%
   mutate(FROM = str_replace(FROM, "Smith, C. NJ", "SMITH, Christopher")) %>%
   mutate(FROM = str_replace(FROM, "Bass", "Charlie Bass"))  
 
  
  
  data %<>%
    mutate(FROM = str_squish(FROM))
  

  #Extract Member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  #Remove Blank Spaces
  data %<>% mutate(ERROR = ifelse(FROM == "", "blank", ERROR))
  
  #ERRORS Not members
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Gov |Gov.|Mayor"), "State Politician", ERROR))
  

  
  #Filter for stil unnamed
  Unfoundnames <- data %>%
    filter(is.na(last_name) & str_detect(FROM, " ") & ! str_detect(FROM, ", "))
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
      
  
  
  
  
 #Notes for multiple unnamed members 
  data %<>%
    mutate(NOTES = ifelse(str_detect(Title, "Multi"), "Multiple unnamed members", NOTES))
  
  Unfound <- data %>%
    filter(is.na(last_name))
  
 
  #FOIA
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Udall|udall") & is.na(first_name), "Multiple Udall's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Levin") & is.na(first_name) & is.na(chamber), "Multiple Levin's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Brown") & is.na(first_name) & is.na(chamber), "Multiple Brown's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Alexander") & is.na(first_name) & is.na(chamber), "Multiple Alexander's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Reed") & is.na(first_name) & is.na(chamber), "Multiple Reed's FOIA", NOTES))
  
  # nochamber <- data %>%
  #   filter(is.na(first_name) & is.na(chamber) & is.na(NOTES))
  
  #Check after run through merge
 #Unmatched <- d %>%
  #filter(is.na(bioname))
  
  return(data)
  
}
    