# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Treasury_FinCEN" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create LetterID
  data %<>%
    mutate(LetterID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  #Format date
  
  data %<>%
    mutate(DATE = str_replace(DATE, "200", "0")) %>%
    mutate(DATE = str_replace(DATE, "201", "1"))
  
  data$tempDATE<- data$DATE %>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = ifelse(is.na(tempDATE), `Due Date`, DATE))
  
  data %<>%
    mutate(DATE = str_replace(DATE, "200", "0")) %>%
    mutate(DATE = str_replace(DATE, "201", "1"))
 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = str_replace(DATE, "2030-03-04", "2015-03-04"))
  
  #Check for DATE NAs
  NoDATE <- data %>%
    filter(is.na(DATE))
    

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(Summary, "Congressman|Rep.|Con. |con. |cong |congs |cong. |rep| congressman  | Congresswoman "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(Summary, "Sen |Sen.|Senators"), "Senate", chamber))

  #String split for multiple members
  data %<>%
    mutate(Summary = str_split(Summary, ",| and ")) %>%
    unnest(Summary)
  
  #Recode to match chamber_last
  data %<>%
    mutate(Summary = str_replace_all(Summary, "Congressman|Rep.|rep |cong | congressman |Cong. |Congresswoman", "Representative ")) %>%
    mutate(Summary = str_replace_all(Summary, "Senators|senators", "Senator"))
  
  #Extract Member names
  data %<>%
    extractMemberName2(members = members, col_name = "Summary")
  
 
  #Add first name 
  data %<>%
    mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name))
  
  
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  
  
  
  
  
  return(data)
  
}