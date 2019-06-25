#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "CSOSA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create LetterID
  data %<>%
    mutate(LetterID = row_number())
 
  data %<>%
    mutate(Blank = is.na(FROM) & is.na(SUBJECT)) %>%
    filter(! Blank)
  
  #create agency column
  data$agency <- file.name 
  
  
  
  #Make ??? NA
 #Format Date
  data$DATE %<>% as.Date("%m/%d/%y")
  data$`Date of Reply/Contact` %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date of Reply/Contact`, DATE))
  data$DATE %<>% as.Date("%d/%m/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "House"), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Senate"), "Senate", chamber))
  
  #String split to separate members
  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(FROM = str_remove(FROM, "Senate|House|. Chair|V. Chair|Chair|, OMB| OMB|Sender's Information"))
  

  #extracts member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  data %<>%
    mutate(FROM = str_trim(FROM)) %>%
    mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
 

  
  NoFirst <- data %>%
    filter(is.na(first_name) & ! is.na(last_name))
  
  #Add first name 
  data %<>%
    mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name))
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
  
 
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Committee"), "Committee", NOTES))
  
   data %<>% select(ID, DATE,  FROM, last_name, chamber, SUBJECT, everything())
   
   Unfound <- data %>%
     filter(is.na(last_name))
  
   #Check after run through merge
   #Unfoundnames <- d %>%
   #filter(is.na(bioname))
   
  
  
  return(data)
}