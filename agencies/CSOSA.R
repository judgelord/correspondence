#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "CSOSA Julia" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  data %<>%
    mutate(Blank = is.na(FROM) & is.na(SUBJECT)) %>%
    filter(! Blank)
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name 
  
  
  
  #Make ??? NA
 #Format Date
  data$DATE %<>% as.Date("%m/%d/%y")
  data$`Date of Reply/Contact` %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date of Reply/Contact`, DATE))
  data$DATE %<>% as.Date("%d/%m/%y")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
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
  
  #Paste Chamber into FROM
  data %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House") & !str_detect(FROM, ","), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate") & !str_detect(FROM, ","), paste("Senator", FROM, sep = " "), FROM))
  

  
  data %<>%
    mutate(FROM = str_remove(FROM, "Senate|House|\\. Chair|V\\. Chair|Chair|, OMB| OMB|Sender\\'s Information"))
  
  #Typo  
  data %<>%
    mutate(FROM = str_replace(FROM, "M\\. Mulvaney|Mulvaney", "Mick Mulvaney")) %>%
    mutate(FROM = str_replace(FROM, "Waxman|Waxman,", "WAXMAN, Henry")) %>%
    mutate(FROM = str_replace(FROM, "Donovan", "Daniel DONOVAN")) %>%
    mutate(FROM = str_replace(FROM, "Senator \\\nCoons ", "Christopher COONS"))
  

  #extracts member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  #data %<>%
    #mutate(FROM = str_trim(FROM)) %>%
    #mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
 

  
  #NoFirst <- data %>%
    #filter(is.na(first_name) & ! is.na(last_name))
  
  #Add first name 
  #data %<>%
    #mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name))
  
  
   data %<>% select(ID, DATE,  FROM, last_name, chamber, SUBJECT, everything())
   
   Unfound <- data %>%
     filter(is.na(last_name))
   
   #Errors
   data %<>%
     mutate(ERROR = ifelse(str_detect(FROM, "Daniel DONOVAN") & congress %in% 113, "Not yet in congress", ERROR))
  
   data %>%
     filter(ID == 6) %>%
     select(FROM)
   #Check after run through merge
   #Unfoundnames <- d %>%
   #filter(is.na(bioname))
  
   data %<>%
     mutate(NOTES = ifelse(str_detect(FROM, "Committee"), "Committee", NOTES))%>%
     mutate(TYPE = ifelse(!str_detect(TYPE,"[0-9]")& str_detect(Action,"Congressional Report FY"),5,TYPE ))%>%
     mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY,"[0-9]")& str_detect(Action, "Congressional Report FY"),1,CERTAINTY))%>% 
     mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT,"[:alnum:]")& str_detect(Action, "Congressional Report FY"),"budget allocation", POLICY_EVENT))%>%
     mutate(EVENT_DATE = ifelse(!str_detect(EVENT_DATE,"[:alnum;]")& str_detect(Action, "Congressional Report FY"),"future",EVENT_DATE ))%>%
     mutate(TYPE = ifelse(!str_detect(TYPE,"[0-9]")& !str_detect(Addressee,"[:alnum:]")& !str_detect(SUBJECT,"[:alnum:]"),0,TYPE))%>%
     mutate(CERTAINTY=ifelse(!str_detect(CERTAINTY,"[0-9]")& !str_detect(Addressee,"[:alnum:]") & !str_detect(SUBJECT,"[:alnum:]"),0,CERTAINTY))
  
  return(data)
}

