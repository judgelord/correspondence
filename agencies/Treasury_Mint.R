
#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Treasury_Mint Rochelle" # for testing

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
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  data %<>% filter(!is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Congressman|Rep.|Con. |con. "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Sen |Sen."), "Senate", chamber))
  
  #Extract member names from SUBJECT
  data$FROM <- data$SUBJECT
  data %<>%
    #select(-chamber) %>% #FIXME? depends on how well it works without
    extractMemberName(members = members, col_name = "FROM")
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  Unfoundnames %>% 
    group_by(FROM) %>% 
    summarise(congress = str_c(congress, collapse = ";")) %>% distinct()  #%>% kable
  
  
  data %>% filter(!is.na(last_name))
  
  data %>% filter(pattern != "404error")  %>% 
    select(congress, pattern, SUBJECT, last_name) %>% 
    #select(-last_name) %>% 
    inner_join(members)
  
  data %>% select(congress, pattern, FROM)
  
  
  #FIXME from here down DROP CHAMBER or complete missing chamber?
  
 lastnames <- str_c(members$last_name, collapse = "|")

 memberlastnames <- data %>%
   filter(str_detect(SUBJECT, lastnames))
   
 memberletters <- data %>%
   filter(str_detect(SUBJECT, "entative|REPRESENTATIVE |SENATOR |CONGRESSMAN ", ignore.case = T))
 
  
  return(data)
}
  