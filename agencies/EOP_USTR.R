# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "EOP_USTR Hope" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% multidate("%d-%b-%y", "%m/%d/%y") # FIXME
  
  bad.dates <- data %>% filter(is.na(DATE)) %>% .$LetterID
  data$DATE[bad.dates]
  
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  data <- extractMemberName(data, members, 'FROM')
  
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data %<>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "Tariff"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT, "Tariff"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "Tariff"),"international agreement;trade",POLICY_EVENT))%>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "WTO"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT,"WTO"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "WTO"),"international agreement;trade",POLICY_EVENT))%>%
    mutate(TITLE = ifelse(!str_detect(TITLE, "[:alnum:]")& str_detect(SUBJECT, "WTO"), "WTO Ruling", TITLE))%>%                            
    mutate(TYPE = ifelse(!str_detect(TYPE,"[0-9]")& str_detect(SUBJECT, "FTA"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9")& str_detect(SUBJECT, "FTA"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum;]")& str_detect(SUBJECT, "FTA"),"international agreement;trade", POLICY_EVENT))%>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "Courtesy Meeting"),2, TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT, "Courtesy Meeting"),1,CERTAINTY))
          return(data)
}
