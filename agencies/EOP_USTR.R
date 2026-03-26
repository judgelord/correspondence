# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "EOP_USTR Hope" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()
  
  nrow(data)
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  nrow(data)
  
  # create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATEoriginal <- data$DATE
  
  
  data$DATE <- data$DATEoriginal
  #FIXME some massive loss due to bad dates
  data$DATE %<>% str_replace("\\.201", ".1") 
  data$DATE %<>% str_replace("\\.20", ".0") 
  data$DATE %<>% multidate(formats = c("%d-%b-%y", "%m/%d/%y", "%m.%d.%y")) # FIXME
  data$DATE %<>% as.Date()
  
  # inspect 
  data %>% filter(is.na(DATE)| DATE < as.Date("2000-01-01") | DATE > as.Date("2020-01-01")) %>% 
    count(LetterID, DATEoriginal,DATE, FROM)
  
  # make congress
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data %<>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "Tariff"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT, "Tariff"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "Tariff"),"international agreement;trade",POLICY_EVENT))%>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "WTO"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT,"WTO"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "WTO"),"international agreement;trade",POLICY_EVENT))%>%
    mutate(EVENT_NAME = ifelse(!str_detect(EVENT_NAME, "[:alnum:]")& str_detect(SUBJECT, "WTO"), "WTO Ruling", EVENT_NAME))%>%                            
    mutate(TYPE = ifelse(!str_detect(TYPE,"[0-9]")& str_detect(SUBJECT, "FTA"),4,TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT, "FTA"),1,CERTAINTY))%>%
    mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "FTA"),"international agreement;trade", POLICY_EVENT))%>%
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]")& str_detect(SUBJECT, "Courtesy Meeting"),2, TYPE))%>%
    mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY, "[0-9]")& str_detect(SUBJECT, "Courtesy Meeting"),1,CERTAINTY))
          
  return(data)
}
