# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 # file.name <- "DOJ_CIV" # for testing
 
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
  
  data$DATE %<>% 
    #str_replace("/ */", "/1/") 
    as.Date("%Y-%m-%d")
  
  data$originalDATE %<>%
    str_replace(" 20", "/20") %>%
    str_replace("([0-9])20", "\\1/20") %>% 
    as.Date("%m/%d/%y")
  
  data$DATE %<>% coalesce(data$originalDATE)
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))

  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # FIXME # THIS IS NOT TARGETED ENOUGH
  data %<>%
    mutate(First.Name = ifelse(data$Last.Name == "YOUNG", "Bill", data$First.Name))  
  data %<>% 
    mutate(First.Name = ifelse(data$Last.Name == "AKIN", "Todd", data$First.Name))
  
  # combine first and last name and call name method
  data$FROM <- paste(data$First.Name, data$Last.Name)

  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  # Errors for missing data
  data %<>%
    mutate(ERROR = ifelse(data$FROM == "NA NA", "NA FROM information", ERROR))
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RADIATION COMPENSATION|911 VICTIM|REFUND|RETURN|(6)|REQUEST|CLAIM|BREAST", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RADIATION COMPENSATION|911 VICTIM|REFUND|RETURN|(6)|REQUEST|CLAIM|BREAST", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROSECUTION|ILLEGAL DRUG RAIDS|HEARING|DOD|REQ|NAVAL AIR STATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROSECUTION|ILLEGAL DRUG RAIDS|HEARING|DOD|REQ|NAVAL AIR STATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROSECUTION", SUBJECT, ignore.case = TRUE), "ENFORCEMENT", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOBACCO", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOBACCO", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
return(data)  
  
}
