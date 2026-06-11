# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "USPS" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  

  # create agency column
  data$agency <- file.name

  # Format date, year, Congress, member name etc.
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #checking for noDATE
  NOdate <- data %>%
    filter(is.na(DATE))

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  #Formerly used getFirstLast
  #data <- getFirstLast.Comma(data, 'FROM')
  
  
  #Changing from getFirstLast to extractMemberName
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }  

  
  
  data %<>%
  mutate(ERROR = ifelse(grepl("^White House$", FROM, ignore.case=T), "White House", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^(Miscellaneous|MICELLANIOUS)$", FROM, ignore.case=T), "Miscellaneous", ERROR))


  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INJURY COMPENSATION|LOST MAIL REPORTS|EMPLOYEE|RETIREMENT ISSUES|DELAYED MAIL|ACCESSIBILITY|FOIA|FORWARDING|ZIPCODES|INDEMNITY|PRIORITY MAIL|LOBBY SERVICE|COLLECTION|FRAUD|REASSIGNMENT|REINSTATEMENT|SPECIAL SERVICES|HARDSHIP DELIVERY|PERIODICALS|MAIL DELIVERY TIME|LEGAL ISSUES|SELECT|POST OFFICE BOXES|MAILABILITY|EEO|PHILATELIC|MISC", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INJURY COMPENSATION|LOST MAIL REPORTS|EMPLOYEE|RETIREMENT ISSUES|DELAYED MAIL|ACCESSIBILITY|FOIA|FORWARDING|ZIPCODES|INDEMNITY|PRIORITY MAIL|LOBBY SERVICE|COLLECTION|FRAUD|REASSIGNMENT|REINSTATEMENT|SPECIAL SERVICES|HARDSHIP DELIVERY|PERIODICALS|MAIL DELIVERY TIME|LEGAL ISSUES|SELECT|POST OFFICE BOXES|MAILABILITY|EEO|PHILATELIC|MISC", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POST OFFICE ACTIONS", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POST OFFICE ACTIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("POST OFFICE ACTIONS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DELIVERY METHOD|STAMP SUGGESTIONS|RATES|OPERATIONS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DELIVERY METHOD|STAMP SUGGESTIONS|RATES|OPERATIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DELIVERY METHOD|STAMP SUGGESTIONS|RATES|OPERATIONS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DEL SVC|PROCUREMENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DEL SVC|PROCUREMENT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INSPECTION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INSPECTION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("INSPECTION", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POSTMASTER ISSUES", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POSTMASTER ISSUES", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STAFFING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STAFFING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("STAFFING", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MAIL PROCESSING|FACILITIES", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MAIL PROCESSING|FACILITIES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("MAIL PROCESSING|FACILITIES", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[A-Z]", EVENT_NAME) & grepl("EEO", SUBJECT, ignore.case = TRUE), "EQUAL EMPLOYMENT OPPORTUNITIES", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ADVERTISING MAIL|MARKETING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ADVERTISING MAIL|MARKETING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ADVERTISING MAIL|MARKETING", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE))
    
  
  
  
  
  
  
  
return(data)

}
