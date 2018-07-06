# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Minor spelling stuff, format rows and subjects yet

#file.name <- "DOC_OCPA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name 
  
  
   data$DATE %<>% as.Date("%m/%d/%Y")
   data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
   data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
   
  
  # create variable for first and last name
  data <- extractMemberName(data, members, 'FROM')
  
  # paste all subject content 
  data %<>% mutate(SUBJECT = paste(`ACTION TYPE`, SUBJECT, `ADDITIONAL NOTES`, staffer, stafferCONTACT_INFO, ACTIONS, STATUS ))
  
  
  
  # IS THIS OCPA, or coppied from CENSUS? 
  data%<>%
  mutate(SUBJECT=paste(SUBJECT,Constituent)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|AMERICAN COMMUNITY SURVEY|POPULATION SURVEY|SURVEY|CENSUS|EMPLOYMENT|WRONGFUL|ISSUE|STATUS|CHECK|DISCRIMINATION|TERMINAT|BENEFIT|ACCIDENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|AMERICAN COMMUNITY SURVEY|POPULATION SURVEY|SURVEY|CENSUS|EMPLOYMENT|WRONGFUL|ISSUE|STATUS|CHECK|DISCRIMINATION|TERMINAT|BENEFIT|ACCIDENT", SUBJECT, ignore.case = TRUE), "1",CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOWNSHIP|SCHOOL DISTRICT|CITY|LETTER IN SUPPORT OF", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOWNSHIP|SCHOOL DISTRICT|CITY|LETTER IN SUPPORT OF", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SAME SEX MARRIAGE|FOIA|COMMITTEE|RULE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SAME SEX MARRIAGE|FOIA|COMMITTEE|RULE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "THAN YOU", EVENT_NAME))
  
  
  
  
  
  
  
  
  
  
  
  
}