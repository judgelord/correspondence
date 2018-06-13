# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Minor spelling stuff, format rows and subjects yet

#file.name <- "DOC_OS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name 
  
  
   data$DATE %<>% as.Date("%m/%d/%y")
   data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
   data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
   
  
  # create variable for first and last name
   data$FROM %<>% {gsub(" [A-Z]. "," ",.)}
  data <- extractMemberName(data, members, 'FROM')
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|AMERICAN COMMUNITY SURVEY|POPULATION SURVEY|SURVEY|CENSUS|EMPLOYMENT|WRONGFUL|ISSUE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|AMERICAN COMMUNITY SURVEY|POPULATION SURVEY|SURVEY|CENSUS|EMPLOYMENT|WRONGFUL|ISSUE", SUBJECT, ignore.case = TRUE), "1",CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOWNSHIP|SCHOOL DISTRICT|CITY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOWNSHIP|SCHOOL DISTRICT|CITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("MAYOR OF|MAYOR", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("POPULATION ESTIMATE", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SAME SEX MARRIAGE|FOIA|COMMITTEE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SAME SEX MARRIAGE|FOIA|COMMITTEE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "THAN YOU", EVENT_NAME))
  
  
  
  
  
  
  
  
  
  
  
  
}