# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 232 out of 1403 last names not matched. Wait for better data from agency. 

  #file.name <- "DOI_NPS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # Remove duplicated rows
  data <- data[!duplicated(data[,c('ID')]),]  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  
  ################ We got better quality data, now ignoring FROM2 col
  
  # create variable for last name
  # data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$FROM)
  # data$FROM2 <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, M.C.,|, III,|, P.E.,| Ii,| \\(Il\\), Rep.", replacement = ",", data$FROM2)
  # data$last_name <- formatLastName(data, 'FROM2')
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|ON BEHALF OF .*6|EMPLOYEE|(B) (6)|EMPLOYMENT|WRONGFUL TERMINATION|SEXUAL|INTERNSHIP|RETIREMEN|FARMHOUSE|WRONGFUL TERMINATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|ON BEHALF OF .*6|EMPLOYEE|(B) (6)|EMPLOYMENT|WRONGFUL TERMINATION|SEXUAL|INTERNSHIP|RETIREMENT|FARMHOUSE|WRONGFUL TERMINATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SCHOOL DISTRICT|CITY OF|GRANT APPLICATION|TOWNSHIP|REBUILD|SUPPORT APPLICATION|APPLICATION|GRANT|HISTORIC PRESERVATION|DONATION|CONSERVANCY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SCHOOL DISTRICT|CITY OF|GRANT APPLICATION|TOWNSHIP|REBUILD|SUPPORT APPLICATION|APPLICATION|GRANT|HISTORIC PRESERVATION|DONATION|CONSERVANCY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("CITY OF|GRANT APPLICATION|SUPPORT APPLICATION", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTRUCTION APPROVAL", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTRUCTION APPROVAL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "THANK YOU", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONGRESSIONAL SUPPORT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONGRESSIONAL SUPPORT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CONGRESSIONAL SUPPORT", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("URGE|COLLABORATION|REGULAT|LIST OF NEEDS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("URGE|COLLABORATION|REGULAT|LIST OF NEEDS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("URGE|REGULAT", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "MEETING", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RESOLUTION|CONGRESSIONAL CONCERN|HEARING|SURVEY|CENTENNIAL|CONCERNED ABOUT NPS|REQUESTS PRESIDENT|EVERY CLASSROOM|CONGRESSIONAL QUESTIONS|EVERGLADES RESTORATION|WATER BOTTLE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RESOLUTION|CONGRESSIONAL CONCERN|HEARING|SURVEY|CENTENNIAL|CONCERNED ABOUT NPS|REQUESTS PRESIDENT|EVERY CLASSROOM|CONGRESSIONAL QUESTIONS|EVERGLADES RESTORATION|WATER BOTTLE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("RESOLUTION|CONGRESSIONAL CONCERN|BOUNDARY|CONCERNED ABOUT NPS|WATER BOTTLE", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING|CONGRESSIONAL QUESTIONS|STUDY", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("SURVEY", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BOUNDARY|NATIONAL REGISTER OF HISTORIC PLACES|STUDY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BOUNDARY|NATIONAL REGISTER OF HISTORIC PLACES|STUDY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BOUNDARY|NATIONAL REGISTER OF HISTORIC PLACES|STUDY", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RESPONSE LETTER|MUSEUM", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RESPONSE LETTER", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RESPONSE LETTER", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NOMINATION|MUSEUM", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
  
  
  
  
  
}
