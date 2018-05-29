# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# 284 (out of 4363) not matching, go back and fix

#file.name <- "DHHS_CDC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  ### Remove duplicate IDs
  data <- data[!duplicated(data$ID), ]
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|SENATORS SHERROD BROWN|SENATORS CLINTON AND SCHUMER|SENATORS JACK REED|MICHAEL HONDA|SENATOR LINDSEY GRAHAM|SENATORS JOHN THUNE|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|SENATORS SHERROD BROWN|SENATORS CLINTON AND SCHUMER|SENATORS JACK REED|MICHAEL HONDA|SENATOR LINDSEY GRAHAM|SENATORS JOHN THUNE|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|SENATORS SHERROD BROWN|SENATORS CLINTON AND SCHUMER|SENATORS JACK REED|MICAHEL HONDA|SENATORS JOHN THUNE", SUBJECT, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEPATITIS|SENATORS EDWARD KENNEDY", Title, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEPATITIS|SENATORS EDWARD KENNEDY", Title, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEPATITIS", Title, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU NOTE|TED S. YOHO", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU NOTE|TED S. YOHO", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]",CERTAINTY) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DIACETYL|ANTIBIOTICS IN|OPIOID SENATORS|PHIL GINGREY, JOSEPH |PHIL GINGREY AND|REQUESTING A BRIEFING|ROSA DELAURO, NITA|KEN SALAZAR AND MARK UDALL|AMY KLOBUCHAR, JEFF|REPRESENTATIVES DAVID OBEY|SENATORS DAVID OBEY|SENATOR DIANNE FEINSTEIN|SENATORS JOHN KERRY|H5N1|DAVID HEYMSFELD", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DIACETYL|ANTIBIOTICS IN|OPIOID SENATORS|PHIL GINGREY, JOSEPH|PHIL GINGREY AND|REQUESTING A BRIEFING|ROSA DELAURO, NITA|KEN SALAZAR AND MARK UDALL|AMY KLOBUCHAR, JEFF|REPRESENTATIVES DAVID OBEY|SENATORS DAVID OBEY|SENATOR DIANNE FEINSTEIN|SENATORS JOHN KERRY|H5N1|DAVID HEYMSFELD", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("DIACETYL|ANTIBIOTICS IN|PHIL GINGREY, JOSEPH|SENATORS JOHN KERRY", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SENATOR KEN SALAZAR|SENATORS TOM CARPER", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%   #ON BEHALF OF UNION WORKERS
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SENATOR KEN SALAZAR|SENATORS TOM CARPER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%  
  mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("SENATOR KEN SALAZAR", SUBJECT, ignore.case = TRUE), "PRETTY SURE THIS IS ON BEHALF OF A WORKERS UNION", NOTES)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BRENDAN F. BOYLE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BRENDAN F. BOYLE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BRENDAN F. BOYLE", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Title, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INAPPROPRIATE METHODOLOGIES ", Title, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Title, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPRESENTATIVES MALONEY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REPRESENTATIVES MALONEY", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("REPRESENTATIVES MALONEY", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("(4)", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("(4)", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("(4)", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW YORK CONGRESSIONAL DELEGATION|SENATORS HILLARY RODHAM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW YORK CONGRESSIONAL DELEGATION|SENATORS HILLARY RODHAM", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW YORK CONGRESSIONAL DELEGATION|SENATORS HILLARY RODHAM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) 
  
   

  
  
  
  
  
  
  
  
  
  
  
  
}