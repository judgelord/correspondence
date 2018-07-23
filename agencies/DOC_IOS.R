# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 342 out of 441 matches on last_name. Go back and fix spelling

#file.name <- "DOC_IOS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # creat variable for first and last name
  data <- extractMemberName(data, members, 'FROM')
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("IN FICON|POTENTIAL CLOSING", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("IN FICON|POTENTIAL CLOSING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|IN SUPPORT OF.*6|RESUME|RECOMMEND.*6|REAPPOINT|NOMINAT|RECOMMEND|STRONGLY|POTENTIAL HIRING|CANDIDACY|FISHERMAN|APPOINTMENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|IN SUPPORT OF.*6|RESUME|RECOMMEND.*6|REAPPOINT|NOMINAT|RECOMMEND|STRONGLY|POTENTIAL HIRING|CANDIDACY|FISHERMAN|APPOINTMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK|GRATITUDE", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK|GRATITUDE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("IN SUPPORT OF.*APPLICATION|SYRACUSE|COMMUNITIES|LTR.*GRANT APPLICATION|SUPPORTING THE APPLICATION|CITY OF|COUNTY|COMMUNITY DEVELOPMENT|SUPPORT FOR THE.*GRANT|SUPPORT FOR.*INSTITUTE|UNIVERSITY|APPLICATION|INTERNATIONAL INDUSTRY|BTOP ENDORSEMENT|TED STRICKLAND|RHODE ISLAND|LTR IN SUPPORT|GRANT|MINNESOTA", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("IN SUPPORT OF.*APPLICATION|SYRACUSE|COMMUNITIES|LTR.*GRANT APPLICATION|SUPPORTING THE APPLICATION|CITY OF|COUNTY|COMMUNITY DEVELOPMENT|SUPPORT FOR THE .*GRANT|SUPPORT FOR.*INSTITUTE|UNIVERSITY|APPLICATION|INTERNATIONAL INDUSTRY|BTOP ENDORSEMENT|TED STRICKLAND|RHODE ISLAND|LTR IN SUPPORT|GRANT|MINNESOTA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("IN SUPPORT OF .*APPLICATION|SYRACUSE|LTR.*GRANT APPLICATION|SUPPORTING THE APPLICATION|CITY OF|COUNTY|COMMUNITY DEVELOPMENT|SUPPORT FOR THE .*GRANT|SUPPORT FOR THE.*INSTITUTE|UNIVERSITY|APPLICATION|INTERNATIONAL INDUSTRY|TED STRICKLAND|RHODE ISLAND|LTR IN SUPPORT|GRANT|MINNESOTA", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DOC TO|URGE THE|URGES|REQUEST THE DOC|N ALL|DOC SHOULD|REGULATION|LTR URGING THE|DUMPING|ASK THAT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DOC TO|URGE THE|URGES|REQUEST THE DOC|N ALL|DOC SHOULD|REGULATION|LTR URGING THE|DUMPING|ASK THAT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("DOC TO|URGE THE|URGES|REQUEST THE DOC|N ALL|DOC SHOULD|REGULATION|AMERICAN RECOVERY|LTR URGING THE|DUMPING|ASK THAT", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OPPOSE|EXPRESSING CONCERN|AMERICAN RECOVERY|FREE TRADE|JAPAN|DISASTER", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OPPOSE|EXPRESSING CONCERN|AMERICAN RECOVERY|FREE TRADE|JAPAN|DISASTER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("OPPOSE|FREE TRADE|JAPAN|DISASTER", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMERCIAL FISHERY|SHOEMAKING JOBS", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMERCIAL FISHERY|SHOEMAKING JOBS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("COMMERCIAL FISHERY|DECISION|SHOEMAKING JOBS", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("WESTELCOM|SPECIES PARTS|CELLULAR BIOENGINEERING", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("WESTELCOM|SPECIES PARTS|CELLULAR BIOENGINEERING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DECISION|DRAFT TEXT|COMMENTS ON DRAFT|PASSED|COMMITTEE|EXTENSION|GAO REPORT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DECISION|DRAFT TEXT|COMMENTS ON DRAFT|PASSED|COMMITTEE|EXTENSION|GAO REPORT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("DRAFT TEXT|COMMENTS ON DRAFT|PASSED|GAO REPORT", SUBJECT, ignore.case = TRUE), "LEGISLATION", POLICY_EVENT))
  
  
  
  
  
  
}
