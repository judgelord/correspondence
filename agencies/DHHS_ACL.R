# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Completed Matching on last_name

# file.name <- "DHHS_ACL" # for testing

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
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Lujan Grishman", "Michelle Lujan Grishman"))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
data %<>% 
    mutate(FROM = str_split(FROM, ",")) %>% 
    unnest(FROM)
  
  #create ID variable 
  data$ID <- c(1:nrow(data))
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", FROM), "House", chamber)) 
  
  # create variable for state
  data %<>%
    mutate(state = gsub(".*\\((\\w{2})\\).*", "\\1", data$FROM))
  data$state <- stateFromLower(data$state)
  
  
  # create variable for first and last name
  data <- extractMemberName(data,members,'FROM')
  

  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES))  
    
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BOSTON-HARVARD|WRITES IN SUPPORT OF|PUBLIC HEALTH SERVICE|HOLOCAUST|WRITING TO SUPPORT|SAMOA|MISSOURI", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BOSTON-HARVARD|WRITES IN SUPPORT OF|PUBLIC HEALTH SERVICE|HOLOCAUST|WRITING TO SUPPORT|SAMOA|MISSOURI", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "MEETING", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FAMILY CAREGIVERS|REQUESTED", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FAMILY CAREGIVERS|REQUESTED", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("FAMILY CAREGIVERS|REQUESTED", SUBJECT, ignore.case = TRUE), "LEGISLATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DEVELOPMENT DISABILITIES", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DEVELOPMENT DISABILITIES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DEVELOPMENT DISABILITIES", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OAA NUTRITION|AGING NETWORK", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OAA NUTRITION|AGING NETWORK", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("OAA NUTRITION|AGING NETWORK", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NIDILLR", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NIDILLR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NIDILLR", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) 
  
  
  
  return(data)
  
}