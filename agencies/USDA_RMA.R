# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# last name only matched on 659 of 709 observations. Some aren't last names (e.g. CONGRESS) and 
# there is probably spelling errors and missed matches 


 file.name <- "USDA_RMA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m-%d-%y")
  data$DateSigned %<>% as.Date("%m-%d-%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###     ###     ###
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(",", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ",") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ","))
      
      data <- rbind(data, new)
      
    }
  }
  for(i in 1:nrow(data)){
    if(grepl("/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(",", data$FROM),] # removes orginal row with all data
  data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  ###     ###     ###
  
 
  
  # create variable for last name
  data$last_name <- formatLastName(data, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CLAIM|LATE PAYMENT|PREMIUM|PREVENTED PLANTING|FRAUD|ITS|ROTAT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CLAIM|LATE PAYMENT|PREMIUM|PREVENTED PLANTING|FRAUD|ITS||ROTAT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CLAIM|LATE PAYMENT|PREMIUM|PREVENTED PLANTING|FRAUD|ITS|ROTAT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("CLAIM", SUBJECT, ignore.case = TRUE), "THE RMA PROVIDES US FARMERS CROP INSURANCE, NOT SURE IF PROVIDING THIS TO FARMERS WOULD BE CONSIDERED INDIVIDUAL SERVICE OR COMMERCIAL SERVICE", NOTES)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOBACCO|WHEAT|COTTON|SPECIALTY CROP", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOBACCO|WHEAT|COTTON|SPECIALTY CROP", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("TOBACCO|WHEAT|COTTON|SPECIALTY CROP", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PLANTING DATE", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PLANTING DATE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BOARD NOMINATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BOARD NOMINATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("BOARD NOMINATION", SUBJECT, ignore.case = TRUE), "NOMINATION", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT PROGRAM|GRANT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT PROGRAM|GRANT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GRANT PROGRAM|GRANT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STUDY|REPORT|ARBITRATION|INSURANCE POLICY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STUDY|REPORT|ARBITRATION|INSURANCE POLICY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("STUDY|REPORT", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PRF|IOWA", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PRF|IOWA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FOIA", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FOIA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
    
  
  
  
  
  
  
}
