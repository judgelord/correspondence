# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Finished. Matched perfectly on last_name

#file.name <- "DOI_BSEE" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
 
  
  data$FROM <- data$`Congressional Office`
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # add in notes if a number of unspecified congressman contributed
  data %<>%
    mutate(NOTES = ifelse(grepl("other", FROM), paste(NOTES, FROM), NOTES))
  
  
  ###     ###     ###
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  ###     ###     ###
  
  # create variable for last name
  data <- extractMemberName(data, members, 'FROM')
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Sen.", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Rep.", FROM), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  data %<>% filter(!is.na(FROM))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BOP RULE|DECOMMISSIONING|INVESTIGATION|QUESTIONS|BLACK|HYDR|COMMENTS|LEAKS|OIL AND GAS|NTL|BSEE|DOI|CERTIFICATION|STATUS|SAFETY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BOP RULE|DECOMMISSIONING|INVESTIGATION|QUESTIONS|BLACK|HYDR|COMMENTS|LEAKS|OIL AND GAS|NTL|BSEE|DOI|CERTIFICATION|STATUS|SAFETY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("BOP RULE", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("DECOMMISSIONING", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DRILLING|SHELL|TAYLOR ENERGY|MUSEUM", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DRILLING|SHELL|TAYLOR ENERGY|MUSEUM", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DRILLING|SHELL|TAYLOR ENERGY|MUSEUM", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY|A&M", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY|A&M", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("APPROVAL OF BP|FIELDWOOD", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("APPROVAL OF BP|FIELDWOOD", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GLOBAL ENERGY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GLOBAL ENERGY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GLOBAL ENERGY", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE))
  
  
  
  
  
  return(data)
  
  
}
