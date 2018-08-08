# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DHS_HQ Anna" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data %<>% select(-n)
  
  data %<>% distinct() %>% 
    group_by(WF) %<>% mutate(nWF = n()) %>% ungroup() %>% 
    group_by(WF, TYPE) %<>% mutate(nTYPE = n()) %>% ungroup() %>% 
    filter(!(nWF>1 & nTYPE == 1 & is.na(TYPE))) # cut uncoded versions of duplicates
  
  data %<>% distinct() %>% 
    group_by(WF) %<>% mutate(nWF = n()) %>% ungroup() %>% 
    group_by(WF, POLICY_EVENT) %<>% mutate(nPE = n()) %>% ungroup() %>% 
    filter(!(nWF>1 & nPE == 1 & is.na(POLICY_EVENT))) # cut uncoded versions of duplicates
  
  data %<>% distinct() %>% 
    group_by(WF) %<>% mutate(nWF = n()) %>% ungroup() %>% 
    group_by(WF, NOTES) %<>% mutate(nNOTES = n()) %>% ungroup() %>% 
    filter(!(nWF>1 & nNOTES == 1 & is.na(NOTES))) # cut uncoded versions of duplicates
  
  data %<>% distinct() %>% 
    group_by(WF) %<>% mutate(nWF = n()) %>% ungroup() %>% 
    group_by(WF, ALT_TYPE) %<>% mutate(nALT_TYPE = n()) %>% ungroup() %>% 
    filter(!(nWF>1 & nALT_TYPE == 1 & is.na(ALT_TYPE))) # cut uncoded versions of duplicates
  
  data %<>% distinct() %>% 
    group_by(WF) %<>% mutate(nWF = n()) %>% ungroup() %>% 
    group_by(WF, CERTAINTY) %<>% mutate(nCERT = n()) %>% ungroup() %>% 
    filter(!(nWF>1 & nCERT == 1 & is.na(CERTAINTY))) # cut uncoded versions of duplicates
  
  data %<>% distinct()
  
  # potential_duplicates <- data %>% group_by(WF) %<>% mutate(n = n()) %<>% filter(n >1, !is.na(WF)) %>% arrange(WF) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
  #rename agency column
  colnames(data)[colnames(data) == 'AGENCY'] <- 'subagency'
  data %<>%
    mutate(agency = ifelse(is.na(subagency), 'DHS_HQ', paste("DHS_", subagency)))
  
  
  # Format date, year, Congress, member name etc.

  data$DATE[which(is.na(data$DATE))] <- as.Date(data$originalDATE[which(is.na(data$DATE))], "%m/%d/%Y") 

  #sum(is.na(data$DATE))
  
  
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  
  
  # chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Senator", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Congressman", FROM), "House", chamber)) 
  

  # fix FROM 
  data$FROM <- gsub("Senator |Congressman ", "", data$FROM)
  data$FROM <- gsub(",", ".", data$FROM)
  data$FROM <- gsub("Ti m |Tim ", "", data$FROM)
  data$FROM <- gsub("l l|ll", "", data$FROM)
  data$FROM <- gsub("Bun-", "Bun", data$FROM)
  data$FROM <- gsub("Ban-", "Ban", data$FROM)
  data$FROM <- gsub("C.Johnson", "C. Johnson", data$FROM)
  data$FROM <- gsub("y' ", "y ", data$FROM)
  data$FROM <- gsub("A1 ", "Al ", data$FROM)
  data$FROM <- gsub(" 1. ", " L. ", data$FROM)
  data$FROM <- gsub("Hany", "Harry", data$FROM)
  data$FROM <- gsub("John Abney Culberson", "John Culberson", data$FROM)

  # adds "ll" to names that were misread and other ocr errors
  data$FROM <- ocr.errors(data$FROM)
  # names 
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>%
    mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name)) %>%
    mutate(last_name = ifelse(grepl("M. Tia Johnson", FROM), "Johnson", last_name))
  
  
  # #create variable for first name of the Sen/Rep
  # data %<>%
  #   mutate(first_name = gsub(pattern = "(Congressman|Senator) (\\w+).*", replacement = "\\2", x=FROM)) %>% 
  #   mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
  #   mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name))
  # 
  # 
  # 
  # #create variable for last name of the Sen/Rep
  # data %<>%
  #   mutate(last_name = gsub(pattern = ".* (\\w+)$", 
  #                           replacement = "\\1", x=FROM)) %>% 
  #   mutate(last_name = ifelse(grepl("Jason Cha", FROM), "Chaffetz", last_name)) %>%
  #   mutate(last_name = ifelse(grepl("O'Rourke", FROM), "O'Rourke", last_name)) %>% 
  #   mutate(last_name = ifelse(is.na(title), NA, last_name))
  # 
  # data$last_name %<>% toupper()
  
  
  
  data %<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ON BEHALF OF CON|CONSTITUENT|CUSTOMER IS|SENTRI|PERMANENT RESIDENCE|REGARDING HER STATUS|REGARDING HIS STATUS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ON BEHALF OF CON|CONSTITUENT|CUSTOMER IS|SENTRI|PERMANENT RESIDENCE|REGARDING HER STATUS|REGARDING HIS STATUS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("FIRE DEPARTMENT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FIRE DEPARTMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("REQUESTS INFORMATION|HEARING|DELAYED PROCESSING|PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "5", TYPE))  %>% 
    mutate(CERTAINTY = ifelse(!grepl("[0-9]", CERTAINTY) & grepl("REQUESTS INFORMATION|HEARING|DELAYED PROCESSING|PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("REQUESTS INFORMATION", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("GRANT PROGRAM|GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse(!grepl("[0-9]", CERTAINTY) & grepl("GRANT PROGRAM|GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING", SUBJECT, ignore.case = TRUE), "HEARING", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANKS", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANKS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ADOPTION|REQUESTS STATUS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ADOPTION|REQUESTS STATUS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
}

