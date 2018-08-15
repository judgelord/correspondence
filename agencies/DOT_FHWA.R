# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# file.name <- "DOT_FHWA" # for testing

clean <- function(file.name) {
  # DOT_FHWA 
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE <- data$CompletedDate %>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- paste(data$FName, " ", data$LName )
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("United States Senate|Senate", Organization), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("U.S. House of Representatives|House|Representatives", Organization), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  
  
  
  
  
  
  
  
  
  #################################################################################
  
  
  
  
  
  
  
  
  
# SECOND DATA SOURCE IS FORMATTED DIFFERENTLY
  data2 <- gs_title(paste(file.name, "2007-14")) %>% gs_read() # get data
  
  # create ID variable
  data2$ID <- c(nrow(data):nrow(data)+nrow(data2))
  
  #create agency column
  data2$agency <- file.name
  
  # Format date, year, Congress
  data2$DATE <- data2$DATE %>% as.Date("%Y/%m/%d")
  data2 %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data2 %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data2$FROM <- gsub("Writer\\(s\\):( |$)|Writer/Editor: |Writers): |\\.$", "", data2$FROM)
  data2 <- data2[-which(data2$FROM==""),]
  
  data2 %<>% getFirstLast.Comma('FROM')
  # data2 <- extractMemberName(data2, members, 'FROM') # getFirstLast seems to work better, but there are a lot of non-members and bad OCR
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # merge 2007-2014 with 2015-2017
  data %<>% full_join(data2)
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, everything())
  
  # apply coding rules
  data%<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY|COOK COUNTY|SMART CITY|CITY OF DETROIT|JANUARY 13|ST. CHARLES", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY|COOK COUNTY|SMART CITY|CITY OF DETROIT|JANUARY 13|ST. CHARLES", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT,|HIS|STATUS UPDATE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT,|HIS|STATUS UPDATE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POLIC|VOTING|MOTORCYCLIST ADVISORY|BUY AMERICA WAIVERS|LEGACY INFORMATION|INFRASTRUCTURE PACKAGE|REPORT TO CONGRESS|1664|EXPRESSING CONCERN|OPPOSE|ZERO EMISSIONS|URGING THE COMPLETION|DEPARTMENT REVERSE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POLIC|VOTING|MOTORCYCLIST ADVISORY|BUY AMERICA WAIVERS|LEGACY INFORMATION|INFRASTRUCTURE PACKAGE|REPORT TO CONGRESS|1664|EXPRESSING CONCERN|OPPOSE|ZERO EMISSIONS|URGING THE COMPLETION|DEPARTMENT REVERSE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RTC", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RTC", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RTC", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("REPORT TO CONGRESS", SUBJECT, ignore.case = TRUE), "REPORT", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(EVENT_NAME = ifelse (!grepl("[A-Z]", EVENT_NAME) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "EARMARK (I THINK?)", EVENT_NAME)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) 
  
  
  
  
  
  
  
  
  
  
  
  
  
}