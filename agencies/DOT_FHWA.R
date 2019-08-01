# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOT_FHWA" # for testing


# Duplicates need to be addressed

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
  data$FROM <- gsub("e'", "e" ,data$FROM)
  
  
  # is this not what is done with data3?
  #data2 <- data[grepl("Writer",data$FROM),]
  #data <- data[!grepl("Writer",data$FROM),]
  
  data <- extractMemberName(data, members, 'FROM')
  
  #data2$FName <- gsub("Writer\\(s\\): |Writers\\):","",data2$FName)
  #data2 <- getFirstLast.Comma(data2, "FName")
  
  #data %<>% full_join(data2)
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("United States Senate|Senate", Organization), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("U.S. House of Representatives|House|Representatives", Organization), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  
  
  
  
  
  
  
  
  
  #################################################################################
  
  
  
  
  
  
  
  
  
# SECOND DATA SOURCE IS FORMATTED DIFFERENTLY
  data3 <- gs_title(paste(file.name, "2007-14")) %>% gs_read() %>% distinct() # get data
  
  # create ID variable
  data3$ID <- c((nrow(data)+1):(nrow(data)+nrow(data3)))
  
  #create agency column
  data3$agency <- file.name
  
   # Format date, year, Congress
  data3$DATE %<>% multidate( c("%m/%d/%y","%Y-%m-%d"))
  
  data3 %<>%
    mutate(tempDATE = str_extract(X7, "[0-9][0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9]/[0-9][0-9]|[0-9][0-9]/[0-9]/[0-9][0-9]")) 
  data3$tempDATE %<>% as.Date("%m/%d/%y")
  
  data3 %<>%
       mutate(DATE = if_else(is.na(DATE), tempDATE, DATE))
  
  NoDate <- data3 %>%
     filter(is.na(DATE))
  
  data3 %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data3 %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data3$FROM <- gsub("Writer\\(s\\):( |$)|Writer/Editor: |Writers): |\\.$", "", data3$FROM)
  #data3 <- data3[-which(data3$FROM==""),]
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  
   data3 %<>%
     mutate(FROM = str_replace(FROM, "( )(\\w)\\.", "\\1\\2")) %>%
     mutate(FROM = str_split(FROM, "\\.")) %>%
     unnest(FROM) %>%
    distinct()
  
  #Rewrote with tidy
  #  for(i in 1:nrow(data3)){
  #    if(grepl("\\w{3,}\\.", data3$FROM[i])) {
  #      
  #      data3$FROM <- gsub("( )(\\w)\\.", "\\1\\2", data3$FROM)
  #      new <- data3 %>% dplyr::slice(rep(i, each = str_count(data3$FROM[i], pattern = "\\.") + 1))
  #      new$FROM <- unlist(str_split(data3$FROM[i], "\\."))
  #      
  #      data3 <- rbind(data3, new)
  #      
  #    }
  #  }
  #  data3 <- data3[-grep("\\w{3,}\\.", data3$FROM),] # removes orginal row with all data
  #  data3$FROM <- gsub("^ |^  | $|  $", "", data3$FROM)
  #  data3 <- data3[!data3$FROM == "",] # removes blank observations
  #  
  # ################
  # data$FROM <- gsub("([a-z]{3})[A-Z]", '\\1', data$FROM)
  
  data3 %<>% getFirstLast.Comma('FROM')
  
  #data3 <- extractMemberName(data3, members, 'FROM') # getFirstLast seems to work better, but there are a lot of non-members and bad OCR
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # merge 2007-2014 with 2015-2017
  data %<>%
    mutate(ERROR =  as.character(ERROR) ) %>%
    full_join(data3 %>% mutate(ERROR =  as.character(ERROR)))
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, everything())
  
  # add errors
  data %<>%
    mutate(ERROR = ifelse(grepl("Jenna Maslyn", data$FROM), "Jenna Maslyn not in Congress", ERROR))
  
  Unfoundnames <- data3 %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  
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
