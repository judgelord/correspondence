# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOT_FHWA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% multidate( c("%m/%d/%y","%Y-%m-%d"))
  
  data %<>%
    mutate(tempDATE = str_extract(X7, "[0-9][0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9]/[0-9][0-9]|[0-9][0-9]/[0-9]/[0-9][0-9]")) 
  data$tempDATE %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = if_else(is.na(DATE), tempDATE, DATE))
  
  NoDate <- data %>%
    filter(is.na(DATE))
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- gsub("e'", "e" ,data$FROM)
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("United States Senate|Senate", Organization), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("U.S. House of Representatives|House|Representatives", Organization), "House", chamber)) #%>% 
  #mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  
  #Remove non members from dataset
  data$FROM <- gsub("Writer\\(s\\):( |$)|Writer/Editor: |Writers): |\\.$", "", data$FROM)
  
  #Format Typos
  data %<>%
    mutate(FROM = str_remove_all(FROM, " Jr.| JR.")) %>%
    mutate(FROM = str_replace(FROM, ",, ", ", "))
  
  #Name Format Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "YOUNG, C. W. BILL", "Charles YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "Schoch, P.E., Barry J", "Schoch, Barry")) %>%
    mutate(FROM = str_replace(FROM,"Prasad, P.E., Ananth", "Prasad, Ananth"))
    
  
  
  #String split for multiple member
  data %<>%
    mutate(FROM = str_replace(FROM, "( )(\\w)\\.", "\\1\\2")) %>%
    mutate(FROM = str_split(FROM, "\\.")) %>%
    unnest(FROM) %>%
    distinct()
  
  #data %<>% getFirstLast.Comma('FROM')
  
 
data <- extractMemberName(data, members, 'FROM')

  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, everything())
  
  # add errors
  data %<>%
    mutate(ERROR = ifelse(grepl("Jenna Maslyn", data$FROM), "Jenna Maslyn not in Congress", ERROR))
  


  
  unfoundnames <- data %>%
    filter(is.na(last_name))
  

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
  
  
  
  return(data)  
  
}
