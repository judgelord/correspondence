# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# 170 (out of 4363) not matching, go back and fix

#file.name <- "DHHS_HRSA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  #data$DATE %<>% as.Date("%B %d, %Y")
  data$DATE <- multidate(data$DATE, c("%B %d, %Y", "%m/%d/%y"))

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, " and |;|, Sen(\\.| )|, Rep(\\.| )")) %>% 
    unnest()
  
  data$ID <- 1:nrow(data)
  
  # create variable for first and last name
  data %<>% extractMemberName(members, "FROM")
  
  data %<>% 
    mutate(ERROR = ifelse(FROM == "", "blank", ERROR), 
           ERROR = ifelse(FROM %in% c("Senate HELP Committee",
                                      "Senate HELP Committee - Bipartisan Staff",
                                      "House E&C Committee",
                                      "Senate HELP Subcommittee on Children and Families",
                                      "House Ways and Means Committee",
                                      "Senate Committee on Finance"), 
                          "committee",
                          ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data %<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT CONCERNS|NOMINATION|(6)", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT CONCERNS|NOMINATION|(6)", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STATE UNIVERSITY|UNIVERSITY OF|UNIVERSITY'S", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STATE UNIVERSITY|UNIVERSITY OF|UNIVERSITY'S", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("UNIVERSITY OF", SUBJECT, ignore.case = TRUE), "WOULD BE #2 IF ANY OF THESE ARE PRIVATE SCHOOLS, DON'T THINK THEY ARE THOUGH", NOTES)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT APPLICATION.*INC.|HOSPITAL.*INC.", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT APPLICATION.*INC.|HOSPITAL.*INC", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FIRST CHOICE HEALTH CENTERS|FAMILY HEALTHCARE|COLLEGE|HEALTH SERVICES|HEALTH CENTER|MEDICAL CENTER", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FIRST CHOICE HEALTH CENTERS|FAMILY HEALTHCARE|COLLEGE|HEALTH SERVICES|HEALTH CENTER|MEDICAL CENTER", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("FIRST CHOICE HEALTH CENTERS|FAMILY HEALTHCARE|COLLEGE|HEALTH SERVICES|HEALTH CENTER|MEDICAL CENTER", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
    mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("FIRST CHOICE HEALTH CENTERS", SUBJECT, ignore.case = TRUE), "LIKE 95% SURE THIS IS A NON-PROFIT, BUT COULD NOT FIND DEFINITIVE EVIDENCE", NOTES)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HUDSON HEADWATERS|JEWISH|NEW ACCESS POINTS|MICHIGAN PRIMARY CARE|DECKER|HARLEM UNITED|SUPPORT.*PUBLIC HEALTH|SUPPORT.*REGIONAL|SUPPORT.*COUNTY|WAKEMED|SUPPORT OF.*COMMUNITY|WEST VIRGINIA|COMMUNITY HEALTH CENTER|RURAL HEALTH CARE|NURSE ASSOCIATION|CITY OF|GRANT APPLICATION|FAMILY CARE CENTER", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HUDSON HEADWATERS|JEWISH|NEW ACCESS POINTS|MICHIGAN PRIMARY CARE|DECKER|HARLEM UNITED|SUPPORT.*PUBLIC HEALTH|SUPPORT.*REGIONAL|SUPPORT.*COUNTY|WAKEMED|SUPPORT OF.*COMMUNITY|WEST VIRGINIA|COMMUNITY HEALTH CENTER|RURAL HEALTH CARE|NURSE ASSOCIATION|CITY OF|GRANT APPLICATION|FAMILY CARE CENTER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROPOSED RULE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROPOSED RULE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  return(data)
}