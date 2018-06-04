# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# 284 (out of 4363) not matching, go back and fix

#file.name <- "DHHS_HRSA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create ID variable 
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%B %d, %Y")

  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(" and | Sen | Rep | Sen.| Rep.", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = " and | Sen | Rep | Sen.| Rep.") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], " and | Sen | Rep | Sen.| Rep."))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(" and | Sen | Rep | Sen.| Rep.", data$FROM),] # removes orginal row with all data
  ################
  # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")

  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
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
  
  
  
  
  
  
  
  
  
  
  
  
}