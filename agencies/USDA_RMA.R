# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "USDA_RMA" # for testing

#file.name <- "USDA_RMA" ##for testing 14 June 


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
 #create column that is converted version of DATE, new column is NEWDATE
  
  data$NEWDATE <- data$DATE %>% multidate(c("%m-%d-%y","%m/%d/%y"))
  
  # figuring out class for NEWDATE column
  # class(data$NEWDATE)
  
  
  
  #data$DATE %>% multidate(c("%m-%d-%y","%m/%d/%y"))
  
  #help(as.Date)
  
 data %<>% 
    mutate(DATE = ifelse(is.na(NEWDATE), DateSigned, DATE))  ##replacing NA dates with date signed
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% multidate(c("%m-%d-%y","%m/%d/%y"))  
  
  ##allow for different variations of dates for better matches
  
  

  
  
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
  data <- data[-grep(",", data$FROM),] # removes original row with all data
  data <- data[-grep("/", data$FROM),] # removes original row with all data
  ###     ###     ###
  
  data$FROM <- (gsub("& 12 Senators","",data$FROM)) # remove +
  data <- data[-grep("Senators", data$FROM),]
  
  # # Give first names to A. Green and G. Green
  # data$first_name <- ifelse(grepl("G. Green", data$FROM), "Gene", NA)
  # data$FROM <- gsub("G. Green", "Green", data$FROM)
  # data$first_name <- ifelse(grepl("A.\nGreen", data$FROM), "Alan", data$first_name)
  # data$FROM <- gsub("A.\nGreen", "Green", data$FROM)
  
  # create variable for last name
  data$last_name <- formatLastName(data, 'FROM')
  
  #data <- extractMemberName(data, members, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  # add ERROR notes
  data %<>%
    mutate(ERROR = ifelse(grepl("Congress", data$FROM), "Not valid name info", ERROR))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  
  #sample <- data %>%
  #filter(is.na(DATE))  
  #View(sample) 
  
  ##testing code
  
  
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
