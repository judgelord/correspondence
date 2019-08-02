# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FCC Devin" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

  # create ID variable
  #colnames(data)[colnames(data) == 'X1'] <- 'ID'

# Making ID column
data %<>% 
  rowid_to_column("ID")
  
  # create agency column
  data$agency <- file.name
  
  data$date <- data$DATE
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE)
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE)
  data$DATE <- gsub("-200", "-0", data$DATE)
  #data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))

  
  data$DATE %<>% multidate(c("%Y-%m-%d","%m/%d/%y"))
  
  #checking for dates that are NA
  
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %<>%
    mutate(party = ifelse(party == "GOP", "Republican",party)) %>% 
    mutate(party = ifelse(party == "DEM", "Democrat", party))
  
  #data <- getFirstLast.Comma(data, 'FROM')
  
  #change from getfirstlast to extractmembername
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # format state variable
  data$state <- stateFromLower(gsub(".*\\(.-|\\)","", data$FROM))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("USF SUBSIDIES", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("USF SUBSIDIES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("USF SUBSIDIES", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SERVICE COMPLAINTS|TELEMARKETING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SERVICE COMPLAINTS|TELEMARKETING", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SERVICE COMPLAINTS|TELEMARKETING", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ENFORCEMENT|LICENSING", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ENFORCEMENT|LICENSING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("LICENSING", SUBJECT, ignore.case = TRUE), "DECISION", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LEGISLATIVE|OPEN INTERNET|BUDGET", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LEGISLATIVE|OPEN INTERNET|BUDGET", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%   #come back to Incentive auction tomorrow
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DISABILITY ACCESSIBILITY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DISABILITY ACCESSIBILITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Privacy", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PRIVACY", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PRIVACY", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RF RADIATION", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%         #originally expected "radiation" to be policy related, but "spectrum" listed under the Strategic/Plan/Goal category led me to believe it is more likely type 2
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RF RADIATION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RF RADIATION", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NUMBERING", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NUMBERING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NUMBERING", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%   #figure businesses and individuals could be making requests about getting their number changed
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ALLOCATION", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ALLOCATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("ALLOCATION", SUBJECT, ignore.case = TRUE), "DECISION", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("AGENCY REFORM|PUBLIC INTEREST OBLIGATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("AGENCY REFORM|PUBLIC INTEREST OBLIGATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MERGER", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MERGER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EMERGENCY COMMUNICATIONS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EMERGENCY COMMUNICATIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("EMERGENCY COMMUNICATIONS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(NOTES = ifelse (!grepl("[A-Z]", NOTES) & grepl("EMERGENCY COMMUNICATIONS", SUBJECT, ignore.case = TRUE), "NOT SURE IF EMERGENCY ALERTS WOULD BE POLICY OR SOLELY FOR CONSTITUENT BENEFITS", NOTES)) %>%
  mutate(TYPE = ifelse (grepl("INCENTIVE AUCTIONS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (grepl("INCENTIVE AUCTIONS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (grepl("SERVICES", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (grepl("SERVICES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (grepl("SERVICES", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSUMER INFORMATION|HUMAN RESOURCES", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSUMER INFORMATION|HUMAN RESOURCES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CONSUMER INFORMATION|HUMAN RESOURCES", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(NOTES = ifelse (!grepl("[A-Z]", NOTES) & grepl("CONSUMER INFORMATION", SUBJECT, ignore.case = TRUE), "ASSUME THIS IS POLICY (WITH REGARDS TO CONSUMER INFORMATION) TO PROTECT CONSUMERS, BUT COULD ALSO BE FOR THE BENEFIT OF SPECIFIC MARKETING BUSINESSES I SUPPOSE", NOTES)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEDIA|DIVERSITY|OWNERSHIP|LOCALISM", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEDIA|DIVERSITY|OWNERSHIP|LOCALISM", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INDECENT PROGRAMMING|ACCESS CHARGES", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INDECENT PROGRAMMING|ACCESS CHARGES", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("911|PROGRAMMING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%   #"PUBLIC SAFETY LEADS ME TO BELIEVE THIS IS POLICY
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("911|PROGRAMMING", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9, A-Z]", ALT_TYPE) & grepl("911|PROGRAMMING", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BROADBAND COVERAGE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BROADBAND COVERAGE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BROADBAND COVERAGE", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INTERFERENCE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INTERFERENCE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
 
  
  

    
    
}

  
  
  
  

