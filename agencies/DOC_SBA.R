# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOC_SBA" # for testing


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
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y") # FIXME THERE ARE OTHER DATE FORMATS IN FROM (MAY NEED TO BE FIXED BY HAND)

  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # creat variable for first and last name
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data%<>%
  mutate(SUBJECT=paste(`LeadOffice`,"-",SUBJECT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CAPITAL ACCESS|GOVERNMENT CONTRACTING|DISASTER ASSISTANCE|BUSINESS DEVELOPMENT|ENTREPRENEURIAL DEVELOPMENT|HUBZONE|CONGRESSIONAL AND LEGISLATIVE|CONGRESSIONAL AFFAIRS|OFFICE OF CHIEF|INVESTMENT", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CAPITAL ACCESS|GOVERNMENT CONTRACTING|DISASTER ASSISTANCE|BUSINESS DEVELOPMENT|ENTREPRENEURIAL DEVELOPMENT|HUBZONE|CONGRESSIONAL AND LEGISLATIVE|CONGRESSIONAL AFFAIRS|OFFICE OF CHIEF|INVESTMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU|ADMINISTRATOR", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU|ADMINISTRATOR", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POLICY PLANNING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU|ADMINISTRATOR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) 

  return(data)  
}
