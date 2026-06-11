# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 76 out of 757 non matches on last_name. GO back to fix spelling and other errors.

# file.name <- "DOD_DFAS" # for testing


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
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", FROM, ignore.case = TRUE), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", FROM, ignore.case = TRUE), "House", chamber)) 

  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  data %<>%
    mutate(ERROR = ifelse(grepl('^None$',FROM, ignore.case = T), 'None provided in FROM column', ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber,  everything())
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MILITARY PAY|CIVILIAN PAY|TRAVEL PAY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MILITARY PAY|CIVILIAN PAY|TRAVEL PAY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMERCIAL PAY", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMERCIAL PAY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  return(data) 
  

  }
