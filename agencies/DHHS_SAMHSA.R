# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 2000+ non matches, but most shouldn't be matching. 

#file.name <- "DHHS_SAMHSA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name
  
  # # Format date, year, Congress, member name etc. 
  # data$DATE %<>% as.Date("%m/%d/%Y")
  # 
  # 
  # #create year and congress columns
  # data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  # data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for full name
  
  #data$FROM <- data$X3
  data <- getFirstLast.Comma(data, "FROM")
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, FROM, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  data %<>%
  mutate(SUBJECT = paste(SUBJECT,`Refd. To`)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|SON'S ADDICTION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|SON'S ADDICTION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT SUPPORT LETTER|GRANT APPLICATION|GRANT SUPPORT|SUPPORT FOR FUNDING|WRITTEN IN SUPPORT OF .*APPLICATION|SEEKING FUNDING|WRITES IN SUPPORT OF.*APPLICATION|SUPPORTS APPLICATION FROM|GRANT REQUEST|RE-GRANT|REQUEST FOR FUNDING", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT SUPPORT LETTER|GRANT APPLICATION|GRANT SUPPORT|SUPPORT FOR FUNDING|WRITTEN IN SUPPORT OF.*APPLICATION|SEEKING FUNDING|WRITES IN SUPPORT OF .*APPLICATION|SUPPORTS APPLICATION FROM|GRANT REQUEST|RE-GRANT|REQUEST FOR FUNDING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("GRANT SUPPORT LETTER|GRANT APPLICATION|GRANT SUPPORT|SUPPORT FOR FUNDING|WRITTEN IN SUPPORT OF .*APPLICATION|SEEKING FUNDING|WRITES IN SUPPORT OF .*APPLICATION|SUPPORTS APPLICATION FROM|GRANT REQUEST|RE-GRANT|REQUEST FOR FUNDING", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "THANK YOU", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SUPPORT LETTER", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%   
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SUPPORT LETTER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REGULATIONS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REGULATIONS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("REGULATIONS", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "INVITE", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("MEETING", SUBJECT, ignore.case = TRUE), "MEETING", POLICY_EVENT)) 
  
  
  
  
  
  
  
  
  
  
  
  return(data)
  
  
}






