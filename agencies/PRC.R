# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Only last_name info and some state and chamber info
# 24 mismatches on last_name

#file.name <- "PRC" # for testing

clean <- function(file.name) {
  #  get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  data1 <- data
  data1$last_name <- formatLastName(data1, 'Last Name')
  data1$first_name <- formatFirstName(data1, 'First Name')
  
  data2 <- data
  #create last name variable for Sen/Rep
  data2 %<>%
    mutate(last_name = gsub(
      pattern = ".* |.*\\.",
      replacement = "",
      x = FROM
    ))
  data2$last_name <- formatLastName(data2, 'last_name')
  
  data <- left_join(data2, data1)
  
  
  # create agency column
  data$agency <- file.name
  
  #create year and congress columns
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen\\.|Sen |Senator |Sen,", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep\\.|Rep |Representative |Rep,", FROM), "House", chamber))
  
  # Format State
  data$State <- stateFromLower(data$State)
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data %<>% 
  mutate(SUBJECT = paste(SUBJECT,Sub_Issue)) %>%
  mutate(SUBJECT = paste(SUBJECT,Issue)) %>%
  mutate(SUBJECT = paste(SUBJECT, Category)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|WORKMEN'S|UNDELIVERED MAIL|REIMBURSEMENT|SATURDAY MAIL|DISABILITY|PASSPORT|LOBBY HOURS|FRAUD|MISSING MAIL|SERVICE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|WORKMEN'S|UNDELIVERED MAIL|REIMBURSEMENT|SATURDAY MAIL|DISABILITY|PASSPORT|LOBBY HOURS|FRAUD|MISSING MAIL|SERVICE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POST OFFICE CLOSING|PO CLOSING|POSSIBLE CLOSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POST OFFICE CLOSING|PO CLOSING|POSSIBLE CLOSING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("POST OFFICE CLOSING|PO CLOSING|POSSIBLE CLOSING", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("POST OFFICE CLOSING|PO CLOSING|POSSIBLE CLOSING", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VALASSIS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VALASSIS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("VALASSIS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TESTIMONY|HEARING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TESTIMONY|HEARING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("TESTIMONY|HEARING", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("APPEAL PROCESS|INFORMATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("APPEAL PROCESS|INFORMATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RATE CHANGE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RATE CHANGE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
    
  
  
    
    
    
    
    
    
  
  
} # end function
