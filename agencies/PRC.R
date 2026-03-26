# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Only last_name info and some state and chamber info
# 24 mismatches on last_name 
#testing on 13 June

#file.name <- "PRC" 

clean <- function(file.name) {
  #  get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  

    
  
  
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
  

  
  data$FROM %<>% 
    str_replace("\\.", " ") %>% 
    str_squish() %>% 
    str_replace("Sen ", "Senator ") %>% 
    str_replace("Rep ", "Representative ") 
  
  data %<>%
    mutate(last_name = str_extract(FROM, " .*")) %>% 
    mutate(title = str_extract(FROM, ".*")) 
  
  data$last_name <- formatLastName(data, col_name = "last_name")
  
  # inspect
  paste(data$last_name, data$FROM)
  
  data %<>% 
    mutate(FROM = paste(title, last_name) ) 
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
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
    
  
  
    
    
#sample <- data %>%
#filter(is.na(first_name))  
#View(sample)    

##checking code
    
    
  
return(data)  
  
} # end function

