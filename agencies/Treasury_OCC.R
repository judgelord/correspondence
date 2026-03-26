# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "Treasury_OCC" # for testing

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
  data$DATE <- data$`Date Received or Meeting Date` %>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  

  
  ## create chamber variable
   data %<>%
     mutate(chamber = ifelse(!is.na(Senator), "Senate", NA)) %>% 
     mutate(chamber = ifelse(!is.na(`House Member`), "House", chamber))
   
  data %<>% 
    mutate(FROM = Senator) %>% 
    mutate(FROM = ifelse(is.na(Senator), `House Member`, FROM))
  
  
  ###############    
  
  #String Split for Multiple Members
  data %<>%
    mutate(FROM = str_remove_all(FROM, ";#[0-9]+")) %>%
    mutate(FROM = str_remove_all(FROM, "#")) %>%
    mutate(FROM = str_split(FROM, ";")) %>%
    unnest(FROM)
  
  
  
  # Creates duplicate rows for lines with multiple representatives
  # for(i in 1:nrow(data)){
  #   if(grepl(";#\\d{+};#", data$FROM[i])) {
  # 
  #     new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";#\\d{1,3};#") + 1))
  #     new$FROM <- unlist(str_split(data$FROM[i], ";#\\d{1,3};#"))
  # 
  #     data <- rbind(data, new)
  # 
  #   }
  # }
  # 
  # 
  # data <- data[-grep(";#\\d{+};", data$FROM),] # removes original row with all data
  
  # create Letter ID variable
  data$LetterID <-  c(1:nrow(data))
  
  ################
  
  
  data$FROM <- gsub(", Jr.", ",", data$FROM)
  
 #create variable for first and last name
  
  #data <- getFirstLast.Comma(data, "FROM")
  #data$first_name <- formatFirstName(data, "first_name")
  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
#non-members of congress   
data %<>%
   mutate(ERROR = ifelse(str_detect(FROM, "Radewagen, Aumua Amata"), "Non Voting Member", ERROR)) %>%
   mutate(ERROR = ifelse(str_detect(FROM, "Plaskett, Stacey"), "Non Voting Member", ERROR))

#NOTES
data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Hastings, Doc"), "Wrong Congress", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Johnson, Tim"), "Wrong Congress", NOTES))

   #Failing observations
   unfoundnames <- data %>%
   filter(is.na(last_name),
          is.na(ERROR),
          is.na(NOTES))  
 
 unfoundnames %<>%
   select(ID, DATE, FROM, SUBJECT, last_name, everything())
 
 # THERE WAS A PROBLEM WITH BOOZMAN, BUT SEEMS TO BE FINE NOW 
 # data %>% filter(str_detect(FROM, regex("boozman", ignore_case = T))) %>% select(string, pattern)
 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  data %<>%
  mutate(SUBJECT = paste(SUBJECT, `Specific Subject`)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VOLCKER RULE|OUTREACH|RULE|MSB|MEETING|STANDARDS|RATIO|MARIJUANA|RECORD|TESTIFY|INVESTIGATION|FSOC|LCR|CEASE AND DESIST|TESTIMONY|MIKE FLYNN|FISMA|PURCHASE REQUIREMENT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VOLCKER RULE|OUTREACH|RULE|MSB|MEETING|STANDARDS|RATIO|MARIJUANA|RECORD|TESTIFY|INVESTIGATION|FSOC|LCR|CEASE AND DESIST|TESTIMONY|MIKE FLYNN|FISMA|PURCHASE REQUIREMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("VOLCKER RULE|RULE", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEARING|OPERATION CHOKE POINT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEARING|OPERATION CHOKE POINT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING", SUBJECT, ignore.case = TRUE), "HEARING", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("OPERATION CHOKE POINT|TESTIFY|INFORMATION", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|REMITTANCE|CLOSING OF BANK ACCOUNTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|REMITTANCE|DOCUMENT REQUEST|CLOSING OF BANK ACCOUNTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("WELLS FARGO|DOCUMENT REQUEST", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("WELLS FARGO", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("WELLS FARGO", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RISK MANAGEMENT|FINTECH|BANK OF TEXAS|BITCOIN", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RISK MANAGEMENT|FINTECH|BANK OF TEXAS|BITCOIN", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
  
return(data)  
  
  
  
}
