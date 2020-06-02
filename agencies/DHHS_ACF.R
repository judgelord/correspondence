# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 84 mismatches on last_name

# file.name <- "DHHS_ACF Hope" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read()
    
  data$LetterID <- 1:nrow(data)
  
  # duplicate DOC ID rows were all invalid observations (removes 44 rows)
  # data <- data[-which(duplicated(data$'Doc ID', fromLast = TRUE)|duplicated(data$'Doc ID', fromLast = FALSE)),]

  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # Remove unwnated rows # CAN ERROR THESE OUT, BUT DON'T REMOVE THEM 
  # data <- data[-grep("^(B|1|Unknown, Unknown)$",data$FROM),]

      
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for full name
  #data <- getFirstLast.Comma(data, "FROM")
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Gillibrand. Kirsten E.", "Gillibrand, Kirsten E.")) %>%
    mutate(FROM = str_replace(FROM, "Butterfield, G. K.", "Butterfield, George")) %>%
    mutate(FROM = str_replace(FROM, "BONO Mack, MARY", "Bono, Mary")) %>%
    mutate(FROM = str_replace(FROM, "Ros-Lehtinen, lleana", "ROS-LEHTINEN, Ileana"))
    
 
    
  
  #Change from getFirstLast to extractMemberName
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>% mutate(SUBJECT = paste(SUBJECT, `Refd. To`, `Action Required`))
  
  # arrange columns for hand coding
  # data %<>% select(ID, DATE,  FROM, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ON BEHALF OF.*CONSTITUENT|CHILD SUPPORT|CHILD CUSTODY|CONSTITUENT|EMPLOYMENT|SEXUAL ASSAULT|PARENTAL RIGHTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ON BEHALF OF.*CONSTITUENT|CHILD SUPPORT|CHILD CUSTODY|CONSTITUENT|EMPLOYMENT|SEXUAL ASSAULT|PARENTAL RIGHTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT SUPPORT|PREP FOR SIG|SCHOOL|CITY OF|WEST VIRGINIA|OHIO|GRANT APPLICATION|GRANT LETTER|APPLICATION FOR FUNDING|CHILD CARE CENTER|CALIFORNIA|COUNTY|UNIV|CHURCH", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT SUPPORT|PREP FOR SIG|SCHOOL|CITY OF|WEST VIRGINIA|OHIO|GRANT APPLICATION|GRANT LETTER|APPLICATION FOR FUNDING|CHILD CARE CENTER|CALIFORNIA|COUNTY|UNIV|CHURCH", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("GRANT SUPPORT|PREP FOR SIG|SCHOOL|CITY OF|WEST VIRGINIA|OHIO|GRANT APPLICATION|GRANT LETTER|APPLICATION FOR FUNDING|COUNTY|UNIV|CHURCH", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TRANSITION PROCESS|INCREASING ADOPTION|OVERSIGHT|IMMEDIATE RELEASE|INTERAGENCY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TRANSITION PROCESS|INCREASING ADOPTION|OVERSIGHT|IMMEDIATE RELEASE|INTERAGENCY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("INCREASING ADOPTION", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("INVITATION", SUBJECT, ignore.case = TRUE), "INVITE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT|CONGRESSIONAL SUPPORT LETTER", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT|CONGRESSIONAL SUPPORT LETTER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEAD START", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEAD START", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("HEAD START", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE))
  
  data %<>%
    mutate(TYPE = ifelse(! str_detect(TYPE, "[0-9]") & str_detect(SUBJECT, "grant support"), 3, TYPE)) %>%
    mutate(TYPE = ifelse(! str_detect(TYPE, "[0-9]") & str_detect(SUBJECT, "Grant support"), 3, TYPE)) %>%
    mutate(TYPE = ifelse(! str_detect(TYPE, "[0-9]") & str_detect(SUBJECT, "Grant Support"), 3, TYPE)) %>%
    mutate(POLICY_EVENT = ifelse(str_detect(SUBJECT, "grant support"), "grant", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse(str_detect(SUBJECT, "Grant support"), "grant", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse(str_detect(SUBJECT, "Grant Support"), "grant", POLICY_EVENT))
  
  
  return(data)
}




