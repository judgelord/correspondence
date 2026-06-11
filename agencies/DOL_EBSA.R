# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# file.name <- "DOL_EBSA" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%Y-%m-%d")
  # bad <- data %>% filter(is.na(DATE)) %>% .$LetterID
  # data %>% filter(LetterID %in% bad) %>% .$DATE %>% unique()
  # data %>% select(LetterID, DATE) %>% filter(LetterID %in% bad)
  # 
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Butterfield, G.K.", "BUTTERFIELD, George Kenneth, Jr. (G.K.)")) %>%
    mutate(FROM = str_replace(FROM, "Sensenbrenner, Jr., F. James", "SENSENBRENNER, Frank James, Jr.")) %>%
    mutate(FROM = str_replace(FROM, "Bono Mack, Mary", "BONO, Mary")) %>%
    mutate(FROM = str_replace(FROM, "Young, C.W. Bill", "YOUNG, Charles William (Bill)")) %>%
    mutate(FROM = str_replace(FROM, "Johnson \\(Il\\), Rep. Timothy", "JOHNSON, Timothy V.")) 
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  # bad <- data %>% filter(is.na(last_name)) %>% .$LetterID
  # data %>% filter(LetterID %in% bad) %>% .$FROM %>% str_squish() %>% unique()
  # data  %>% filter(LetterID %in% bad) %>% select(congress, FROM) %>% distinct() %>% kable()
  
  data %<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DISABILITY|PARTICIPANT RIGHTS|SPOUSE RIGHTS|ENROLLMENT RIGHTS|PARTICIPANTS|ELIGIBILITY|NOTICE|NOTICES|BENEFIT REDUCTION|ENTITLEMENT|ISSUE|ISSUES|PARTICIPANT|PARTICIPANTS|PARTICIPATION|PAYMENT|PAYMENTS|FACTS|HEALTH|SPOUSE BENEFIT|LOCATE PLAN|CALCULATION|PREMIUMS|LIFE|NO FUNDS|DURATION|COVERED WELFARE|COORDINATION|ABANDONED PLAN|BENEFIT LIMITATIONS|EARLY RETIREMENT|SEVERANCE PAY|BENEFIT DISTRIBUTIONS|TERMINATIONS|REQUEST|CONDITION|CONDITIONS|SURVIVOR|NON-COVERED PLANS|TRANSACTION|TRANSACTIONS|RECOVERY|FMLA|MEDICARE|DOMESTIC RELATIONS|INVESTMENT|VACATION|SUCCESSOR PLANS|CONFLICT OF INTEREST|DEPENDANTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DISABILITY|PARTICIPANT RIGHTS|SPOUSE RIGHTS|ENROLLMENT RIGHTS|PARTICIPANTS|ELIGIBILITY|NOTICE|NOTICES|BENEFIT REDUCTION|ENTITLEMENT|ISSUE|ISSUES|PARTICIPANT|PARTICIPANTS|PARTICIPATION|PAYMENT|PAYMENTS|FACTS|HEALTH|SPOUSE BENEFIT|LOCATE PLAN|CALCULATION|PREMIUMS|LIFE|NO FUNDS|DURATION|COVERED WELFARE|COORDINATION|ABANDONED PLAN|BENEFIT LIMITATIONS|EARLY RETIREMENT|SEVERANCE PAY|BENEFIT DISTRIBUTIONS|TERMINATIONS|REQUEST|CONDITION|CONDITIONS|SURVIVOR|NON-COVERED PLANS|TRANSACTION|TRANSACTIONS|RECOVERY|FMLA|MEDICARE|DOMESTIC RELATIONS|INVESTMENT|VACATION|SUCCESSOR PLANS|CONFLICT OF INTEREST|DEPENDANTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))  %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PENSION REFORM ACT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PENSION REFORM ACT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(EVENT_NAME = ifelse (!grepl("[A-Z]", TYPE) & grepl("PENSION REFORM ACT", SUBJECT, ignore.case = TRUE), "LEGISLATION", EVENT_NAME)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OTHER", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OTHER", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("OTHER", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INDIVIDUAL POLICY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INDIVIDUAL POLICY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("INDIVIDUAL POLICY", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ERISA|REQUIREMENTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ERISA|REQUIREMENTS", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ERISA|REQUIREMENTS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNDER 20 EMPLOYEES|BANKRUPTCY", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNDER 20 EMPLOYEES|BANKRUPTCY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("UNDER 20 EMPLOYEES|BANKRUPTCY", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROTECTIONS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROTECTIONS", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROTECTIONS", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("PROTECTIONS", SUBJECT, ignore.case = TRUE), "LEGISLATION", EVENT_NAME)) 
  
return(data)  
}