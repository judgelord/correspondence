# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# 1204 out of 1316 matches. I think all non-matches are non-members after checking, should be good. 
# Complete

#file.name <- "DHHS_CDC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  

  
  #Create LetterID
  data %<>%
    rename(LetterID = ID)
  
  #Fix duplication
  data %<>%
    select(-c(FolderID, LetterID, Action)) %<>% distinct()
 
   data %<>%
    group_by(FROM, SUBJECT, DATE) %>%
    mutate(n = n(),
           addressees = str_c(Addressee, collapse = "; ")) %>%
    arrange(-n) %>%
    select(-Addressee) %>%
     ungroup() %>%
    distinct()
  
   data %<>%
     group_by(FROM, SUBJECT, DATE) %>%
     mutate(n = n(),
            Prorities = str_c(Prority, collapse = "; "),
            Titles = str_c(Title, collapse = "; "),
            AnswerDates = str_c(Answer.Date, collapse = "; "),
            CloseDates = str_c(Close.Date, collapse = "; "),
            ProgramDueDates = str_c(Program.Due.Date, collapse = "; "),
            ReceiveDates = str_c(Receive.Date, collapse = "; ")) %>%
     arrange(-n) %>%
     select(-Prority, -Title, -Answer.Date, -Close.Date, -Program.Due.Date, -Receive.Date) %>%
     ungroup() %>%
     distinct()
  
    data %<>%
     group_by(FROM, SUBJECT, DATE) %>%
     mutate(n = n())%>%
     arrange(-n, FROM) %>%
     ungroup()
     
  #Fix Duplicate ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")

  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
 
   # data2 %<>%
  #   mutate(first_name = ifelse(FROM=="(b)(6)", first_name, NA)) %>%
  #   mutate(last_name = ifelse(FROM=="(b)(6)", last_name, NA))
  
  #data %<>%
  #mutate(first_name = ifelse(data$FROM =="(b)(6)", data2$first_name  , data$first_name  )) %>% 
  #mutate(last_name = ifelse(data$FROM == "(b)(6)", data2$last_name , data$last_name))
  
  
  #Comments errors for CDC Director and HHS Secretary  
  data %<>%
    mutate(ERROR = ifelse(grepl('^Director, CDC$',data$FROM), 'Director, CDC', ERROR ))
  
  data %<>%
    mutate(ERROR = ifelse(grepl('^HHS, Secretary',data$FROM), 'HHS, Secretary', ERROR ))
  
  #Filters out unwanted observations
  data %<>%
    filter( ! str_detect(FROM, "CDC, Director|Director, CDC|Director, DO NOT USE CDC|Director, DO NOT USE THIS ONE CDC")) %<>%
    filter ( ! str_detect(FROM, "HHS, Secretary")) %<>%
    filter( ! str_detect(FROM, "President, of the United States"))
  
   # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")
  
  #Checks how many members are not captured
  FROMunamed <- data %>%
    filter(is.na(last_name))
  
  #Create sample for all of the NA names and extract names from 'Title' into dataset
  Unfoundnames <- data %>%
    filter(is.na(last_name)) %>%
    extractMemberName(members, 'Title')
  
  #Create sample for all of the NA names and extract names from 'SUBJECT' into dataset
  Unfoundnames2 <- Unfoundnames %>%
    filter(is.na(last_name)) %>%
    extractMemberName(members, 'SUBJECT') %>%
    drop_na(last_name)
  
  Unfoundnames %<>%
    drop_na(last_name)
 
 #Drops duplicate observations  
Unfoundnames %<>%
   filter( ! str_detect(first_last, "\\(B\\)\\(6\\) \\(B\\)\\(6\\)"))
  
Unfoundnames2 %<>%
   filter( ! str_detect(first_last, "\\(B\\)\\(6\\) \\(B\\)\\(6\\)"))
  
data %<>%
  filter( ! str_detect(first_last, "\\(B\\)\\(6\\) \\(B\\)\\(6\\)"))
  
  
#Rejoin data that pulls authors from FROM, Title & SUBJECT
  data %<>%
    full_join(Unfoundnames)
  
  data %<>%
    full_join(Unfoundnames2)
  
  
  #Filter for observations with un-named authors
  otherauthors <- data %>%
    filter(str_detect(FROM, "others|et al"))
  
  #Make note of all observations with un-named authors
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "others|et al"), "multiple unnamed authors", NOTES))
    
data %<>%
  mutate(NOTES = ifelse(str_detect(SUBJECT, "others"), "multiple unnamed authors", NOTES))



#Check (b)(6) removals are correct
Nab6<- data %>%
  filter(str_detect(first_last, "\\(B\\)\\(6\\) \\(B\\)\\(6\\)"))


  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|TUBERCULOSIS. URGE|STEPS TO A HEALTHIER|PREVENTING SECONDARY|HEPATITIS FUNDING|SECTION 317|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|TUBERCULOSIS. URGE|STEPS TO A HEALTHER|PREVENTING SECRETARY|HEPATITIS FUNDING|SECTION 317|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|TUBERCULOSIS. URGE|STEPS TO A HEALTHER|PREFENTING SECONDARY|HEPATITIS FUNDING|SECTION 317", SUBJECT, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEPATITIS|INFECTIOUS DISEASE OUTBREAK", Title, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEPATITIS|INFECTIOUS DISEASE OUTBERAK", Title, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEPATITIS", Title, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU NOTE|VIRUS ANTIBODY", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU NOTE|VIRUS ANTIBODY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]",CERTAINTY) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW MEXICO'S NATIONAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DIACETYL|ANTIBIOTICS IN|OPIOID SENATORS|IMMIGRANT SEEKING ADJUSTMENT STATUS |UNITED STATES BY ILLEGAL IMMIGRANTS|REQUESTING A BRIEFING|CHRONIC OBSTRUCTIVE|NUCLEAR WEAPONS PLANT|BEHI|RECONSIDER THE DECISION|WRITING IN TO EXPRESS|CDC DIRECTOR AND THE FDA|RULE TO REMOVE|H5N1|DRS. F", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DIACETYL|ANTIBIOTICS IN|OPIOID SENATORS|IMMIGRANT SEEKING ADJUSTMENT STATUS|UNITED STATES BY ILLEGAL IMMIGRANTS|REQUESTING A BRIEFING|CHRONIC OBSTRUCTIVE|NUCLEAR WEAPONS PLANT|BEHI|RECONSIDER THE DECISION|WRITING IN TO EXPRESS|CDC DIRECTOR AND THE FDA|RULE TO REMOVE|H5N1|DRS. F", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("DIACETYL|ANTIBIOTICS IN|IMMIGRANT SEEKING ADJUSTMENT STATUS|SENATORS JOHN KERRY", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKY FLATS PLANT|CDC YOUTH", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%   #ON BEHALF OF UNION WORKERS
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKY FLATS PLANT|CDC YOUTH", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%  
  mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("ROCKY FLATS PLANT", SUBJECT, ignore.case = TRUE), "PRETTY SURE THIS IS ON BEHALF OF A WORKERS UNION", NOTES)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NCEH-ATSDR", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NCEH-ATSDR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NCEH-ATSDR", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Title, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INAPPROPRIATE METHODOLOGIES ", Title, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Title, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("WORLD TRADE CENTER NATIONAL RESPONDER PROGRAM", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("WORLD TRADE CENTER NATIONAL RESPONDER PROGRAM", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("WORLD TRADE CENTER NATIONAL RESPONDER PROGRAM", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("(4)", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("(4)", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("(4)", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW YORK CONGRESSIONAL DELEGATION|WORLD TRADE CENTER EXPOSURE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW YORK CONGRESSIONAL DELEGATION|WORLD TRADE CENTER EXPOSURE", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW YORK CONGRESSIONAL DELEGATION|WORLD TRADE CENTER EXPOSURE", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>% 
  mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") & str_detect(SUBJECT, "Grant Support"), 1, TYPE)) %>% 
  mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") & str_detect(Affiliation, "Constituent"), 1, TYPE))
  
  
   

  
  
  
  
  
  
  
  
  
  
  
  
}
