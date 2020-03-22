# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 1204 out of 1316 matches. I think all non-matches are non-members after checking, should be good. 
# Complete

# file.name <- "DHHS_CDC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()   
  

  # select distinct observations 
  data_distinct <- data %>% select(-c(FolderID, Action)) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
   data %<>%
     group_by(FROM, SUBJECT, DATE) %>%
     mutate(n = n(),
            Addressees = str_c(Addressee, collapse = "; "),
            Prorities = str_c(Prority, collapse = "; "),
            Titles = str_c(Title, collapse = "; "),
            AnswerDates = str_c(Answer.Date, collapse = "; "),
            CloseDates = str_c(Close.Date, collapse = "; "),
            ProgramDueDates = str_c(Program.Due.Date, collapse = "; "),
            ReceiveDates = str_c(Receive.Date, collapse = "; ")) %>%
     arrange(-n) %>%
     select(-Prority, -Title, -Answer.Date, -Close.Date, -Program.Due.Date, -Receive.Date, -Addressee) %>%
     ungroup() %>%
     distinct()
  
    data %<>%
     group_by(FROM, SUBJECT, DATE) %>%
     mutate(n = n())%>%
     arrange(-n, FROM) %>%
     ungroup()
     

    
    data %<>%
      mutate(origID = ID)
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")

  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
 

  
  #Comments errors for CDC Director and HHS Secretary  
  data %<>%
    mutate(ERROR = ifelse(grepl('^Director, CDC$',data$FROM), 'Director, CDC', ERROR ))
  
  data %<>%
    mutate(ERROR = ifelse(grepl('^HHS, Secretary',data$FROM), 'HHS, Secretary', ERROR ))
  
  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Wolfe, Frank", "Wolf, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Hollen Chris Van", "Chris Van Hollen")) %>%
    mutate(FROM = str_replace(FROM, "Garrett, E\\.", "Garrett, Scott"))
  
  
   # extract member names from FROM
  data %<>% extractMemberName(members, col_name = "FROM") %>%
    mutate(origID = ifelse(is.na(origID), FolderID, origID)) %>%
    select(-ID) %>%
    mutate(LetterID = origID) %>%
    distinct()
  
data %<>%
  select(first_name, last_name, everything())
  
  #Checks how many members are not captured
  FROMunamed <- data %>%
    filter(is.na(last_name))
  

  
  dataTitles <- data %>%
    select(-first_name, -last_name, -LetterID)
    
    dataTitles %<>%
      extractMemberName(members, col_name = "Titles") %>%
      mutate(origID = ifelse(is.na(origID), FolderID, origID)) %>%
      select(-ID) %>%
      mutate(LetterID = origID) 
    
    #addedNames <- dataTitles %>%
     # filter(! is.na(last_name))
    #adedNames %<>% select(LetterID, last_name, first_name, DATE, FROM, everything())
  
  dataSUBJECT <- data %>%
    select(-first_name, -last_name, -LetterID)
  
  dataSUBJECT %<>%
    extractMemberName(members, col_name = "SUBJECT") %>%
    mutate(origID = ifelse(is.na(origID), FolderID, origID)) %>%
    select(-ID) %>%
    mutate(LetterID = origID)
  
  #addedNamesSUB <- dataSUBJECT %>%
   # filter(! is.na(last_name))
  #addedNamesSUB %<>% select(LetterID, last_name, first_name, DATE, FROM, everything())
  
  data %<>%
    full_join(dataTitles)
  data %<>%
    full_join(dataSUBJECT)
    
    data %<>%
    mutate(LetterID = origID) %>%
    distinct()
   
    
  NAstring <- data %>%
    filter(is.na(string))

  
  
  #Filter for observations with un-named authors
  otherauthors <- data %>%
    filter(str_detect(FROM, "others|et al"))
  
  #Make note of all observations with un-named authors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "CDC, Director|Director, CDC|Director, DO NOT USE CDC|Director, DO NOT USE THIS ONE CDC|HHS, Secretary|CDC Director"), "CDC Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "President, of the United States"), "President", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "others|et al"), "multiple unnamed authors", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Awa Coll-Seck|DeLeon, Patrick"), "non member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Boyle, Kevin|Briggs, Tim"), "state legislator", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Bruce Aylward"), "world health organization member", ERROR))



  #Add ID
  data %<>%
   mutate(ID = row_number())
  
#Check (b)(6) removals are correct
Nab6<- data %>%
  filter(str_detect(FROM, "\\(b\\)\\(6\\)"))


Unfoundnames <- data %>%
  filter(is.na(last_name))

MergeUnfound <- d %>%
  filter(is.na(last_name))




  # arrange columns for hand coding
  data %<>% select(LetterID, last_name, first_name, DATE, FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|TUBERCULOSIS. URGE|STEPS TO A HEALTHIER|PREVENTING SECONDARY|HEPATITIS FUNDING|SECTION 317|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|HEAVY METALS|TRAVEL POLICIES|TUBERCULOSIS. URGE|STEPS TO A HEALTHER|PREVENTING SECRETARY|HEPATITIS FUNDING|SECTION 317|OPIOID THE COMMITTEE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("SICKLE CELL DISEASE|ZIKA FUNDING|VECTOR BORNE DISEASE|TUBERCULOSIS. URGE|STEPS TO A HEALTHER|PREFENTING SECONDARY|HEPATITIS FUNDING|SECTION 317", SUBJECT, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("BEHALF OF CONSTITUENT|BEHALF OF HIS CONSTITUENT|(6)", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEPATITIS|INFECTIOUS DISEASE OUTBREAK", Titles, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEPATITIS|INFECTIOUS DISEASE OUTBERAK", Titles, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEPATITIS", Titles, ignore.case = TRUE), "BUDGET ALLOCATION", POLICY_EVENT)) %>%
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
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Titles, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("INAPPROPRIATE METHODOLOGIES ", Titles, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("INAPPROPRIATE METHODOLOGIES", Titles, ignore.case = TRUE), "3", ALT_TYPE)) %>%
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
 
  return(data) 
}

