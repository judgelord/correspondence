# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 1204 out of 1316 matches. I think all non-matches are non-members after checking, should be good. 
# Complete

# file.name <- "DHHS_CDC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()   
  
  # helper function to deal with duplicates
  combine <- . %>% unique() %>% str_c(collapse = ";")
  
  # select unique observations # NOTE: THIS COLLAPSES LETTERS SENT TO MORE THAN ONE AGENCY OFFICE
  data %<>% group_by(FROM, SUBJECT, DATE) %>% 
    add_count() %>% 
    summarise_all(combine) %>% 
    ungroup() %>% 
    distinct() 
  
  # Letter ID and Folder ID are both incomplete, but seem to be complete when combined
  data %<>% mutate(LetterID = ifelse(LetterID == "NA", FolderID, LetterID))
  # look <- data %>% filter(str_detect(LetterID, ";"))
  # data %>% filter(is.na(LetterID))
  # data$LetterID
  

  
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
    mutate(ERROR = ifelse(str_detect(FROM, "^Director, CDC$|^CDC Director"), 'Director, CDC', ERROR),
           ERROR = ifelse(str_detect(FROM, "^b6$|^B6$"), "non-member", ERROR))
  
  data %<>%
    mutate(ERROR = ifelse(grepl('^HHS, Secretary',data$FROM), 'HHS, Secretary', ERROR ))
  
  data %<>%
    mutate(Title = str_replace(Title, " Sen\\.| sen\\.| sen | Sen ", "Senator")) %>%
    mutate(Title = str_replace(Title, " Rep\\.| rep\\.| Rep | rep |Congressman", "Representative")) %>%
    mutate(SUBJECT = str_replace(SUBJECT, " Sen\\.| sen\\.| sen | Sen ", "Senator")) %>%
    mutate(SUBJECT = str_replace(SUBJECT, " Rep\\.| rep\\.| Rep | rep |Congressman", "Representative"))
  
  data %<>%
    mutate(Title = str_replace(Title, "Representative Merkley", "Senator Merkley"))
  
  #Typos # SHOULD BE CORRECTED IN TYPOS SCRIPT
  data %<>%
    mutate(FROM = str_replace(FROM, "Wolfe, Frank", "Wolf, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Hollen Chris Van", "Chris Van Hollen")) %>%
    mutate(FROM = str_replace(FROM, "Garrett, E\\.", "Garrett, Scott")) %>%
    mutate(FROM = str_replace(FROM, "Bachus, Stephen", "Bachus, Spencer")) %>%
    mutate(FROM = str_replace(FROM, "Young, C\\. W\\.", "YOUNG, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Mack, Mary", "Mack Bono, Mary")) %>%
    mutate(FROM = str_replace(FROM, "Chabliss", "Chambliss"))
  
  data %<>%
    mutate(FROM =  paste(Title, SUBJECT, FROM))


  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "writes to Representative|writes to Senator|writing to Representative|writing to Senator|Letter to Senator|letter to Senator|letter to Representative|Letter to Representative| to Senator "),
                          "from agency to member(s)",
                          ERROR))
  
    #extract member names from FROM
   data %<>% extractMemberName(members, col_name = "FROM") 
   
   
 data %<>%
   distinct() %>% 
   select(DATE, FROM, first_name, last_name, pattern, everything())
  
  #Checks how many members are not captured
  FROMunamed <- data %>%
    filter(is.na(ERROR), is.na(last_name))
  
   # #   
   # NAstring <- data %>%
   #    filter(is.na(string))

  
  
  #Filter for observations with un-named authors
  otherauthors <- data %>%
    filter(str_detect(FROM, "others|et al"))
  
  data %<>% mutate(NOTES = ifelse(str_detect(FROM, "others|et al"), paste("FOIA additional members", NOTES), NOTES))
  
  #Make note of all observations with un-named authors # WE SHOULD CONFIRM THAT ALL OF THESE SHOULD BE ERRORED OUT
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "CDC, Director|Director, CDC|Director, DO NOT USE CDC|Director, DO NOT USE THIS ONE CDC|HHS, Secretary|CDC Director|Director, NCEH|Gerberding, Julie|Secretary Michael O\\. Leavitt|Gerberding, Julie") & is.na(last_name), "CDC Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "President, of the United States|President Bush") & is.na(last_name), "President", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "others|et al"), "multiple unnamed authors", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Awa Coll-Seck|DeLeon, Patrick|Bonham, David|Boyer, Ashley|Collins, Francis|Gabbard, Mike|Graham, Garth|Groblewski, Mark|William F\\. Marshal") & is.na(last_name), "non member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "State Representative Steve Wieckert|Governor Bobby Jindal|Boyle, Kevin|Briggs, Tim|Duff, Bob|Rubio, Michael|Scott, Rick|David Ige|Bob Duff|State Senator|Bob Duff") & is.na(last_name), "state legislator", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(Title, "David Ige") & is.na(last_name), "state legislator", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Bruce Aylward") & is.na(last_name), "world health organization member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM , "Graham, Bob") & congress %in% c(111,113), "no longer in congress", ERROR))

  
#Check (b)(6) removals are correct
Nab6<- data %>%
  filter(str_detect(FROM, "\\(b\\)\\(6\\)"))

  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           !str_detect(FROM, "Donna Christensen") )
  
  
   
# TYPE CODING 
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
 
  return(data) 
}

