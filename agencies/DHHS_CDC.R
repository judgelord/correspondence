# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 1204 out of 1316 matches. I think all non-matches are non-members after checking, should be good. 
# Complete

# file.name <- "DHHS_CDC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()   
  
  # Letter ID and Folder ID are both incomplete, but seem to be complete when combined
  data %<>% mutate(LetterID = ifelse(LetterID == "NA", FolderID, LetterID))
  # look <- data %>% filter(str_detect(LetterID, ";"))
  # data %>% filter(is.na(LetterID))
  # data$LetterID
  
  # inspect
  data %>% filter(is.na(LetterID))
  
  
  # helper function to deal with duplicates
  combine <- . %>% unique() %>% str_c(collapse = ";;;")
  
  # because names usually appear in from, but not always, we first collapse the from column. later we paste it in with SUBJECT
  # where CDC breaks out letters by member, they all have the same subject (i'm pretty sure)
  # they all have the same action office, folderID, referrer, due date, closed date, action, and priority
  data %<>% group_by(LetterID, SUBJECT, DATE) %>% 
    add_count() %>% 
    summarise_all(combine) %>% 
    ungroup() %>% 
    distinct() 
  
  # inspect cases where FROM was combined -- this looks right
  look <- data %>% 
    filter(n>1, str_detect(FROM, ";;;") )
  
  # now that FROM is the same for each letter, we can select unique observations without letterID # NOTE: THIS COLLAPSES LETTERS SENT TO MORE THAN ONE AGENCY OFFICE
  # letters with different Letter or folder IDs may have different action, due date, closd date,
  data %<>% group_by(FROM, SUBJECT, DATE) %>% 
    add_count() %>% 
    summarise_all(combine) %>% 
    ungroup() %>% 
    distinct() 
  
  # inspect cases that did not appear to have unique letterIDs to make sure we are right to collapse them
  # this looks mostly right. we may be collapsing some constituent letters about drywall
  look <- data %>% 
    filter(n>1,
           str_detect(LetterID, ";;;"),
           !str_detect(FROM, "Director, "))

  
  # Error out letters from CDC director 
  data %<>% mutate(ERROR = ifelse(str_detect(FROM, "Director, "), "from agency", ERROR))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% str_replace("/201", "/1") 
  data$DATE %<>% str_replace("/200", "/0")
  data$DATE %<>% as.Date("%m/%d/%y")

  # bad dates
  data %>% filter(is.na(DATE)) %>% nrow()
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
 

  
  #Letters FROM CDC Director and HHS Secretary  to members
 #FIXME should double check that these are all correct and this code must always go before combinging FROM, SUBJECT, and TITLE
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "^Director, CDC$|^CDC Director"), 'Director, CDC', ERROR),
           # are these b6 constituent letters duplicates (the member names are in the SUBJECT Col)
           # no they are not duplicates. "Opioids: Representative Scott Taylor" is only B6 in FROM, but is the only letter observatoin for this letter
           #ERROR = ifelse(str_detect(FROM, "^b6$|^B6$"), "non-member", ERROR),
           # several letters with HHS, Secretary in FROM indicate letters from members that are not otherwise in the data--probably HHS is forwarding to CDC, but I'm only erroring out ones that are going from HHS to a member
           ERROR = ifelse(str_detect(SUBJECT, 'HHS Secretary writes|HHS is asking'), 'HHS, Secretary', ERROR ))
  
  
  
  data %<>%
    mutate(Title = str_replace(Title, " Sen\\.| sen\\.| sen | Sen ", "Senator")) %>%
    mutate(Title = str_replace(Title, " Rep\\.| rep\\.| Rep | rep |Congressman", "Representative")) %>%
    mutate(SUBJECT = str_replace(SUBJECT, " Sen\\.| sen\\.| sen | Sen ", "Senator")) %>%
    mutate(SUBJECT = str_replace(SUBJECT, " Rep\\.| rep\\.| Rep | rep |Congressman", "Representative"))
  
  
  data %<>%
    mutate(FROM =  paste(FROM, Title, SUBJECT))
  
  
  #Typos # SHOULD BE CORRECTED IN TYPOS SCRIPT
  data %<>%
    mutate(FROM = str_replace(FROM, "Wolfe, Frank", "Wolf, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Hollen Chris Van", "Chris Van Hollen")) %>%
    mutate(FROM = str_replace(FROM, "Garrett, E\\.", "Garrett, Scott")) %>%
    mutate(FROM = str_replace(FROM, "Bachus, Stephen", "Bachus, Spencer")) %>%
    mutate(FROM = str_replace(FROM, "Young, C\\. W\\.", "YOUNG, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Mack, Mary", "Mack Bono, Mary")) %>%
    mutate(FROM = str_replace(FROM, "Chabliss", "Chambliss")) %>% 
    mutate(FROM = str_replace(FROM, "Representative Merkley", "Senator Merkley"))



  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "writes to Representative|writes to Senator|writing to Representative|writing to Senator|Letter to Senator|letter to Senator|letter to Representative|Letter to Representative| to Senator "),
                          "from agency to member(s)",
                          ERROR))
  
  # inspect errors (we might be over-doing it)
  look <- filter(data, !is.na(ERROR), ERROR != "NA")
  
  
  nrow(data)
    #extract member names from FROM
  
   data %<>% extractMemberName(members, col_name = "FROM") 
   # inspect
   nrow(data)
   
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
    mutate(NOTES = ifelse(str_detect(FROM, "others|et al"), paste(NOTES, "multiple unnamed authors"), NOTES)) %>%
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
    # These should be last since they are very general guesses based on CDC classes, not content
  mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") & str_detect(Affiliation, "Constituent"), 1, TYPE)) %>% 
    mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") & str_detect(Affiliation, "Legislative"), 5, TYPE))
 
  return(data) 
}

