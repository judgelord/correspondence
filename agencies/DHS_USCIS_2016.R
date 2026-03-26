# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# source("setup.R")
# file.name <- "DHS_USCIS_2016" # for testing

clean <- function(file.name) {
  data_raw <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data_raw$LetterID <- 1:nrow(data_raw)
  
  # select distinct observations 
  data_distinct <- data_raw %>% select(-LetterID) %>% distinct()
  
  ##########################################################
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data_raw) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))

  
  #Code to split the first column into three:
  data %<>%
    separate(col = Subject, into = c("FROM", "SUBJECT", "USCIS_ID"),
             sep = " / ", remove = FALSE)
    
  
  data %<>% 
    mutate(DATE = `Received Date`)
    
  # test to make sure we did not creat a bunch of NAs (str_c propegates NAs )
  #  data %>% count(is.na(SUBJECT))
  
  
  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }  
## TYPE CODING
  
  #add TYPE and CONSTITUENT_TYPE columns (only when testing w/o running the merge file)
  # data[,"TYPE"] <- NA
  # data[,"CONSTITUENT_TYPE"] <- NA
  
  #need to combine SUBJECT and Form.Type columns to use str_detect to find combinations:
  data <- data %>%
    mutate(combined = str_c(SUBJECT, `Form Type`, sep = ";;;"))
  
  if(!"TYPE" %in% names(data)){
    data$TYPE <- NA
  }
  
  #need to code all of "Casework and Policy" observations first:
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined, "Casework and Policy") &
                           str_detect(combined, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined, "Casework and Policy") &
                           str_detect(combined, "I-129CW, Petition for a CNMI-Only Nonimmigrant Transitional Worker"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined, "Casework and Policy") &
                           str_detect(combined, "I-140 Immigrant Petition for Alien Worker"), 2, TYPE)) %>%
    #these could also be 3s - either public or private economic units
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined, "Casework and Policy") &
                           str_detect(combined, "I-924 Application for Regional Center Under the Immigrant Investor Pilot Program"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined, "Casework and Policy"), 1, TYPE))
  
  #coding for things that are obviously policy-related (not many of these):
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Hearing Notice"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Congressional Report(s)"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Executive Correspondence (for Secretary or Director)"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Invitation/Meeting Request"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Legislation"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "QFR"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Technical Guidance Request"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Testimony"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Thank You"), 6, TYPE))
  
  
  #coding for TYPE by Form.Type, regardless of whether SUBJECT is Casework or Policy?
  #I'd assume that if a form type is specified then a constituent is involved. but maybe not
  #we can talk about this
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "Application for Replacement/Initial Nonimmigrant Arrival/Departure Document"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "EOIR-29 Notice of Appeal to the Board of Immigration Appeals from a Decision of a DHS Officer"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "First Preference EB-1"), 1, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "FOIA"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "G-639 Freedom of Information Act/Privacy Act Request"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "G-884 Return of Original Documents"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-129CW, Petition for a CNMI-Only Nonimmigrant Transitional Worker"), 2, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-129F Petition for Alien Fiancé"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-130 Petition for Alien Relative"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-131 Application for Travel Document"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-134 Affidavit of Support"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-140 Immigrant Petition for Alien Worker"), 1, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-164 Notice of Appeal of Decision"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-192 Advance Permission to Enter as Nonimmigrant"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-212 Permission to Reapply for Admission into U.S."), 1, TYPE)) %>% #permission to apply for reentry after deportation or removal - interesting
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-290B Notice of Appeal or Motion"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-360 Petition for Amerasian, Widow(er), or Special Immigrant"), 1, TYPE)) %>% #special
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-407 Abandonment of Permanent Resident Card"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "(Employee)"), 1, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "(Family)"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "(Others)"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-526 Immigrant Petition by Alien Entrepreneur"), 1, TYPE)) %>% #should this be employment??? seems different than other things that fall in this category
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-539 Extend/Change Nonimmigrant Status"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-589 Asylum and for Withholding of Removal"), 1, TYPE)) %>% #humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "Orphan"), 1, TYPE)) %>% #family - covers two types of orphan forms
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-601 Waiver of Grounds of Inadmissibility"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-601A Provisional Unlawful Presence Waiver"), 1, TYPE)) %>% #family - requires qualifying relative
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-602 Application by Refugee for Waiver of Grounds of Excludability"), 1, TYPE)) %>% #humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-612 Waiver of the Foreign Residence Requirement"), 1, TYPE)) %>% #could be either family or humanitarian per the form
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-730 Refugee/Asylee Relative Petition"), 1, TYPE)) %>% #both family and humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-751 Petition to Remove Conditions on Residence"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-765 Application for Employment Authorization (DACA)"), 1, TYPE)) %>% #employment and DACA
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-765 Application for Employment Authorization"), 1, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "Adopt"), 1, TYPE)) %>% #family - covers two types of adoption forms
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-817 Application for Family Unity Benefits"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-821 Application for Temporary Protected Status"), 1, TYPE)) %>% #humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-824 Action on an Approved Application or Petition"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-829 Petition by Entrepreneur to Remove Conditions"), 1, TYPE)) %>% #employment
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-854 Inter-Agency Alien Witness and Informant Record"), 3, TYPE)) %>% #used by law enforcement agencies
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-864 Affidavit of Support Under Section 213A"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-90 Application to Replace Permanent Resident Card"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-910 Application for Civil Surgeon Designation"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-912 Request for Fee Waiver"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-914 Application for T Nonimmigrant Status"), 1, TYPE)) %>% #humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-918 Petition for U Nonimmigrant Status"), 1, TYPE)) %>% #humanitarian
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "I-924 Application for Regional Center Under the Immigrant Investor Pilot Program"), 2, TYPE)) %>% #could also be 3s - public or private entities
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-300 Application to File Declaration of Intention"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-336 Request for a Hearing on a Decision in Naturalization Proceedings"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-400 Application for Naturalization"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-426 Certification of Military or Naval Service"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-470 Application to Preserve Residence for Naturalization Purposes"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-565 Replacement Naturalization/Citizenship Document"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-600 Application for Certificate of Citizenship"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-600K Citizenship and Issuance of Certificate"), 1, TYPE)) %>% #family
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-644 Application for Posthumous Citizenship"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "N-648 Medical Certification for Disability Exceptions"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(`Form Type`, "USCIS Immigrant Fee"), 1, TYPE))
  
  ## Coding for TYPE by Topic column, if SUBJECT is casework:
  #first need to combine Topic and SUBJECT columns to use str_detect on both:
  data <- data %>%
    mutate(combined2 = str_c(SUBJECT, Topic, sep = ";;;"))
  
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "(7)"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Advance Parole"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Approved Document - Permanent Resident Card"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "B - Tourists or Visitors on Business"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Biometrics Collection"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Completing a Form"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Consular Return"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Cuban"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Deferred Action"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "ELIS I-90"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Expedite Request"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "(H-2B)"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "H-2A Temporary Agricultural Workers"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "I-551/ADIT Stamp"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Immigrant Status"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Immigration Policy"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Oath Ceremony"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Other Agency Referral"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Other Temporary Protected Status"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "(ONPT)"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Re-Entry Permits and Refugee Travel Documents"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "Renewal Delays"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "ASC"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "(SAVE)"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(combined2, "Casework") &
                           str_detect(combined2, "(Adoption related)"), 1, TYPE))
  
  #everything that the following line of code picks up should be 1s??
  #no uncoded "Casework and Policy" observations from this code????
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & 
                           str_detect(SUBJECT, "Casework"), 1, TYPE))
  
  ## Coding for CONSTITUENT_TYPE (immigrant/immigration type)
  if(!"CONSTITUENT_TYPE" %in% names(data)){
    data$CONSTITUENT_TYPE <- NA
  }
  #DACA and employment:
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "(DACA)"), 
                                     "Immigrant-Employment, Immigrant-DACA", CONSTITUENT_TYPE))
  
  #Employment (includes 1s and 2s):
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "First Preference EB-1"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-129 Petition for Nonimmigrant Worker"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-129CW, Petition for a CNMI-Only Nonimmigrant Transitional Worker"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-140 Immigrant Petition for Alien Worker"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "(Employee)"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-765 Application for Employment Authorization"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-829 Petition by Entrepreneur to Remove Conditions"), "Immigrant-Employment", CONSTITUENT_TYPE))
  
  #Family:
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-129F Petition for Alien Fiancé"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-130 Petition for Alien Relative"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "(Family)"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "Orphan"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-601A Provisional Unlawful Presence Waiver"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-751 Petition to Remove Conditions on Residence"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "Adopt"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-817 Application for Family Unity Benefits"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-600K Citizenship and Issuance of Certificate"), "Immigrant-Family", CONSTITUENT_TYPE))
  
  #Humanitarian:
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-589 Asylum and for Withholding of Removal"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-602 Application by Refugee for Waiver of Grounds of Excludability"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-821 Application for Temporary Protected Status"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-914 Application for T Nonimmigrant Status"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-918 Petition for U Nonimmigrant Status"), "Immigrant-Humanitarian", CONSTITUENT_TYPE))
  
  #Family and Humanitarian (I can't remember if we actually coded multiple types in the other data??)
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-730 Refugee/Asylee Relative Petition"), 
                                     "Immigrant-Humanitarian, Immigrant-Family", CONSTITUENT_TYPE))
  
  #General:
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-164 Notice of Appeal of Decision"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-290B Notice of Appeal or Motion"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "(Others)"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-601 Waiver of Grounds of Inadmissibility"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-824 Action on an Approved Application or Petition"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-90 Application to Replace Permanent Resident Card"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "I-912 Request for Fee Waiver"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-300 Application to File Declaration of Intention"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-336 Request for a Hearing on a Decision in Naturalization Proceedings"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-400 Application for Naturalization"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-470 Application to Preserve Residence for Naturalization Purposes"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-565 Replacement Naturalization/Citizenship Document"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "N-600 Application for Certificate of Citizenship"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(`Form Type`, "USCIS Immigrant Fee"), "Immigrant-General", CONSTITUENT_TYPE))
  
  #coding for CONSTITUENT_TYPE based on Topic (if Casework):
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Family"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Adoption"), "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Deferred Action"), "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "DACA"), "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Refugee"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Other Temporary Protected Status"), "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Temporary Nonagricultural Workers"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Temporary Agricultural Workers"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Combined EAD-Advance Parole Card Questions"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Approved Document - Permanent Resident Card"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Advance Parole"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Cuban Medical Professional Parole"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "ELIS I-90"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "I-551/ADIT Stamp"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Consular Return"), "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Employment"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Exceptional Ability"), "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                       str_detect(combined2, "Casework") &
                                       str_detect(combined2, "Skilled Worker"), "Immigrant-Employment", CONSTITUENT_TYPE))
  
  
}



if(F){
  
#Code just to see what the casework/policy breakdown looks like:
test <- subset(data, str_detect(SUBJECT, "Casework and Policy"))

#Same thing for policy:
test2 <- subset(data, SUBJECT == "Policy")
#and other ambiguous SUBJECT categories:
test3 <- subset(data, SUBJECT == "General")
test4 <- subset(data, SUBJECT == "Duplicate")


# code to create the table to check TYPE code is working:
addmargins(table(data$Topic, data$TYPE, useNA = "ifany"))

#code to create the tables to check the CONSTITUENT_TYPE code:
addmargins(table(data$`Form Type`, data$CONSTITUENT_TYPE, useNA = "ifany"))
addmargins(table(data$Topic, data$CONSTITUENT_TYPE, useNA = "ifany"))
addmargins(table(data$Topic, data$SUBJECT, useNA = "ifany"))

}
