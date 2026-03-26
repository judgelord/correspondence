# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOJ_EOIR" # for testing

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
  
  #Format date
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Con "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen "), "Senate", chamber))
  
  #Splits Rows with multiple authors
  data %<>%
    mutate(FROM = str_split(FROM, " and |&|\\(")) %>%
    unnest(FROM)

  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Con Jim Morgan", "Con Jim Moran")) %>%
    mutate(FROM = str_replace(FROM, "Eric J.J. Massa", "Eric Massa")) %>%
    mutate(FROM = str_replace(FROM, "Henry C. \"Hank\" Johnson, Jr.", "Henry Johnson")) %>%
    mutate(FROM = str_replace(FROM, "J. Gresham Barrett", "James Gresham BARRETT")) %>%
    mutate(FROM = str_replace(FROM, "Robert B. Anderholt", "Robert B ADERHOLT"))
  
 
  #Chamber typos
  data %<>%
    mutate(chamber = ifelse(FROM == "Joseph Pitts" & str_detect(congress, "Senate"), str_replace(congress, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(FROM == "Steve Driehaus" & str_detect(congress, "Senate"), str_replace(congress, "Senate", "House"), chamber))

  
  data %<>%
    mutate(FROM = (str_remove_all(FROM, "Con |Sen ")))
  
  # #Remove middle initials for now
  # data %<>%
  #   mutate(FROM = (str_replace(FROM, " \\D. ", " ")))
  
  
  #Extract Member names
    
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  
  #Unmatched
  unmatched <- data %>%
    filter(is.na(last_name)) %>%
    select(DATE, FROM, first_name, last_name, SUBJECT, everything())
  
  #Subject catching non authors
  #Filters for names still unmatched
  #Unfoundnames <- data %>%
   # filter(is.na(last_name)) %>%
    #extractMemberName(members = members, col_name = "SUBJECT") %>% 
  
  
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Don Tripp, State Representative of New Mexico|Daniel Dromm, New York City Council Member|Eva Galambos, Mayor of Sandy Springs, Georgia|
John M. Kefalas, State Representative of Colorado|Elaine Nekritz, Illinois State Representative - 57th District|John J. Gleason, State Senator of Michigan, 27th District|
Rashida H. Tlaib, State Representative of Michigan|Sen Leticia Van de Putte, R.PH., State of Texas, District 26|Amanda Aguirre, Senator, District 24, Arizona State Senate|
Willie Simmons, State Senator of Mississpi|Daphne Campell, RN, State Representative of Fl, District 108|Sen Noreen Evans, California State Senate, Second Senate District|
                                     Robert W. Singer, Senator, District 30|Roberto G. Lebron, AAG, State of New York|Rashida H. Tlaib, State Representative of Michigan"), 
                          "State Legislator", 
                          ERROR)) %>% 
    mutate(ERROR = ifelse(str_detect(FROM, "Judith A Neugent, City Clerk of Pembroke Pines|amie W. Curtis, Chairperson, Genessee County Board of Commissioners|
                                     Daniel Dromm, New York City Council Member"),
                          "From Local Gov", 
                          ERROR))
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "signers|other|others"), "Multiple unnamed members", NOTES))
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "John T. Morton, ICE"), "Director of ICE", ERROR))
  
  #Filter while working Comment out
  #data %<>%
   # mutate(FROM = (str_remove(FROM, "Don Tripp, State Representative of New Mexico|Daniel Dromm, New York City Council Member|Eva Galambos, Mayor of Sandy Springs, Georgia|John M. Kefalas, State Representative of Colorado|Elaine Nekritz, Illinois State Representative - 57th District|
    #                                 John J. Gleason, State Senator of Michigan, 27th District|Rashida H. Tlaib, State Representative of Michigan|
     #                              Sen Leticia Van de Putte, R.PH., State of Texas, District 26|Amanda Aguirre, Senator, District 24, Arizona State Senate|Willie Simmons, State Senator of Mississpi|
      #                               Daphne Campell, RN, State Representative of Fl, District 108|Sen Noreen Evans, California State Senate, Second Senate District")))
 
  
  #FOIA List
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Jim Bates") & is.na(last_name), "Jim Bates FOIA", NOTES)) #Could be Jim Bates from 103rd congress (Former Members of Congress for Common Ground), or James B. Renacci or Cheif Counsel Jim Bates
  
  
  #Duplicates
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Jim Moran") & is.na(last_name), "James Moran Duplicate", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Chris Van Hollen") & is.na(last_name), "Van Hollen Duplicate", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Sherrod Brown") & is.na(last_name), "Sherrod Brown Duplicate", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Barack Obama") & is.na(last_name), "Obama Duplicate", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Carolyn B. Maloney") & is.na(last_name), "Maloney Duplicate", NOTES))
  
 unmatched <- data %>%
   filter(is.na(last_name), 
          is.na(ERROR), 
          is.na(NOTES))
 
 Noted <- data %>%
   filter(! is.na(NOTES))
  
 data %<>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "^\\(b\\)\\(6\\)$"),1, TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "^\\(b\\)\\(6\\)$"),1, CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "^\\(Fax\\) \\(b\\)\\(6\\)$"),1, TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "^\\(Fax\\) \\(b\\)\\(6\\)$"),1, CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "nomination"),6, TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "nomination"),2, CERTAINTY))%>%
   mutate(ALT_TYPE = ifelse(str_detect(SUBJECT, "nomination"),1, ALT_TYPE))%>% 
   mutate(TYPE = ifelse(str_detect(SUBJECT, "Inc."),2, TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "Inc."),1, CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "non-profit"),3,TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "non-profit"),1,CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "Recognition"),3,TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "Recognition"),1,CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "Accreditation"),3,TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "Accreditiation"),1, CERTAINTY))%>%
   mutate(TYPE = ifelse(str_detect(SUBJECT, "^Writing on behalf of \\(b\\)\\(6\\)$"),1, TYPE))%>%
   mutate(CERTAINTY = ifelse(str_detect(SUBJECT, "^Writing on behalf of \\b\\)\\(6\\)$"),1, CERTAINTY))
  
  # unfoundmerge <- d %>%
  #   filter(is.na(bioname), is.na(ERROR))
  
  
  return(data)
  
}
