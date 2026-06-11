# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Lots of errors, needs fixing
# down to 264 errors, lots of spelling

# file.name <- "ED" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$originalDATE <- data$DATE
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking for dates that are NA
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  # preprocess FROM column
  data$FROM <- gsub("( |^)The( |$)|honorable|Honorable|Honorabl e|hon\\.|honora ble|Honorab le|Senator|Name:|Ho norable|Representataive|Honorabel", "", data$FROM, ignore.case = TRUE)
  data$FROM <- gsub("Mr\\.|Ms\\.|Mr ", "",data$FROM, ignore.case = TRUE)
  data$FROM <- gsub("Wolff", "Wolf", data$FROM)
  
  #data %<>% filter(str_detect(FROM,"Hollen, Chris Van"))
  
  #Fixes name typo
  data$FROM %<>%
    str_replace_all("Willia Roger", "Roger Williams") %>%
    str_replace_all("Cornyn,TheJohn", "John Cornyn") %>%
    str_replace_all("Hollen,  Chris Van", "VAN HOLLEN, Christopher") %>%
    str_replace_all("Wasserman Schultz,", "Wasserman Schultz, Debbie") %>%
    str_replace_all("Capito,TheShelley", "CAPITO, Shelley Moore") %>%
    str_replace_all("Frelinghuysen,", "Frelinghuysen, Rodney") %>%
    str_replace_all("Cortez Masto,", "CORTEZ MASTO, Catherine Marie") %>%
    str_replace_all("Uda ll,  Tom                        QS-QQ", "UDALL, Thomas (Tom)") %>%
    str_replace_all("McMorris Rodgers ,", "McMORRIS RODGERS, Cathy") %>%
    str_replace_all("Ros-Lehtinen,", "ROS-LEHTINEN, Ileana") %>%
    str_replace_all("Butterfield,  G.K.", "BUTTERFIELD, George Kenneth, Jr. (G.K.)") %>%
    str_replace_all("Butterfield,  G. K.", "BUTTERFIELD, George Kenneth, Jr. (G.K.)") %>%
    str_replace_all("Ada Alma", "ADAMS, Alma") %>%
    str_replace_all("Young,  CW Bill", "YOUNG, Charles William (Bill)") %>%
    str_replace_all("Young,  C.W. Bill", "YOUNG, Charles William (Bill)") %>%
    str_replace_all("Cornyn, Mr John", "Cornyn, John") %>%
    str_replace_all("Barr,TheAndy", "BARR, Garland H. (Andy) IV") %>%
    str_replace_all("Griffiths,  H. Morgan", "GRIFFITH, H. Morgan") %>%
    str_replace_all("Warner, Hono rable Mark", "WARNER, Mark") %>% 
    str_replace_all("Graham, Lind sey", "Graham, Lindsey") %>%
    str_replace_all("Beyer S, Donald,", "BEYER, Donald Sternoff Jr") %>%
    str_replace_all("Lewis,  Jerry", "LEWIS, Charles Jeremy (Jerry)")
  
    
    
  
    
 
   

  


  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  #Error for nonmembers
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Robinson, Stebe|Sampson,  Ann|Raad, Jim|Christensen,  Donna M.|Second Congressional District of Illinois|Coock, Barbara|Norton, Eleanor Holmes|Norton,  Eleanor H.|Norton ,  Eleanor|Young,  Nancy W."), "Non members of Congress", ERROR))
  

  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STUDENT LOAN QUESTION|STATUS CHECK|CONSTITUENT|CASEWORK|STUDENT LOAN REPAYMENT|FSA QUESTION|STUDENT LOAN INQUIRY|FOIA REQUEST|STUDENT LOAN DISCHARGE|STAFFER.*QUESTION|STAFFER.*REQUEST|FINANCIAL AID|LOAN FORGIVENESS|DISCHARGE APPLICATION|DEFAULTED STUDENT LOAN|SEEKING ASSISTANCE|REQUEST ASSISTANCE|HIS APPLICATION|STUDENT LOAN|HE OWES|STATUS UPDATE|REGARDING HIS|SHE IS|FSA2|FSA INFORMATION|NEED ASSISTANCE|HIGH SCHOOL|HER SON|HER DAUGHTER|HIS SON|HIS DAUGHTER|FAFSA|FERPA|REGARDING HER|CLAIMING|STUDENT AID|REPAYMENT OPTIONS|DISABILITY CASE|WOULD LIKE|LOAN QUESTION|UPDATE|FSA INQUIRY|SURVEY|HARASSMENT COMPLAINT|INQUIRY QUESTION|PARENTS.*CONCERNED|REQUESTING ASSISTANCE|SCHOLARSHIP|LOAN CONSOLIDATION|TUITION|PUBLICATION REQUEST|OCR COMPLAINT|APPEAL CASE|REPAYMENT|FOLLOW UP|FSA|FOLLOW-UP|DEBT|STATUS U|COMPLAINT AGAINST|FOREIGN SCHOOL", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STUDENT LOAN QUESTION|STATUS CHECK|CONSTITUENT|CASEWORK|STUDENT LOAN REPAYMENT|FSA QUESTION|STUDENT LOAN INQUIRY|FOIA REQUEST|STUDENT LOAN DISCHARGE|STAFFER.*QUESTION|STAFFER.*REQUEST|FINANCIAL AID|LOAN FORGIVENESS|DISCHARGE APPLICATION|DEFAULTED STUDENT LOAN|SEEKING ASSISTANCE|REQUEST ASSISTANCE|HIS APPLICATION|STUDENT LOAN|HE OWES|STATUS UPDATE|REGARDING HIS|SHE IS|FSA2|FSA INFORMATION|NEED ASSISTANCE|HIGH SCHOOL|HER SON|HER DAUGHTER|HIS SON|HIS DAUGHTER|FAFSA|FERPA|REGARDING HER|CLAIMING|STUDENT AID|REPAYMENT OPTIONS|DISABILITY CASE|WOULD LIKE|LOAN QUESTION|UPDATE|FSA INQUIRY|SURVEY|HARASSMENT COMPLAINT|INQUIRY QUESTION|PARENTS.*CONCERNED|REQUESTING ASSISTANCE|SCHOLARSHIP|LOAN CONSOLIDATION|TUITION|PUBLICATION REQUEST|OCR COMPLAINT|APPEAL CASE|REPAYMENT|FOLLOW UP|FSA|FOLLOW-UP|DEBT|STATUS U|COMPLAINT AGAINST|FOREIGN SCHOOL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("LANGSTON UNIVERSITY|COMMUNITY COLLEGE|TEACHING AMERICAN HISTORY|INVESTING IN INNOVATION|RACE TO THE TOP|IMPROVEMENT OF POSTSECONDARY|BEHALF.*UNIVERSITY|CAROL M. WHITE|PROMISE NEIGHBORHOOD|PROGRAM APPLICATION|ON BEHALF OF.*GRANT APPLICATION|GRANT APPLICATION|SUPPORT FOR GRANT|UPWARD BOUND|ON BEHALF OF.*COLLEGE|CITY SCHOOLS|ENHANCEMENT|ON BEHALF OF.*SCHOOL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LANGSTON UNIVERSITY|COMMUNITY COLLEGE|TEACHING AMERICAN HISTORY|INVESTING IN INNOVATION|RACE TO THE TOP|IMPROVEMENT OF POSTSECONDARY|BEHALF.*UNIVERSITY|CAROL M. WHITE|PROMISE NEIGHBORHOOD|PROGRAM APPLICATION|ON BEHALF OF.*GRANT APPLICATION|GRANT APPLICATION|SUPPORT FOR GRANT|UPWARD BOUND|ON BEHALF OF.*COLLEGE|CITY SCHOOLS|ENHANCEMENT|ON BEHALF OF .*SCHOOL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STUDENT LOAN PROGRAM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STUDENT LOAN PROGRAM", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("STUDENT LOAN PROGRAM", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC SCHOOLS", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC SCHOOLS", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PUBLIC SCHOOLS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GLASGOW SCHOOL|JACOB JAVIT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GLASGOW SCHOOL|JACOB JAVIT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GLASGOW SCHOOL|JACOB JAVIT", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (grepl("COMPANY WOULD LIKE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (grepl("COMPANY WOULD LIKE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
  
  
  
  
  
  
  return(data)
  
  
}
