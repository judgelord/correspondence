# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOJ_ENRD" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data %<>%
    group_by(SUBJECT, DATE, FROM) %>%
    mutate(n = n(),
           WFs = str_c(WF, collapse = "; ")) %>%
    arrange(-n) %>%
    select(-WF) %>%
    ungroup() %>%
    distinct()
  
  # create LetterID variable
  data$LetterID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name

  #Format Date
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Congressman |Rep.|Con. |con. |Congresswoman |MCs |Cong. |congress |cong.|Reresentatives|Congressmen |Cong."), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen |Sen.|Senator |Senators "), "Senate", chamber))  
  
  data %<>%
    mutate(FROM = str_replace(FROM, ", Jr.", " Jr.")) %>%
    mutate(FROM = str_replace(FROM, ", II", " II")) %>%
    mutate(FROM = str_replace(FROM, ", Jr", " Jr")) %>%
    mutate(FROM = str_replace(FROM, "Cong. Timothy Ryan Cong. Betty Sutton", "Cong. Timothy Ryan, Cong. Betty Sutton"))
  
  data %<>%
    mutate(FROM = str_remove(FROM, "Congressman |Rep.|Con. |con. |Congresswoman |MCs |Cong. |congress |cong.|Reresentatives|Congressmen |Cong.")) %>%
    mutate(FROM = str_remove(FROM, "Sen |Sen.|Senator |Senators ")) %>%
    mutate(FROM = str_remove_all(FROM, ", DC.*|, WA.*|, TN.*|, CT.*|, NY.*|, D.C..*"))
  
  data %<>%
    mutate(FROM = str_split(FROM, ",| and ")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(FROM = str_replace(FROM, "tors Wyden", "Senator WYDEN")) %>%
    mutate(FROM = str_replace(FROM, "essman j gresham barrett", "Representative BARRETT")) %>%
    mutate(FROM = str_replace(FROM, "McCain", "Senator McCAIN")) %>%
    mutate(FROM = str_replace(FROM, "essman J. Gresham Barrett", "Representative BARRETT")) %>%
    mutate(FROM = str_replace(FROM, "E. Benjamin Nelson", "Earl B NELSON")) %>%
    mutate(FROM = str_replace(FROM, "Hon. Mary LANDRIEU United States te Washington", "Mary LANDRIEU"))
  
  #Create ID
  data$ID <- c(1:nrow(data))
  
  data %<>%
    extractMemberName2(members = members, col_name = "FROM")
 
   #Check for duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; ")) %>%
    distinct()
  
  
  NoChamber <- data %>%
    filter(is.na(chamber))
  
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
 
  #Format last name and put in last_name  
  data %<>%
    mutate(FROM = str_trim(FROM)) %>%
    mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  NoFirst <- data %>%
    filter(is.na(first_name))
  
  #Add first name 
  data %<>%
    mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name)) 

  
   data %<>%
     mutate(NOTES = ifelse(str_detect(FROM, "Davis") & is.na(first_name), "Multiple Davis' FOIA", NOTES)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Governor"), "State Govermor", ERROR)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "committee|Committee|Cmte|Comte"), "Committee", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "other Members of Congress"), "Multiple unnamed members", NOTES))
    
   unfoundnames2 <- data %>%
     filter(is.na(last_name))
   
  #Filter to use after merge
 # Unmatched <- d %>%
   # filter(is.na(bioname))
  
  return(data)
  
}
