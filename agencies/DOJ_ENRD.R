# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOJ_ENRD Julia" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% 
    select(-LetterID) %>%     
    group_by(SUBJECT, DATE, FROM) %>%
    mutate(n = n(),
           WFs = str_c(WF, collapse = "; ")) %>%
    arrange(-n) %>%
    select(-WF) %>%
    ungroup() %>% 
    distinct() 
  
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% 
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
    mutate(chamber = ifelse(str_detect(FROM, "congressman |Congressman |Rep\\.|Con\\. |con\\. |Congresswoman |MCs |Cong\\. |congress |cong\\.|Reresentatives|Congressmen |Cong\\.|Representative|House of Reps\\."), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen |Sen\\.|Senator |Senators |Sens\\. "), "Senate", chamber))  
  
  data %<>%
    mutate(FROM = str_replace(FROM, ", Jr\\.", " Jr\\.")) %>%
    mutate(FROM = str_replace(FROM, ", II", " II")) %>%
    mutate(FROM = str_replace(FROM, ", Jr", " Jr")) %>%
    mutate(FROM = str_replace(FROM, "Cong\\. Timothy Ryan Cong\\. Betty Sutton", "Cong\\. Timothy Ryan, Cong\\. Betty Sutton"))
  
  data %<>%
    mutate(FROM = str_remove(FROM, "congressman |Congressman |Rep\\.|Con\\. |con\\. |Congresswoman |MCs |Cong\\. |congress |cong\\.|Reresentatives|Congressmen |House of Reps\\.")) %>%
    mutate(FROM = str_remove(FROM, "Sen |Sen\\.|Senators |Sens\\. "))
  
  data %<>%
     mutate(FROM = str_remove_all(FROM, ", DC.*|, WA.*|, TN.*|, CT.*|, NY.*|, D.C..*|FL|CO"))
  
  #Separate Multiple Authors
  data %<>%
    mutate(FROM = str_split(FROM, ",| and |&|;")) %>%
    unnest(FROM)
  

  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Rahall") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Albio Sires") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Earl Blumenauer") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Emanuel Cleaver II") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Charles B\\. Rangel") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Fred Upton") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber))
 
  #Paste Chamber into FROM
  data %<>%
    mutate(FROM = str_trim(FROM)) %>%
    mutate(FROM =ifelse(! str_detect(FROM, " ") & str_detect(chamber, "House"), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(! str_detect(FROM, " ") & str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "Senator Young", "Representative Young"))
  
  #Typos 
  data %<>%
   # mutate(FROM = str_replace(FROM, "tors Wyden", "Senator WYDEN")) %>%
    mutate(FROM = str_replace(FROM, "J\\. Gresham Barrett", "James Gresham BARRETT")) %>%
    mutate(FROM = str_replace(FROM, " \\. McCain", " John McCAIN")) %>%
    #mutate(FROM = str_replace(FROM, "essman J. Gresham Barrett", "Representative BARRETT")) %>%
    mutate(FROM = str_replace(FROM, "E\\. Benjamin Nelson", "Earl B NELSON")) %>%
    mutate(FROM = str_replace(FROM, "Hon\\. Mary LANDRIEU United States te Washington", "Mary LANDRIEU")) %>%
    mutate(FROM = str_replace(FROM, "Cong\\. Carolyn Kilpatrick", "Carolyn KILPATRICK")) %>%
    mutate(FROM = str_replace_all(FROM, "Honorable John McCain United States te Washington|Hon\\. John McCain United States te Washington|John McCain United States Senate Washington", "John McCAIN")) %>%
    mutate(chamber = ifelse(FROM == "Young" & congress == 109 & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(FROM = str_replace(FROM, "Bono Mack", "Mary Mack BONO")) %>%
    mutate(FROM = str_replace(FROM, "Representative Water", "Representative Waters"))
    
    # FIXME # THIS LETTER ID IS NO LONGER CORRECT:
    #mutate(FROM = ifelse(str_detect(FROM, "Nelson") & LetterID == 161, str_replace(FROM, "Nelson", "Clarence NELSON"), FROM))
  
  #Extract Member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
 
  
  NoChamber <- data %>%
    filter(is.na(chamber))
  
  #Filter for Unfoundnames
  Unfoundnames <- data %>%
    filter(is.na(last_name))

  
  #FOIA and State politicians
   data %<>%
     mutate(NOTES = ifelse(str_detect(FROM, "Davis") & is.na(first_name), "Multiple Davis' FOIA", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "Mike Rogers"), "Multiple Mike Rogers' FOIA", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "Emanuel") & is.na(last_name), "Multiple Emanuel's FOIA", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "Senator Nelson") & is.na(last_name), "Multiple Nelson's FOIA", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "Representative Young") & is.na(last_name), "Multiple Young's FOIA", NOTES)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Governor"), "State Governor", ERROR)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "committee|Committee|Cmte|Comte"), "Committee", NOTES)) %>%
     mutate(NOTES = ifelse(str_detect(FROM, "other Members of Congress"), "Multiple unnamed members", NOTES)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Mary Landrieu") & congress %in% 114, "No longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Carolyn KILPATRICK") & congress %in% 112, "No longer in congress", ERROR))
    
   unfoundnames2 <- data %>%
     filter(is.na(last_name))
   
   data %>%
     filter(ID == 67) %>%
     select(FROM)
  #Filter to use after merge
 # Unmatched <- d %>%
  #  filter(is.na(bioname))
  
  return(data)
  
}
