#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "VA Rochelle" # for testing

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

  #Format Date
  # FIXME, if read as character, DATE will be string "NA" not real NA and this will fail:
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date Inquiry Assigned`, DATE))
  
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  
  
  #Check for NA Dates # FIXME
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  data %<>% anti_join(NoDATE)

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  
  #Filter out rows without data
  data %<>% 
    mutate(FROM = ifelse(FROM %in% c("", "N/A","n/a","N/a", "n/a/","m/a","`N/A") | is.na(FROM),
                         person,
                         FROM))
  
  data %<>%
    mutate(FROM = str_remove(FROM, " N/A"))
  
  #sample
  #sampledata <- data %>%
   #filter(str_detect(FROM, "Capito"))

 # data <- sampledata

  #Trim White Space
  data %<>%
    mutate(FROM = str_squish(FROM))
 
#Typo  
data %<>%
  mutate(FROM = str_replace(FROM, "VanHollen, C", "Van Hollen, C")) %>%
  mutate(FROM = str_replace(FROM, "^Balart, M.", "Diaz-Balart, M.")) %>%
  mutate(FROM = str_replace(FROM, "Diaz Balart, M.", "Diaz-Balart, M.")) %>%
  mutate(FROM = str_replace(FROM, "Johnson, E. B.", "Johnson, E.")) %>%
  mutate(FROM = str_replace(FROM, "Rashia, Jamie", "Raskin, Jamie")) %>%
  mutate(FROM = str_replace(FROM, "Carter, E.L.", "Carter, Earl")) %>%
  mutate(FROM = str_replace(FROM, "Capito, S.M.", "Moore Capito, Shelley"))
  
#Wrong chambers
data %<>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress == 115 & str_detect(chamber, "Senate"), str_replace(FROM, "Kennedy, J.", "John Neely KENNEDY"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress == 115 & str_detect(chamber, "House"), str_replace(FROM, "Kennedy, J.", "Joseph P KENNEDY"), FROM)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Risch"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Carter, Earl"), str_replace(chamber, "Senate", "House"), chamber)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Moran, J.") & congress %in% c(112,113) & str_detect(chamber, "Senate"), str_replace(FROM, "Moran, J.", "MORAN, Jerry"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Moran, J.") & congress %in% c(112,113) & str_detect(chamber, "House"), str_replace(FROM, "Moran, J.", "MORAN, James"), FROM))

#string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, "\\/")) %>%
    unnest(FROM)
  
  # create ID 
  data$ID <- 1:nrow(data)

  data %<>%
    mutate(FROM = str_remove(FROM, "\\/"))
  
  #Paste Chamber into FROM
  data %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House") & !str_detect(FROM, ","), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate") & !str_detect(FROM, ","), paste("Senator", FROM, sep = " "), FROM))
  
  #Extract Member Names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  
  
  
  
  #Check for duplicates
  sample2data <- data %>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "))
  
  #Filter for Unfoundnames
  Unfoundnames <- data %>%
    filter(is.na(last_name), !is.na(string)) %>%
    select(-last_name, -first_name)
  
  
  #Membership Errors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "SVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "non-cong"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Non-Congressional"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative Pellito, John"), "House Staff", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Sablan, Gregorio|Representative Radewagen, A|Representative Sablan, G.|Representative Sablan, G"), "Not voting member", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative non Congressional|Representative Non-Congressional|Representative NonCongressional|Representative NY-25"), "Not Member", ERROR))
  
  #FOIA NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Young, D."), "Multiple Young's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Miller, G."), "Multiple Miller's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Rogers, M."), "Multiple Rogers' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Davis"), "Multiple Davis' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Rose"), "Multiple Rose's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Smith"), "Multiple Smith's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Johnson"), "Multiple Johnson's FOIA", NOTES))
  
  # Error out non-members 
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Pierluisi|Bordallo|Norton|Faleomavaega|Christensen|Representative non Congressional|
                         Representative Non-Congressional|Representative NonCongressional|Representative NY-25"),
                          "non-member",
                          ERROR))
           
  
 
  Unfoundnames2 <- data %>%
    filter(is.na(last_name),
           is.na(ERROR), 
           is.na(NOTES),
           str_detect(pattern, "404error"))
  
# Unfoundnames2 %<>% extractMemberName(members, "FROM")
  
   #Check after run through merge
   #Unmatched <- d %>%
   #filter(is.na(bioname))
  
  

  
 return(data)
  
}

  