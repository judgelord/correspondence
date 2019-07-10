#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "VA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  

  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name 

  #Format Date
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date Inquiry Assigned`, DATE))
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  
  #Filter out rows without data
  data %<>% filter(!FROM == "")
  data %<>% filter(!FROM == "N/A") %>%
    filter(! FROM == "n/a") %>%
    filter(! FROM == "N/a") %>%
    filter(! FROM == "n/a/") %>%
    filter(! FROM == "m/a") %>%
    filter(! FROM == "`N/A")
  
  data %<>%
    mutate(FROM = str_remove(FROM, " N/A"))
  
  #sample
  #sampledata <- data %>%
   #filter(str_detect(FROM, "Capito"))

 # data <- sampledata

  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))
 
#Typo  
data %<>%
  mutate(FROM = str_replace(FROM, "VanHollen, C", "Van Hollen, C")) %>%
  mutate(FROM = str_replace(FROM, "^Balart, M.", "Diaz-Balart, M.")) %>%
  mutate(FROM = str_replace(FROM, "Diaz Balart, M.", "Diaz-Balart, M.")) %>%
  mutate(FROM = str_replace(FROM, "Johnson, E. B.", "Johnson, E.")) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Risch"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(FROM = str_replace(FROM, "Rashia, Jamie", "Raskin, Jamie")) %>%
  mutate(FROM = str_replace(FROM, "Carter, E.L.", "Carter, Earl"))
  
data %<>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress == 115 & str_detect(chamber, "Senate"), str_replace(FROM, "Kennedy, J.", "John Neely KENNEDY"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress == 115 & str_detect(chamber, "House"), str_replace(FROM, "Kennedy, J.", "Joseph P KENNEDY"), FROM))
  
#string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, "\\/")) %>%
    unnest(FROM)

  data %<>%
    mutate(FROM = str_remove(FROM, "\\/"))
  
  #Extract Member Names
  data %<>%
    extractMemberName2(members = members, col_name = "FROM")
  
  #Check for duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "))
  
  #Filter for Unfoundnames
  Unfoundnames <- data %>%
    filter(is.na(last_name)) %>%
    select(-last_name, -first_name)
 
  #Separate from data 
  data %<>%
    anti_join(Unfoundnames)
 
 
  #Paste Chamber into FROM
  Unfoundnames %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House"), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM))
  
  #Extract Member Names
  Unfoundnames %<>%
    extractMemberName2(members = members, col_name = "FROM")
  
  #Rejoin data
  data %<>%
    full_join(Unfoundnames)
  
  
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
  
  #Filter non-members while working
  data %<>%
    filter( ! str_detect(FROM, "Pierluisi|Bordallo|Norton|Faleomavaega|Christensen|Representative non Congressional|
                         Representative Non-Congressional|Representative NonCongressional|Representative NY-25"))
  
 
  Unfoundnames2 <- data %>%
    filter(is.na(last_name),
           is.na(ERROR), 
           is.na(NOTES),
           str_detect(pattern, "404error"))
  

  
   #Check after run through merge
#Unmatched <- d %>%
 #filter(is.na(bioname))
  
  

  
 return(data)
  
}

  