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
  sampledata <- data[sample(1:nrow(data), 1800, replace=FALSE),]

  data <- sampledata

  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))
  
  #filter for multiple authors
  #data <- data %>%
    #filter(str_detect(FROM, "\\/"))
 
data %<>%
  mutate(FROM = str_replace(FROM, "VanHollen, C", "Van Hollen, C"))
  # mutate(FROM = str_replace(FROM , "Sullivan, J", "SULLIVAN, John")) %>%
   #mutate(FROM = str_replace(FROM, "Sullivan, D.", "SULLIVAN, Daniel")) %>%
   #mutate(FROM = str_replace(FROM, "Donovan, D", "DONOVAN, Daniel"))
 
 #data <- data %>%
  # filter(str_detect(FROM, "van|Van|VAN"))
  
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
    select(-Summary, -last_name, -first_name)
  
  data %<>%
    anti_join(Unfoundnames)

  
  #Remove first initial to match on chamber_lastname
  Unfoundnames %<>%
    mutate(FROM = str_remove(FROM, ",.*"))
 
 
  #Paste Chamber into FROM
  Unfoundnames %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House"), paste("Congressperson", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM))
  
  Unfoundnames %<>%
    extractMemberName2(members = members, col_name = "FROM")
  
  data %<>%
    full_join(Unfoundnames)
  
  
  #Membership Errors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "SVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "non-cong"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Non-Congressional"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HVAC"), "Not Member", ERROR))
  
  #FOIA NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Young, D."), "Multiple Young's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Miller, G."), "Multiple Miller's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Rogers, M."), "Multiple Rogers' FOIA", NOTES))
  
   #Check after run through merge
#Unfoundnames <- d %>%
 #filter(is.na(bioname))
  
  

  
 return(data)
  
}
  