#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "TVA" # for testing

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
  
 
  #Format DATE
  data$DATE %<>% as.Date("%m/%d/%Y %H:%M")
  

  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  
  #String split on ',' & 'and' between multiple members
  data %<>%
    mutate(FROM = str_split(FROM, "\\,| and")) %>%
    unnest(FROM)
  
  #Removes unneeded characters
  data %<>%
    mutate(FROM = str_remove(FROM, "\\,| and"))
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Congressman|Rep.|Con. |con. "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(SUBJECT, "Sen |Sen."), "Senate", chamber))
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }

  
  #Checks for NAs
  Unfoundnames <- data %>%
    filter(is.na(last_name)) 
  
  Unfoundnames %<>%
    extractMemberName(col_name = "SUBJECT", congress = "congress") %>%
    drop_na(last_name)

  data %<>%
    full_join(Unfoundnames)
  
  #Error for state leg
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Reginald Tate|Jimmy Matlock|Arthur Orr|Brent Yonts|Melinda G Prunty"), "State Legislator", ERROR))

  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Duncan") & is.na(first_name), "Multiple Duncan's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Markey") & is.na(first_name), "Multiple Markey's FOIA", NOTES))
  
  data %<>% select(ID, DATE,  FROM, last_name, chamber, SUBJECT, everything())
  
  #Check unmatching after merge
  #Unfoundmerge <- d %>%
    #filter(is.na(bioname))
  
  return(data)
  
}
  