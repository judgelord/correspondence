#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EEOC" # for testing

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


  
  data %<>%
    mutate(DATE = str_replace_all(DATE, "JAN", "Jan")) %>%
    mutate(DATE = str_replace_all(DATE, "FEB", "Feb")) %>%
    mutate(DATE = str_replace_all(DATE, "MAR", "Mar")) %>%
    mutate(DATE = str_replace_all(DATE, "APR", "Apr")) %>%
    mutate(DATE = str_replace_all(DATE, "MAY", "May")) %>%
    mutate(DATE = str_replace_all(DATE, "JUN", "Jun")) %>%
    mutate(DATE = str_replace_all(DATE, "JUL", "Jul")) %>%
    mutate(DATE = str_replace_all(DATE, "AUG", "Aug")) %>%
    mutate(DATE = str_replace_all(DATE, "SEP", "Sep")) %>%
    mutate(DATE = str_replace_all(DATE, "OCT", "Oct")) %>%
    mutate(DATE = str_replace_all(DATE, "NOV", "Nov")) %>%
    mutate(DATE = str_replace_all(DATE, "DEC", "Dec"))

  #Format Date
  data$DATE %<>% as.Date("%d-%b-%y")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  data %<>%
    filter(!is.na(DATE))
  
 
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
 
  #Extract member names from SUBJECT
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  
  return(data)
  
}
  