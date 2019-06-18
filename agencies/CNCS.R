# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "CNCS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create ID
  data %<>%
    mutate(ID = row_number())

  
  data %<>%
    mutate(DATE = ifelse(is.na(DATE), Out, DATE))


  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #data %<>%
    #mutate(FROM = ifelse(str_replace()))
  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and|Rep. |Sen. |(S), |(CW), |(CM), ")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. |(S)|Sen "), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. |(CW)|(CM)|Rep "), "House", chamber))
  
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen. ")) %>%
    mutate(FROM = str_remove(FROM, "Rep. |\\)|\\(|Reps |Sen "))
             
  
  data %<>% select(ID, DATE, FROM, everything())  

  
  data <- getFirstLast.Comma(data, 'FROM')
  
  data %<>% filter(!FROM == "")
  
  Unfound <- data %>%
    filter(is.na(last_name))
  
  #Extracts member names from NAs in getfirstlast
  Unfoundnames <- data %>%
    filter(is.na(last_name)) %>%
    extractMemberName(members = members, col_name = "FROM")
  
  #Drops duplicate observations  
  data %<>%
    drop_na(last_name)
  
  #Rejoins data
  data %<>%
    full_join(Unfoundnames)
  
  #Checks for observations still NA
  notfound2 <- data %>%
    filter(is.na(last_name))
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(Title, "Multi"), "Multiple unnamed members", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(Title, "Gov"), "State Governor", ERROR))
  
  return(data)
  
}
    