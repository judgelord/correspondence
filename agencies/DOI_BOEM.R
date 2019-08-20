# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOI_BOEM Aaron" # for testing


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
  
  # Format date, year, Congress, member name etc.
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  # add in notes if a number of unspecified congressman contributed
  data %<>%
    mutate(NOTES = ifelse(grepl("other|members", FROM, ignore.case = TRUE), paste(NOTES, FROM), NOTES))

  #String Split for Multiple Members
  data %<>%
    mutate(FROM = str_split(FROM, ";")) %>%
    mutate(FROM = str_remove_all(FROM, "MOC ")) %>%
    unnest(FROM)
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  return(data)
}