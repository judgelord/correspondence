# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOI_SOL Hope" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% multidate(formats = c("%m/%d/%y")) # FIXME
  
  # bad.dates <- data %>% filter(is.na(DATE)) %>% .$LetterID
  # data$DATE[bad.dates]
  # data %>% select(LetterID, DATE, FROM, SUBJECT) %>% filter(LetterID %in% bad.dates)
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %<>% mutate(FROM = paste(Salutation, FROM))
  
  data %<>% extractMemberName(members, 'FROM')

  bad <- data %>% filter(is.na(last_name)) %>% .$LetterID
  data %>% filter(LetterID %in% bad) %>% .$FROM %>% str_remove_all("NA")  %>% str_squish() %>% unique()
  data %>% select(LetterID,congress, FROM) %>% filter(LetterID %in% bad)
  
 
  data %<>% select(ID, DATE,  FROM, last_name, everything())
  
  data %>% select(FROM, last_name)

  return(data) 
}