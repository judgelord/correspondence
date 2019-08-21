# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "EOP_USTR" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  data %<>% 
    mutate(DATE = 'Date Received',
           FROM = 'Source',
           SUBJECT = paste(Title, 'Signature(s)'))
  
  # create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date() # FIXME
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  data <- extractMemberName(data, members, 'FROM')
  
  data %<>% select(ID, DATE,  FROM,  everything())
}
