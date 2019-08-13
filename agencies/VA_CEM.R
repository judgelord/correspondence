# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "VA_CEM" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data %<>% 
    mutate(DATE = `Date Received`,
           FROM = `Primary Person`,
           SUBJECT = Subject)
  
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))

  # created chamber variable
  data %<>%
  mutate(chamber = ifelse(!is.na("Senator"), "Senate", NA)) %>% 
  mutate(chamber = ifelse(!is.na("House Member"), "House", chamber))  
  
  data %<>% mutate(ERROR = ifelse(grepl("Randy Reeves", FROM), "Under Secretary", ERROR))
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  #sample <- data %>%
  #filter(is.na(first_name))  
  #View(sample) 
  ## code for testing
  
  return(data)
}
