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
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #Fix NA dates
  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %<>% select(ID, DATE, FROM, everything())  
  
  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and")) %>%
    unnest(FROM)
  
  return(data)
  
}
    