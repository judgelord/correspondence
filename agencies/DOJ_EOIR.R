# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOJ_EOIR" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

# create agency column
  data$agency <- file.name
  
  #Format date
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Con "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen "), "Senate", chamber))
  
  #Splits Rows with multiple authors
  data %<>%
    mutate(FROM = str_split(FROM, " and |&")) %>%
    unnest(FROM)
  
  data %<>%
    extractMemberName(members = members, col_name = "FROM") %>% 
    select(DATE, FROM, first_name, last_name, SUBJECT, everything())
  
  
  
  
  
  
  
  
  
  return(data)
  
}