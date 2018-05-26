# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DHHS_ACL" # for testing


# Handful of naming errors, go back and fix

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create ID variable 
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create variable for first and last name
  data <- extractMemberName(data,members,'FROM')
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", FROM), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
    
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
}