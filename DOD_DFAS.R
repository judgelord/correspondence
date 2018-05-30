# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 657 out of 757 matches on last_name. GO back to fix spelling and other errors.

#file.name <- "DOD_DFAS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", FROM, ignore.case = TRUE), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", FROM, ignore.case = TRUE), "House", chamber)) 

  
  # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber,  everything())
}