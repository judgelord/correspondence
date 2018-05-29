# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



#file.name <- "DOT_FHWA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE <- data$CompletedDate %>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- paste(data$FName, " ", data$LName )
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("United States Senate|Senate", Organization), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("U.S. House of Representatives|House|Representatives", Organization), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
 
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, everything())
}