# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Minor spelling stuff, format rows and subjects yet

#file.name <- "DOC_OCPA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name 
  
  
   data$DATE %<>% as.Date("%m/%d/%Y")
   data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
   data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
   
  
  # create variable for first and last name
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>% mutate(chamber = ifelse(grepl("Sen. ", FROM), "Senate", NA))
  data %<>% mutate(chamber = ifelse(grepl("Rep. ", FROM), "House", chamber))
  
  # paste all subject content 
  data %<>% mutate(SUBJECT = paste(`ACTION TYPE`, SUBJECT, `ADDITIONAL NOTES`, staffer, stafferCONTACT_INFO, ACTIONS, STATUS ))
  
  
  

  
return(data)  
  
  
  
  
  
  
}