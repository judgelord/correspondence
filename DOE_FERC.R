# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

file.name <- "DOE_FERC" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID column
  names(data)[names(data) == 'X1'] <- 'ID'
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("(^.*\\d{4})\n.*",  '\\1', data$Date)
  #data$date_received <- gsub("(^.*\\d{4})\n(.*)",  '\\2', data$Date)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #data$FROM <- gsub(pattern = '(US|United States) (Representative) (\\w+ \\w+).*', replacement = "\\3", data$Summary)
  
 # grepl("(Senate|Senator)", data$Summary) &grepl("Represenatative|Congressman|Congresswoman", data$Summary)
  
  data %<>%
    mutate(chamber = ifelse(grepl("(Senate|Senator)",Summary), 'Senate', NA)) %>% 
    mutate(chamber = ifelse(grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman", Summary), "House", chamber))# %>% 
   # mutate(chamber = grepl("(Senate|Senator)", data$Summary) &grepl("Represenatative|Congressman|Congresswoman", data$Summary), 'FIXME', chamber )
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
}

