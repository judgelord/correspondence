# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Minor spelling stuff, format rows and subjects yet

#file.name <- "DOC_OS" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  data$ID <- c(1:nrow(data))
  
  #create agency, date, congress columns
  data$agency <- file.name 
   data$DATE %<>% as.Date("%m/%d/%y")
   data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
   data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
   
  
  # create variable for first and last name
   # apply extractmembername from legislators package 
   data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
   
   # old ID still used in some places
   if(!"ID" %in% names(data)){
     data %<>% mutate(ID = data_id)
   }
   
  
  
  
  
  
return(data) 
  
  
  
}