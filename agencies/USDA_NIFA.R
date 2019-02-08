# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA_NIFA" # for testing





clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- as.Date(data$`DATE RECEIVED`, "%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";| and ", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";| and ") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";| and "))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";| and ", data$FROM),] # removes orginal row with all data
  ################
  

 # Add Errors for non members
  data %<>% mutate(ERROR = ifelse(grepl("Congressional Research Service",FROM), "Congressional Research Service",ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("Senate Agriculture Appropriations Subcommittee Majority Staff",FROM), "Senate Agriculture Appropriations Subcommittee Majority Staff",ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("House Agriculture Appropriations Committee",FROM), "House Agriculture Appropriations Committee",ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("Senate Committee on Agriculture",FROM), "Senate Committee on Agriculture",ERROR))
  data <- data[!(is.na(data$FROM)),]
  
  
  
  data <- extractMemberName(data, members, 'FROM')

  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
  
  
}








