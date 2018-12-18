# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "OSMRE" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name 
  
  
  data$DATE <-  as.Date(data$Received, "%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- data$`Sent From`
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl("/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  # create variable for first and last name
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>%
    mutate(last_name = ifelse(grepl("^(\\w+)$",FROM), gsub("^(\\w+)$", '\\1',FROM),last_name))
  data$last_name <- formatLastName(data, 'last_name')
  
  data$first_name <- addFirst(data$first_name,data$last_name)
  
  
  # ERRORS
  data %<>%
    mutate(ERROR = ifelse(grepl("\\d",FROM), "Other unspecified members of congress on this letter",ERROR))
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  
  
  
  
  
  
  
}