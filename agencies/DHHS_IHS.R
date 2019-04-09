# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DHHS_IHS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
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
  #data$FROM <- gsub("House|Senate|Incoming", "", data$FROM, ignore.case = TRUE)
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, SUBJECT, FROM,  everything())
}
