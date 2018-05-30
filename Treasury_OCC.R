# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 1235 out of 1272 observations matched by last name. Fix spelling errors. 

#file.name <- "Treasury_OCC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <-  c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE <- data$`Date Received or Meeting Date` %>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create chamber variable
  data %<>%
    mutate(chamber = ifelse(!is.na(Senator), "Senate", NA)) %>% 
    mutate(chamber = ifelse(!is.na(`House Member`), "House", chamber))
  
  
  
  data %<>% 
    mutate(FROM = Senator) %>% 
    mutate(FROM = ifelse(is.na(Senator), `House Member`, FROM))
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";#\\d{+};#", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";#\\d{1,3};#") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";#\\d{1,3};#"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";#\\d{+};", data$FROM),] # removes orginal row with all data
  ################
  
  
  
  
  # create variable for first and last name
 data <- getFirstLast.Comma(data, "FROM")
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())
}