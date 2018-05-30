# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# last name only matched on 659 of 709 observations. Some aren't last names (e.g. CONGRESS) and 
# there is probably spelling errors and missed matches 


 # file.name <- "USDA_RMA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m-%d-%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###     ###     ###
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(",", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ",") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ","))
      
      data <- rbind(data, new)
      
    }
  }
  for(i in 1:nrow(data)){
    if(grepl("/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(",", data$FROM),] # removes orginal row with all data
  data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  ###     ###     ###
  
 
  
  # create variable for last name
  data$last_name <- formatLastName(data, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
}