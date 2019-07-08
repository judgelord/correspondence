# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Amtrak" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%y %m %d")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$chamber[data$chamber == "H"] <- "House"
  data$chamber[data$chamber == "S"] <- "Senate"
  data$chamber[data$chamber=="O"] <- "Other"
  
  chamberswitchers <- filter(data, chamber %in% c("H-S","S-H"))
  chamberswitchers$chamber[chamberswitchers$chamber %in% c("H-S","S-H")] <- "Senate"
  data$chamber[data$chamber %in% c("H-S","S-H")] <- "House"
  
  data <- rbind(data, chamberswitchers)
  
  
  ##     ###     ###
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl("/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  data <- data[-grep("--",data$FROM),]
  data <- data[-grep("^\\d", data$FROM),]
  ###     ###     ###
  
  
  # create variable for first and last name
  data$last_name <- formatLastName(data, 'FROM')
  data %<>%
    mutate(last_name = ifelse(last_name %in% members$last_name, last_name,
                              gsub("(^\\w+) .*", replacement = "\\1", last_name )))
  
  
  data$state <- stateFromLower(data$State)
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
}
