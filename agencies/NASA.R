# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 122 non matches on last_name

  # file.name <- "NASA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  data <- data[-which(is.na(data$FROM)),]
  
  # format DATE to multiple formats
  data$DATE <- multidate(data$DATE, c("%d-%b-%y", "%b %d,%Y"))
   
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name

 
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(",", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ",") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ","))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(",", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("House|Senate|Incoming", "", data$FROM, ignore.case = TRUE)
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  # preprocess
  data$FROM <- gsub("(^| )(EB|E\\.B\\.) ", "Eddie ", data$FROM)
  
  data <- extractMemberName(data, members, 'FROM')
  data %<>%
    mutate(last_name = ifelse(is.na(data$last_name), formatLastName(data, 'FROM'), last_name))

  data$last_name <- gsub("^ |^  | $|  $", "", data$last_name)
  data <- data[!data$last_name == "",] # removes blank observations
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, everything())
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
  
  
  
  
  
}
