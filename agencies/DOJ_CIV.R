# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "DOJ_CIV" # for testing
 
clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  data$ID <- c(1:nrow(data)) 
  
  data <- data[ !(is.na(data$Last.Name)&is.na(data$First.Name)),]
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")

  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #create separate data frame with duplicate memembers (/ formating) and remove those observations from the original
  data2 <- data[grepl("/",data$Last.Name),]
  data <- data[!grepl("/",data$Last.Name),]
  
  # combine first and last name and call name method
  data$FROM <- paste(data$First.Name, data$Last.Name)
  data <- extractMemberName(data, members, 'FROM')
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data2)){
    if(grepl("/", data2$Last.Name[i])) {
      
      new <- data2 %>% dplyr::slice(rep(i, each = str_count(data2$Last.Name[i], pattern = "/") + 1))
      new$Last.Name <- unlist(str_split(data2$Last.Name[i], "/"))
      
      data2 <- rbind(data2, new)
      
    }
  }
  data2 <- data2[-grep("/", data2$Last.Name),] # removes orginal row with all data
  ################
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data2)){
    if(grepl("/", data2$First.Name[i])) {
      
      new <- data2 %>% dplyr::slice(rep(i, each = str_count(data2$First.Name[i], pattern = "/") + 1))
      new$First.Name <- unlist(str_split(data2$First.Name[i], "/"))
      
      data2 <- rbind(data2, new)
      
    }
  }
  data2 <- data2[-grep("/", data2$First.Name),] # removes orginal row with all data
  ################
  
  #combine first and last names and call name method
  data2$FROM <- paste(data2$First.Name, data2$Last.Name)
  data2 <- extractMemberName(data2, members, 'FROM')
  # Remove observations that were not correct first & last matches
  data2 <- data2[ !(is.na(data2$last_name)&is.na(data2$first_name)),]
  
  # merge datasets
  data <- full_join(data,data2)
  
  
  data %<>%
    mutate(first_name = ifelse(data$last_name == "YOUNG", "Bill", data$first_name))  
  data %<>% 
    mutate(first_name = ifelse(data$last_name == "AKIN", "Todd", data$first_name))
  

  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  # Errors for missing data
  data %<>%
    mutate(ERROR = ifelse(data$FROM == "NA NA", "NA FROM information", ERROR))
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RADIATION COMPENSATION|911 VICTIM|REFUND|RETURN|(6)|REQUEST|CLAIM|BREAST", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RADIATION COMPENSATION|911 VICTIM|REFUND|RETURN|(6)|REQUEST|CLAIM|BREAST", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROSECUTION|ILLEGAL DRUG RAIDS|HEARING|DOD|REQ|NAVAL AIR STATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROSECUTION|ILLEGAL DRUG RAIDS|HEARING|DOD|REQ|NAVAL AIR STATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROSECUTION", SUBJECT, ignore.case = TRUE), "ENFORCEMENT", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOBACCO", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOBACCO", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
  
  
}
