# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "NASA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # format DATE to multiple formats
  data$DATE <- multidate(data$DATE, c("%d-%b-%y", "%b %d,%Y"))
   
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # create LetterID variable
  data$LetterID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name

 
  
  #Makes note for multiple authors
  data %<>%
    mutate(NOTES = ifelse(str_detect(chamber, "HOUSE AND SENATE"), "Multiple members", NOTES))
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  
  data %<>%
    mutate(FROM = str_split(FROM, ",")) %>%
    unnest(FROM)
  

  data$FROM <- gsub("House|Senate|Incoming", "", data$FROM, ignore.case = TRUE)
  ################
  
  
  # chamber
  data$chamber <- ifelse(data$chamber == "HOUSE", 'House', data$chamber)
  data$chamber <- ifelse(data$chamber == "SENATE", "Senate", data$chamber)
  
  #Create ID variable
  data$ID <- c(1:nrow(data))
  
  # preprocess
  data %<>%
    mutate(FROM = str_remove(FROM, "INFORMATION "))
  
  data$FROM <- gsub("(^| )(EB|E\\.B\\.) ", "Eddie ", data$FROM)
  
  #Extract members from SUBJECT
  Nomembers <- data %>%
    filter(is.na(FROM)) %>%
    extractMemberName(members = members, col_name = "SUBJECT") %>%
    filter( ! str_detect(SUBJECT, " LETTER TO THE HONORABLE ANNA ESHOO ")) %>%
    drop_na(last_name)
  
  #Extract members in FROM
  data <- extractMemberName(data, members, 'FROM')
  
  #Join both datasets
  data %<>%
    full_join(Nomembers)
  
  #Format last_names
  data %<>%
    mutate(last_name = ifelse(is.na(data$last_name), formatLastName(data, 'FROM'), last_name))
  
  #Subset data for observations with no first name and no chamber
  NoChamber <- data %>%
    filter(str_detect(chamber, "HOUSE AND SENATE") & is.na(first_name))
 
   data %<>%
    anti_join(NoChamber)
 
  #Add first name to observations without chamber  
 NoChamber$first_name <- addFirst(NoChamber$first_name,NoChamber$last_name)
 
 # arrange columns for hand coding
 NoChamber %<>% select(ID, DATE, chamber,  FROM, SUBJECT, first_name, last_name, everything())
  
 
 #Rejoin datasets
 data %<>%
   full_join(NoChamber)
  
  
  data$last_name <- gsub("^ |^  | $|  $", "", data$last_name)
  data <- data[!data$last_name == "",] # removes blank observations
  
  data %<>%
    mutate(ERROR = ifelse(grepl("^(AND|STATE)$",FROM), 'Inspect', ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, SUBJECT, first_name, last_name, everything())
  
  
  #Making chamber NA for "HOUSE AND SENATE"
  is.na(data$chamber) <- data$chamber == "HOUSE AND SENATE"

#Check NAs after merge  
#unmatched <- d %>%
 #filter(is.na(bioname))
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
}
