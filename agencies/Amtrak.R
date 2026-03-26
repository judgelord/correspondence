# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "Amtrak" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$dateoriginal <- data$DATE
  data$DATE <- data$dateoriginal
  data$DATE %<>% #str_replace_all("/", " ") %>% 
    multidate(c("%y %m %d","%m/%d/%y"))
  
  data %>% filter(is.na(DATE)) %>% count(dateoriginal)
  
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
 data %<>% 
   mutate(FROM = str_split(FROM, "/")) %>% 
   unnest(FROM)
  
  # create variable for first and last name
  data$last_name <- formatLastName(data, 'FROM')
  
  data$first_name <- NA
  data$first_name %<>% addFirst(data$last_name)
  
  data %<>% 
    mutate(FROM = paste(chamber, first_name, last_name, State) %>% 
             str_replace("\\bNA\\b", " ") %>% 
             str_replace("Senate", "Senator") %>% 
             str_replace("House", "Representative") %>% str_squish() )
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }  
  #data$state <- stateFromLower(data$State)
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(icpsr),
           is.na(ERROR)) %>%
    count(FROM, congress, sort= T)
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  return(data)
}
