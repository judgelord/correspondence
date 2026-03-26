# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOI_BIA Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 2:(nrow(data)+1) 
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name 
  
  data %<>% mutate(DATE = ifelse(is.na(DATE), `Input Date`, DATE))
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  
  # #checking for Nodates
  # NOdate <- data %>%
  #   filter(is.na(DATE))
  # NOdate %>% select(DATE, `Input Date`)
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, ", ")) %>% 
    unnest(FROM) %>%
    mutate(FROM = str_squish(FROM))
  ################
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  #Membership Errors
  NonMembers <- . %>%
    str_detect("\\(b\\) \\(6\\)")
  
  
  data %<>% 
    # mutate(ERROR = ifelse(NonVotingMembers(FROM), "Non-voting member", ERROR))  %>% 
    # mutate(ERROR = ifelse(StatePoliticians(FROM), "State Politician", ERROR)) %>% 
    mutate(ERROR = ifelse(NonMembers(FROM), "Non-Member", ERROR)) #%>% .$ERROR %>% unique()
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  Unfoundnames %>% 
    group_by(FROM) %>% 
    summarise(congress = str_c(congress, collapse = ";")) %>% distinct()  %>% kable
  
  return(data)
}
