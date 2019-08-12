# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "DOT_FAA Sam" # for testing
 

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE <- multidate(data$DATE, c("%d-%b-%y","%B %d, %Y"))
  
  #checking for dates that are NA
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # Add semi colons in rows with multiple congressman
  data$FROM <- gsub("(Senate|Representatives)  (\\w+,)","\\1;\\2", data$FROM, ignore.case = T)
  data$FROM <- gsub("(Infrastructure|Aviation|Transportation|Technology|Reform)  (\\w+,)","\\1;\\2", data$FROM, ignore.case = T)
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
  mutate(FROM = str_split(FROM, ";")) %>%
  unnest(FROM)
  
  ################
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace_all(FROM, " ,", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, " , ", ", "))
    
    
  # data <- getFirstLast.Comma(data, 'FROM')
  
  ##extractmemberName takes longer and is worse than getFirstLast
  ## IS THIS TRUE ????
  
  data %<>% extractMemberName(members, 'FROM')
  
  # #Create variable for chamber position  (Senator or Representative)
  # data %<>%
  #   mutate(chamber = ifelse (grepl("Senator|Senate", FROM), "Senate", NA)) %>% 
  #   mutate(chamber = ifelse(grepl("Representative", FROM), "House", chamber)) %>% 
  #   mutate(chamber = ifelse(grepl("Representative", assigned), "House", chamber)) %>% 
  #   mutate(chamber = ifelse(grepl("Senate", assigned), "Senate", chamber)) 
  # 
  #create variable for state
  
  data %<>% 
    mutate(state = ifelse(grepl(".*\\w{1,}(/|-)(\\w{2})( |)($| U\\.S\\.| United)", FROM), gsub(".*\\w{1,}(/|-)(\\w{2})( |)($| U\\.S\\.| United).*", replacement="\\2", FROM), NA))
  data$state = stateFromLower(data$state)
  
  
  # ERROR
  data %<>%
    mutate(ERROR = ifelse(grepl("FAA Employee",FROM, ignore.case = T), "FAA Employee", ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  

  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data %>%
    filter(ID == 17) %>%
    select(FROM)
    

  return(data)
}



