# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "HUD_HQ" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  names(data)[names(data) == 'Folder ID'] <- 'ID'
  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <-  as.Date(data$'Date on Correspondence', "%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Check for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  data$FROM <- data$Correspondent
  
  #Sample Test code
  #sample <- data[sample(1:nrow(data), 3000, replace=FALSE),]
  
  #data <- sample
 
  #Matching on congress to prevent duplicates 
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, "Duncan Hunter") & congress == 114|111|116|115|113|112, str_replace(FROM, "Duncan Hunter", "Duncan Duane HUNTER"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Duncan Hunter") & congress == 110, str_replace(FROM, "Duncan Hunter", "Duncan Lee HUNTER"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Mike Rogers") & congress == 114|115|116, str_replace(FROM, "Mike Rogers", "Mike Dennis ROGERS"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Donald Payne" & congress == 111|110, str_replace(FROM, "Donald Payne", "Donald Milford PAYNE"), FROM)) %>%
   # mutate(FROM = ifelse(str_detect(FROM, "Donald Payne") & congress == 116|115|114|113, str_replace(FROM, "Donald Payne", "Donald Payne, Jr."), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Tim Johnson") & congress == 113, str_replace(FROM, "Tim Johnson", "Timothy Peter JOHNSON"), FROM))
  
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>%
    mutate(ERROR = ifelse(grepl('^Richard Hillman$',FROM), 'Richard Hillman is not a member of Congress', ERROR)) %>% 
    mutate(ERROR = ifelse(grepl('^Aaron Leong$',FROM), 'Aaron Leong is not a member of Congress', ERROR))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, FROM, everything())
  
  #data %<>%
   # mutate(NOTES = ifelse(str_detect(FROM, "Tim Johnson") & str_detect(congress, "110|111|112"), "Multiple Tim Johnson's FOIA", NOTES))
  
  #Filter for Unfoundnames
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  
  return(data)
  
}
