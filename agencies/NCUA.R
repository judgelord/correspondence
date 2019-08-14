# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "NCUA" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking for dates that are NA
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  # Pre process FROM column
  data$FROM <- gsub("Senator |Sen |Congressman ", "", data$FROM)
  
  # create first and last name variables
  data <- extractMemberName(data, members, 'FROM')
  
  #data <- getFirstLast.Comma(data2, 'FROM')
   
  data %<>%
    mutate(first_name = ifelse(data$last_name %in% members$last_name, data$first_name  , data2$first_name  )) %>% 
    mutate(last_name = ifelse(data$last_name %in% members$last_name, data$last_name , data2$last_name))
 
  data %<>%
    mutate(last_name = ifelse(grepl("^\\w+$", FROM), formatLastName(data, 'FROM'), last_name))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
  return(data)  
  
}






