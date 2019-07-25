# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "NIGC" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # Make ID column. No duplicated or multi-member letters cases found
  data %<>% 
    rowid_to_column("ID")
  
  # Rename to standard column names 
  data %<>% 
    mutate(SUBJECT = Description,
           DATE = Date,
           FROM = `Member of Congress`)  %>%
    select(DATE, FROM, SUBJECT, everything())
  
  
  # create agency column
  data %<>% 
    mutate(agency = file.name)
  
  
  # Format date, year, Congress, member name etc.
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))
  
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  #Create Chamber Variable
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. |\\(S\\)|Sen |Sens. |"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. |\\(CW\\)|\\(CM\\)|Rep |Reps. "), "House", chamber)) 
  
  #String Split for Multiple Members
  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and|Rep. |Sen. |(S), |(CW), |(CM), ")) %>%
    unnest(FROM)
  
  
  


  #Remove in FROM
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen. |\\)")) %>%
    mutate(FROM = str_remove(FROM, "Rep. |\\)|\\(|Reps |Sen | NJ| CM| CW|Senator |\\(CW\\)|\\(CM\\)|Rep |Sens. |Reps. | NY-19| CM"))
  
  
  #test for unmatched dates
  #data %<>% filter(congress<0)

  
  
  # create first and last name variables
  data %<>% extractMemberName(members, 'FROM')

  
  
  # arrange columns for hand coding
  data %<>% select(DATE, FROM, SUBJECT, everything())
}

