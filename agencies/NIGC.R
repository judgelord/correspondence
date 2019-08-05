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
    mutate(SUBJECT = SUBJECT,
           DATE = DATE,
           FROM = FROM)  %>%
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
  
  #finding unfound dates 
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  #Create Chamber Variable
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. |\\(S\\)|Sen |Sens. |"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. |\\(CW\\)|\\(CM\\)|Rep |Reps. "), "House", chamber)) 
  
  #String split on ',' & 'and' between multiple members
  data %<>%
    mutate(FROM = str_split(FROM, "\\,| and")) %>%
    unnest(FROM)
  
  #string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, "\\/")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(FROM = str_remove(FROM, "\\/")) %>%
    unnest(FROM)
  
  
  #Removes unneeded characters
  data %<>%
    mutate(FROM = str_remove(FROM, "\\,| and"))
  
  
  #Remove in FROM
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen. |\\)")) %>%
    mutate(FROM = str_remove_all(FROM, "Rep. |\\)|\\(|Reps |Sen | NJ| CM| CW|Senator |\\(CW\\)|\\(CM\\)|Rep |Sens. |Reps. | NY-19| CM|\\(|AK-2|AK-4"))
  
  
  #test for unmatched dates
  #data %<>% filter(congress<0)

  
  
  # create first and last name variables
  data %<>% extractMemberName(members, 'FROM')
  
  #Format last name and put in last_name  
  data %<>%
    mutate(FROM = str_trim(FROM)) %>%
    mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  #Add first name 
  data %<>%
    mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name))
  
  #Error for nonmembers
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Ken Rooney|Michon Johnson"), "Non members of Congress", ERROR))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES))

  
  # arrange columns for hand coding
  data %<>% select(DATE, FROM, SUBJECT, everything())
  
  return(data)
}

