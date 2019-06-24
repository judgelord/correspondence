# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "CNCS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  #Format Date
  data$tempDATE<- data$DATE %>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = ifelse(is.na(tempDATE), Out, DATE))
  data$DATE %<>% as.Date("%m/%d/%y")
 
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and|Rep. |Sen. |(S), |(CW), |(CM), ")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen. |\\(S\\)|Sen "), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Rep. |\\(CW\\)|\\(CM\\)|Rep |Sens. |Reps. "), "House", chamber))
  
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen. |\\)")) %>%
    mutate(FROM = str_remove(FROM, "Rep. |\\)|\\(|Reps |Sen | NJ| CM| CW|Senator |\\(CW\\)|\\(CM\\)|Rep |Sens. |Reps. | NY-19"))
  
  data %<>%
    mutate(str_replace(FROM, "Thompson Glen  \"GT\"", "Thompson Glen"))
             
  
  data %<>% select(ID, DATE, FROM, everything())  

  
  data <- getFirstLast.Comma(data, 'FROM')
  
  data %<>% filter(!FROM == "")
  
  #Format last name and put in last_name  
  data %<>%
    mutate(last_name = ifelse(! str_detect(FROM, "\\,|\\.") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  #data %<>%
   # mutate(FROM = ifelse(! str_detect(FROM, "\\,|\\.") & is.na(last_name), casefold(FROM, upper = TRUE), FROM)) %>%
    #mutate(last_name = ifelse(! str_detect(FROM, "\\,|\\.") & is.na(last_name), FROM, last_name))
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(Title, "Multi"), "Multiple unnamed members", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(Title, "Gov"), "State Governor", ERROR))
  
  Unfound <- data %>%
    filter(is.na(last_name))
  
  #Check after run through merge
  #Unfoundnames <- d %>%
  #filter(is.na(bioname))
  
  return(data)
  
}
    