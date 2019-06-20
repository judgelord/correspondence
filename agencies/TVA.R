#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "TVA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name 
  
  
  #Format Date
  #data %<>% as.Date(strptime(DATE,format = "%m/%d%Y %H:%M"))
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #String split on ',' & 'and' between multiple members
  data %<>%
    mutate(FROM = str_split(FROM, "\\,| and")) %>%
    unnest(FROM)
  
  #Removes unneeded characters
  data %<>%
    mutate(FROM = str_remove(FROM, "\\,| and"))
  
  #extracts member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  #Format last name and put in last_name  
  data %<>%
    mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  return(data)
  
}
  