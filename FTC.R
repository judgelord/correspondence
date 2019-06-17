# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FTC" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()# get data

#Create ID
  data %<>%
    mutate(ID = row_number())
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  data %<>% select(ID, DATE, FROM, everything())  

  #No chamber variable in script because chambers may be wrong
  
  data %<>%
    mutate(FROM = str_remove(FROM, "Sen |Rep |!|\\}"))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "Charles E. Schumer", "Schumer, Charles E.")) %>%
    mutate(FROM = str_replace(FROM, "David \"Phil\" Roe", "Roe, David")) %>%
    mutate(FROM = str_replace(FROM, "William \"Mo\" Cowan", "Cowan, William"))

  data <- getFirstLast.Comma(data, col_name = "FROM")
  
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  }