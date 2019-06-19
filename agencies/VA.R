#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "VA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  

  #Create ID
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name 

  #Format Date
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date Inquiry Assigned`, DATE))
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  #Filter out rows without data
  data %<>% filter(!FROM == "")
  data %<>% filter(!FROM == "N/A") %>%
    filter(! FROM == "n/a") %>%
    filter(! FROM == "N/a")
  
  #sample
  sampledata <- data[sample(1:nrow(data), 10000, replace=FALSE),]


  #Trim White Space
  sampledata %<>%
    mutate(FROM = str_trim(FROM))
  

  #Run extractMemberName on names with first initial
  initial <- sampledata %>%
    filter(str_detect(FROM, " ")) %>%
    extractMemberName(members = members, col_name = "FROM")
  
  initialNA <- initial %>%
    filter(is.na(last_name))
  
  #Filter for those without initial and run extract and rejoin to data
  sampledata %<>%
    filter(! str_detect(FROM, " ")) %>%
    full_join(initial)
  
  #Format last name and put in last_name  
  sampledata %<>%
    mutate(FROM = ifelse(str_detect(FROM, ", |. |.| |,") & is.na(last_name), str_remove(FROM, " .*|,.*|\\..*"), FROM)) %>%
    mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(sampledata, 'FROM'), last_name))
   
  
  
  dataNA <- sampledata %>%
    filter(is.na(last_name))
  
  

  
 return(sampledata)
  
}
  