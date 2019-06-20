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
    filter(! FROM == "N/a") %>%
    filter(! FROM == "n/a/") %>%
    filter(! FROM == "m/a") %>%
    filter(! FROM == "`N/A")
  
  data %<>%
    mutate(FROM = str_remove(FROM, " N/A"))
  
  #sample
  #sampledata <- data[sample(1:nrow(data), 10000, replace=FALSE),]


  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))
  
  #filter for multiple authors
  #data <- data %>%
    #filter(str_detect(FROM, "\\/"))
  
  #string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, "\\/")) %>%
    unnest(FROM)

  data %<>%
    mutate(FROM = str_remove(FROM, "\\/"))
  
  #Run extractMemberName on names with first initial
  initial <- data %>%
    filter(str_detect(FROM, " ")) %>%
    extractMemberName(members = members, col_name = "FROM")
  
  initialNA <- initial %>%
    filter(is.na(last_name))
  
  #Filter for those without initial and run extract and rejoin to data
  data %<>%
    filter(! str_detect(FROM, " ")) %>%
    full_join(initial)
  
  #Format last name and put in last_name  
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, ", |. |.| |,") & is.na(last_name), str_remove(FROM, " .*|,.*|\\..*"), FROM))
   
  data %<>%
       mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  #Membership Errors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "SVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "non-cong"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Non-Congressional"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HVAC"), "Not Member", ERROR))
  
   #Check after run through merge
#Unfoundnames <- d %>%
 #filter(is.na(bioname))
  
  

  
 return(data)
  
}
  