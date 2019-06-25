# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOJ_ENRD" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # create ID variable
  data$LetterID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name

  #Format Date
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Congressman |Rep.|Con. |con. |Congresswoman |MCs "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen |Sen.|Senator "), "Senate", chamber))  
  
  data %<>%
    mutate(FROM = str_replace(FROM, ", Jr.", " Jr.")) %>%
    mutate(FROM = str_replace(FROM, ", II", " II")) %>%
    mutate(FROM = str_replace(FROM, ", Jr", " Jr"))
  
  data %<>%
    mutate(FROM = str_split(FROM, ",")) %>%
    unnest(FROM)
  
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  unfoundnames <- data %>%
    filter(is.na(last_name))
  
 # data %<>%
  #  mutate(NOTES = ifelse(str_detect(FROM, "Davis") & is.na(first_name), "Multiple Davis\\' FOIA", NOTES))
    
  
  
  
  return(data)
  
}
