# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# matched on last_name perfectly, only last_name and chamber info

 #file.name <- "DOI_USGS Hope" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read()
  data %<>% select(-first_name)
  nrow(data)
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  nrow(data)
  
  #create agency column
  data$agency <- file.name
  
  # inspect rows where date fails 
  data$DATEoriginal <- data$DATE
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% 
    str_replace("/200", "/0") %>% 
    str_replace("/201", "/1") %>% 
    multidate(c("%m/%d/%y", "%d-%b-%y"))
  
  data %>% filter(is.na(DATE)) %>% select(DATE, DATEoriginal, SUBJECT, `Last Name`)
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %>% filter(year<2007|year>2019) %>% select(DATE, DATEoriginal)
  

  data$last_name <- toupper(data$'Last Name')
  data %<>% add_first()
  data$FROM <- paste(data$Salutation, data$first_name, data$last_name) %>% 
    str_replace(" NA ", " ")
  
  data %<>% select(-first_name, -last_name)

  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(`Last Name`),
           is.na(ERROR))  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("Senator|Senate", Salutation), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Representative", Salutation), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(last_name), NA, chamber))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())%>% select(-chamber)

  return(data)
}
# 
# data %>% summarise_all(is.na) %>% summarise_all(sum) %>% gather() %>% kable()
# 
# data %>% mutate(NAs = ifelse(is.na(last_name), "missing", "matched with member")) %>% count(congress, NAs) %>% spread(key = NAs, value = n)
# 
# data %>% mutate(NAs = ifelse(is.na(last_name), "missing", "matched with member")) %>% count(agency, NAs) %>% spread(key = NAs, value = n) %>% kable()
# 
# 
# missing_data <- data %>% mutate(NAs = ifelse(is.na(last_name), "missing", "matched with member")) %>% 
#   add_count(agency, NAs) %>% 
#   filter(NAs == "missing")
# missing_data$FROM
# 
# missing_data %<>% select(agency, DATE, FROM, congress, LetterID, ID, ERROR) %>% extractMemberName(members, "FROM")
# missing_data
# 
