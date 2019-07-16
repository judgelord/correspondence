# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "NARA" # for testing

#file.name <-"NARA"  #retested on 17 June for NA dates

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
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Neugebauer", "Randy Neugebauer"))


  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #test for unmatched dates
  #data %<>% filter(congress<0)
  
  
  #Fixed some names causing duplicates
  data %<>%
    mutate(FROM = ifelse(FROM == "Rep. Steve Daines", str_replace(FROM, "Rep. Steve Daines", "Representative DAINES"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Sen. Steve Daines", str_replace(FROM, "Sen. Steve Daines", "Senator DAINES"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Rep. Kyrsten Sinema", str_replace(FROM, "Rep. Kyrsten Sinema", "Representative SINEMA"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Sen. Kyrsten Sinema", str_replace(FROM, "Sen. Kyrsten Sinema", "Senator SINEMA"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Sen. Todd Young", str_replace(FROM, "Sen. Todd Young", "Senator YOUNG"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "Sen. Cory Gardner", str_replace(FROM, "Sen. Cory Gardner", "Senator GARDNER"), FROM))
    
    


  # create first and last name variables
  data %<>% extractMemberName(members, 'FROM')
  
  
  
  #Check for duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE, FROM) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "),
           first_name = str_c(first_name, collapse = "; ")) 
  
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Rep. Donna Christensen|Del. Madeleine Bordallo"), "Non Voting Member", ERROR))
  
  data %<>%
    mutate(NOTES = ifelse(FROM == "Rep. Tom Rice" & is.na(last_name), "Duplicate Tom Rice", NOTES))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES))  
  

   # 
   # sample <- data %>%
   # filter(is.na(DATE))
   # View(sample)
  ##for testing date and names
  
  #filter(is.na(last_name)) %>%
   # count(last_names,sample) %>%
    #arrange(-n)
##reference code
  
  
  
  # arrange columns for hand coding
  data %<>% select(DATE, FROM, SUBJECT, everything())
}






