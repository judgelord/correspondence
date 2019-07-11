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
  data$DATE %<>% multidate( c("%m-%d-%y", "%m/%d/%Y","%m/%d/%y"))


  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #test for unmatched dates
  #data %<>% filter(congress<0)

  # create first and last name variables
  data %<>% extractMemberName(members, 'FROM')
  
  

  
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






