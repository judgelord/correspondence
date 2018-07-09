# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Only last_name info and some state and chamber info
# 24 mismatches on last_name

#file.name <- "PRC" # for testing

clean <- function(file.name) {
  #  get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  data1 <- data
  data1$last_name <- formatLastName(data1, 'Last Name')
  data1$first_name <- formatFirstName(data1, 'First Name')
  
  data2 <- data
  #create last name variable for Sen/Rep
  data2 %<>%
    mutate(last_name = gsub(
      pattern = ".* |.*\\.",
      replacement = "",
      x = FROM
    ))
  data2$last_name <- formatLastName(data2, 'last_name')
  
  data <- left_join(data2, data1)
  
  
  # create agency column
  data$agency <- file.name
  
  #create year and congress columns
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen\\.|Sen |Senator |Sen,", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep\\.|Rep |Representative |Rep,", FROM), "House", chamber))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data %<>% 
  mutate(SUBJECT = paste(SUBJECT,Sub_Issue)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POLICY PLANNING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    
  
  
} # end function
