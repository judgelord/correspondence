# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


##file.name <- "DOD_USACE" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read(.default = col_character()) # get data
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$originalDATE <- data$DATE
  data$DATE <- gsub(" .*","", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  # member name
  data %<>% 
    mutate(SUBJECT = paste(Priority, SUBJECT, Owner) %>% str_replace_all(" NA |^NA | NA$", " ")) 
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl(" REP | REPS | Rep ", SUBJECT), "House", NA)) %>%
    mutate(chamber = ifelse(grepl(" SEN | SENS | Sen ", SUBJECT), "Senate", chamber))
  

  data$last_name <- gsub(".* REP |.* REPS|.* SEN |.* SENS |.* CONGRESSIONAL -|.* CONRESSIONAL - |^Routine ","", data$SUBJECT)
  data$last_name <-  toupper(data$last_name)  %>%
  {gsub("^ |^MR. |^MS. ", "", .)}  %>%
  {gsub("-.*| .*|,.*|:.*", "", .)}
  
  data$first_name <- NA
  data$first_name <- addFirst(data$first_name,data$last_name)
  
  data %<>% 
    mutate(FROM = paste(chamber, first_name, last_name)) %>%
    mutate(FROM = str_replace(FROM, "House", "Representative"),
           FROM = str_replace(FROM, "Senate", "Senator"),
           FROM = str_replace_all(FROM, " NA |^NA | NA$", " ")) %>% 
    select(DATE, chamber, first_name, last_name, FROM, SUBJECT, everything())
  
  data %<>% extractMemberName(members, "FROM")
  
  data %<>% mutate(NOTES = ifelse(str_detect(SUBJECT, "multi ", paste("FOIA", NOTES), NOTES)))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(pattern == "404error",
           is.na(ERROR)) 
  

data %<>% select(DATE, originalDATE, SUBJECT, last_name, chamber, everything())  

return(data)
  
  
}