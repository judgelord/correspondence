# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# duplicates members in some rows, needs to be fixed


#file.name <- "DOL_VETS" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #rename ID column and remove duplicated observations
  colnames(data)[colnames(data) == 'SIMS ID'] <- 'ID'
  data <- data[!duplicated(data[,c('ID')]),]  
  
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, chamber, everything())
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VETERAN NEEDS ASSISTANCE|WRONGFUL|TERMINATE|USERRA VIOLATION|COMPLAINT|ASSISTANCE|UNFAIR|SUPPORT APPLICATION|GRANT APPLICATION|DISCRIMINATION|REEMPLOYMENT|REINSTATEMENT|PENSION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VETERAN NEEDS ASSISTANCE|WRONGFUL|TERMINATE|USERRA VIOLATION|COMPLAINT|ASSISTANCE|UNFAIR|SUPPORT APPLICATION|GRANT APPLICATION|DISCRIMINATION|REEMPLOYMENT|REINSTATEMENT|PENSION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))  %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LEGISLATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LEGISLATION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("LEGISLATION", SUBJECT, ignore.case = TRUE), "LEGISLATION", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE))
  
  
  
  
  
  
  
  
  
  
  
  
  
}
