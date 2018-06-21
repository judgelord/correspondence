# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



#file.name <- "DOI_NPS2" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data <- data[-which(is.na(data$LNAME)),]
  data <- data[-which(data$LNAME == "LNAME"),]
  
  # create ID variable
  data$ID <- c(1:nrow(data)) 
  
  # create Subject variable
  # data$SUBJECT <- data$SUMMARY
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for full name
  data$FROM <- paste(data$FNAME, data$LNAME, sep = " ")
  data <- extractMemberName(data, members, 'FROM')
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  data%<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
    mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "98% SURE THESE SUBJECTS REPRESENT CERTAIN PEOPLE WORKING FOR THE COMPANIES AND NOT THE COMPANIES THEMSELVES, BUT CAN'T SAY WITH ABSOLUTE CERTAINTY", NOTES)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
  
  
}






