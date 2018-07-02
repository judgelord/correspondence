# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 63 mismatches


file.name <- "RRB" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%d-%b-%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for full name
  data$FROM <- gsub("Tanko", "Tonko", data$FROM)
  data <- extractMemberName(data, members,"FROM")
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("APPLICATION|CALL REQUEST|OPERATION|LETTER|EMAIL|CIS|CALL|H&A|OLA", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("APPLICATION|CALL REQUEST|OPERATION|LETTER|EMAIL|CIS|CALL|H&A|OLA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
  
  
  
  
  
  
  
  
  
}






