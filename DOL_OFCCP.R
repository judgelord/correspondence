# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# duplicate member names in rows needs to be fixed
# only 15 observations not in member set (half shouldn't be, other half are spelling errors)

 #file.name <- "DOL_OFCCP" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
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
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMPLAINT|WRONGFUL|HARASS|TERMINATION|UNFAIR|DISCRIMINATION|REQUEST|IBM|AFFIRMATIVE ACTION|DISABLED|VIOLATION|ASSISTANCE|LAID OFF|TERMINATED|DENIED|DISABILITY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMPLAINT|WRONGFUL|HARASS|TERMINATION|UNFAIR|DISCRIMINATION|REQUEST|IBM|AFFIRMATIVE ACTION|DISABLED|VIOLATION|ASSISTANCE|LAID OFF|TERMINATED|DENIED|DISABILITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("AFFIRMATIVE ACTION GUIDELINE|WHISTLEBLOWER PROTECTIONS|READJUSTMENT ACT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("AFFIRMATIVE ACTION GUIDELINE|WHISTLEBLOWER PROTECTIONS|READJUSTMENT ACT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("AFFIRMATIVE ACTION GUIDELINE|WHISTLEBLOWER PROTECTIONS", SUBJECT, ignore.case = TRUE), "RULE", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CGI|EMPLOYMENT PRACTICES", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CGI|EMPLOYMENT PRACTICES", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
   
  
  
  
  
  
}
