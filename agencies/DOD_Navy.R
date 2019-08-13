# This script defines a function to clean google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables based on agency-specific information

# file.name <- "DOD_Navy Delaney" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # Remove NA Observations
  data <- data[!is.na(data$FROM),]
  
  # create agency column
  data$agency <- file.name
  
  
  # Format date, year, Congress, member name etc.
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  
  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen|SEN", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep|REP", FROM), "House", chamber))
  
  data$FROM <- gsub("--", " ", data$FROM)
  
  # create variable for first and last name
  #data <- getFirstLast.Comma(data, "FROM")
  
  #getFirstLast runs better than extractMemberName
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  data %<>%
    mutate(last_name = ifelse(grepl('^(\\w+)$',FROM2), FROM, last_name))
  
  data$first_name <- addFirst(data$first_name,data$last_name)
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data$NOTES %<>% as.character()
  
  data %<>% 
    mutate(TYPE =
             ifelse(!grepl("[0-9]", TYPE) & grepl(
               "DECEASED|MEDAL|ENLIST|DISCHARGE|SPOUSE|RIBBON|ASSAULT|DISABILITY|RETIREMENT|WIFE|FAMILY|BENEFITS|DEPENDENT|REQUEST.*RECORDS|MEDICAL|PURPLE HEART|FOIA|Elgibility|Scholarship|Death|Constituent|DD|Security Clearance|Travel Pay|Spousal|Job|His Application|His Pending|Would Like|Awards|Deck Logs|Records Request", 
               SUBJECT, ignore.case = TRUE), 
               1, TYPE))
  data %<>%
    mutate(CERTAINTY =
             ifelse(!grepl("[0-9]", CERTAINTY) & grepl(
               "DECEASED|MEDAL|ENLIST|DISCHARGE|SPOUSE|RIBBON|ASSAULT|DISABILITY|RETIREMENT|WIFE|FAMILY|BENEFITS|DEPENDENET|REQUEST.*RECORDS|MEDICAL|PURPLE HEART|FOIA|Elgibility|Scholarship|Death|Constituent|DD|Security Clearance|Travel Pay|Spousal|Job|His Application|His Pending|Would Like|Awards|Deck Logs", 
               SUBJECT, ignore.case = TRUE), 
               1, CERTAINTY))
  data %<>%
    mutate(TYPE =
             ifelse(!grepl("[0-9]", TYPE) & grepl("STOPLIGHT",
                    SUBJECT, ignore.case = TRUE),
                    5, TYPE))
  data %<>%
    mutate(CERTAINTY =
             ifelse(!grepl("[0-9]", CERTAINTY) & grepl("STOPLIGHT|Records Request",
                    SUBJECT, ignore.case = TRUE),
                    2, CERTAINTY))
  data %<>%
    mutate(TYPE =
             ifelse(!grepl("[0-9]", TYPE) & grepl("COMMITTEE|Policy",
                          SUBJECT, ignore.case = TRUE),
                    5, TYPE))
  data %<>%
    mutate(CERTAINTY =
             ifelse(grepl("COMMITTEE|Policy",
                          SUBJECT, ignore.case = TRUE),
                  1, CERTAINTY))
  data %<>%
    mutate(TYPE =
             ifelse(!grepl("[0-9]", TYPE) & grepl("EMPLOYMENT",
              SUBJECT, ignore.case = TRUE),
                1, TYPE))
  data %<>%
    mutate(CERTAINTY =
             ifelse(!grepl("[0-9]", CERTAINTY) & grepl("EMPLOYMENT",
                          SUBJECT, ignore.case = TRUE),
                    2, CERTAINTY))
  
  


  return(data)
  
} # end function
