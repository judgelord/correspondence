# This script defines a function to clean google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables based on agency-specific information

# file.name <- "DOD_Navy Delaney" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column
  data$agency <- file.name
  
  
  # Format date, year, Congress, member name etc.
  data$DATEoriginal <- data$DATE
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  
  #Create variable for chamber (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Sen|SEN", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Rep|REP", FROM), "House", chamber))
  
  # clean up from column
  data$FROM %<>% str_remove_all("-")
  data$FROM %<>% cleanFROMcolumn()
  
  # split multiple members separated by a / (there are only a few, and some subject content is in the FROM column, to correct by hand)
  data %<>% 
    mutate(FROM = str_split(FROM, "/")) %>% 
    unnest(FROM)
  
  # create variable for first and last name
  data %<>%
    # if there is only one word in FROM, format it as a last name
    mutate(last_name = ifelse(grepl('^(\\w+)$',FROM), FROM, NA)) 
  
  data$last_name <- formatLastName(data, "last_name")
  
  data$last_name %<>% 
    str_replace("STOPLIGHT MCCAIN SERGEANT", "MCCAIN") %>% 
    str_remove("REP |SEN |Sen |Rep ") 
  
  # add a first name
  data$first_name <- NA
  data$first_name <- addFirst(data$first_name, data$last_name)
  
  # for these observations, replace FROM with the combined first and last
  data %<>% 
    mutate(FROM = ifelse(!is.na(first_name), paste(first_name, last_name), FROM)) %>% 
    # drop these first and last columns to get new ones from extractMemberName
    select(-first_name, -last_name)
  

  # specific corrections
  data$FROM %<>% 
    str_replace("BORDALLO,LEINE", "BORDALLO, LEINE") %>%
    str_replace("SENSENBRENNER, F JAMES", "SENSENBRENNER, JAMES")
    
    
  
  data %<>% extractMemberName(members, 'FROM')
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  bad_dates <- data %>% 
    filter(is.na(DATE)) %>% 
    select(FROM, string, DATEoriginal)
  
  
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
