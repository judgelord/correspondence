# This script defines a function to clean google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables based on agency-specific information


#file.name <- "DOD_Navy Delaney" # for testing


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
  
  # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")
  
  data2 <- data[is.na(data$last_name),]
  
  # for (i in 1:length(members$id)) {
  #   data %<>% mutate(first_name = ifelse( !is.na(members$common_name[i]) & data$first_name == members$common_name[i] & data$last_name == members$last_name[i],
  #                                         members$first_name[i], data$first_name))
  #   
  # }
  
  # #create variable for last name of the Sen/Rep
  # data %<>%
  #   mutate(
  #     last_name = gsub(
  #       pattern = "(REP|SEN)(.|- | - |. )(\\w+)(,| ,).*",
  #       replacement = "\\3",
  #       x = FROM
  #     )
  #   ) %>%
  #   mutate(last_name = ifelse(is.na(chamber), NA, last_name)) %>%
  #   mutate(last_name = ifelse(grepl("\\W", last_name), NA, last_name)) %>% 
  #   mutate(last_name = toupper(last_name)) 
  # 
  # 
  # 
  # #create variable for first name of the Sen/Rep
  # data %<>%
  #   mutate(first_name = gsub(
  #     pattern = ".*, (\\w+).*",
  #     replacement = "\\1",
  #     x = FROM
  #   )) %>%
  #   mutate(first_name = ifelse(is.na(chamber), NA, first_name)) %>%
  #   mutate(first_name = ifelse(grepl("\\W", first_name), NA, first_name)) %>% 
  #   mutate(first_name = stri_trans_totitle(first_name))
  
  
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
