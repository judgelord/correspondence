# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Less than 100 obs
# needs work

# file.name <- "FCA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  
  # only SUBJECT contains useful data
  #data %<>% select(FROM, SUBJECT, DATE, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR) %>% distinct() 
  
  #create agency column
  data$agency <- file.name 
  


  # Format date, year, Congress
  data$DATE <-  str_extract(data$SUBJECT, "[0-9][0-9]\\/[0-2][0-9]\\/[1-2][0,9][0,1,2,9][0-9]")
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
 # # data$FROM <- gsub("^.*\\/.... .*(Senator|Sen\\.|Congressman|Rep|Rep\\.|Cong|Cong\\.|Congress) (\\w{+}) .*$", '\\2', data$SUBJECT)
 #   data$FROM <- gsub("^.*(Senator|Sen\\.|Congressman|Rep|Rep\\.|Cong|Cong\\.|Congress) (\\w{+}) .*$", '\\2', data$SUBJECT)
 #  
 #  data$FROM <- gsub("'s", "", data$FROM)
 #  
 #  
 #  # a few cases only have last names, so we correct
 #  data$last <- formatLastName(data, 'FROM')
 #  data$first <- addFirst(first_name = NA, last_name = data$last)
 # 
 #  data %<>% 
 #    mutate(FROM = ifelse(str_detect(FROM, "^[A-z][a-z]*$"), str_c(last, first, sep = ", "), FROM))
  
  # format subject to match patterns for senator ____, (must end in comma)
  data %<>% 
    mutate(SUBJECT = str_replace_all(SUBJECT, "Sen\\.", "Senator")) %>% 
    mutate(SUBJECT = str_replace_all(SUBJECT, "Cong\\.", "Representative")) %>% 
    # remove possessive
    mutate(SUBJECT = str_replace_all(SUBJECT, "'s", ",")) 

  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senate|Senator", SUBJECT), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Congress|Cong |Cong\\.|Rep |Rep\\.|Represe|House", SUBJECT), "House", chamber)) 
  
  
  # create variable for first and last name
  data %<>% mutate(FROM = SUBJECT)
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  

  unfoundnames <- data %>% 
    filter(is.na(last_name)) %>% 
    select(-first_name, -last_name) %>% 
    # add commas after member last names so pattern matches 
    mutate(SUBJECT = str_replace_all(SUBJECT, "(Senator|Representative) (\\w+)", "\\1 \\2,")) %>% 
    extractMemberName(col_name = 'SUBJECT', congress = "congress") 
  
  data %<>% 
    # remove found names
    anti_join(unfoundnames %>% filter(!is.na(last_name)) %>% select(ID)) %>% 
    # merge in found names 
    full_join(unfoundnames)
  
  

  # arrange columns for hand coding
  data %<>% select(ID, DATE,  SUBJECT,  everything())
  
  return(data)
}
