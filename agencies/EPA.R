# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EPA Aaron" # for testing

clean <- function(file.name) {
  # get data from google drive
  data1 <- gs_title(file.name) %>% gs_read()
  #FIXME "EPA Devin" expands on EPA Julia, which expands on EPA Adam, right? These need to be combined in the clean script calculating inter-coder reliabity
  data2 <- gs_title("EPA Devin") %>% gs_read() 
  
  data <- data2 %>% full_join(data1)
  
  data %<>% group_by(ID) %>% 
    summarise_all(combine_strings)
  
  data$TYPE %>% unique()
  
  #FIXME for now, just take coding from EPA Aaron, where we have corrected dates 
  str_select <- . %>% str_remove(";;;.*")
  
  data %<>% mutate_at(c("TYPE", "ALT_TYPE", "CERTAINTY", "DATE"), str_select)
  
  # check that it worked
  data$TYPE %>% unique()
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
   
  # create agency column
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$originalDate <- data$DATE
  data$DATE %<>% as.Date("%d-%b-%y")
  data$Received %<>% as.Date('%d-%b-%y')
  
  # create year and congress variables
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  # create first and last name variables
  #data <- getFirstLast.Comma(data, 'FROM')
  
  
  
  # create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senate|SENATE", FROM, ignore.case = T), "Senate", NA)) %>%
    mutate(chamber = ifelse(
      grepl("Representative|REPRESENTATIVE|Repesentatives", FROM, ignore.case = T),
      "House",
      chamber
    )) 
  
  # data %<>% mutate(FROM = paste(chamber, FROM))
  
  # create state variable (if given)
  data %<>%
    mutate(state = gsub(
      pattern = ".*Senate-..(\\w{+})/DC.*",
      replacement = "\\1",
      x = FROM
    )) %>%
    mutate(
      state = gsub(
        pattern = ".*House of Represent.*-..(\\w{+})/...*",
        replacement = "\\1",
        x = state
      )
    )
  data %<>%
    mutate(state =  ifelse(grepl(pattern = "\\W+", x = state), NA, state))
  
  
  data$state %<>% stateFromLower()
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  #changing from getfirstlast to extractMemberName
  data <- extractMemberName(data, members, 'FROM')

  
    
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # is the chamber and state causing problems? 
  Unfoundnames %>% count(FROM, string, congress, pattern, chamber, state, sort = T)
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPORT TO CONGRESS|WATERS OF THE US|REQUEST INFORMATION|LEAD IN AMMUNITION|HEARING INVITE|FUEL STANDARD|CLEAN AIR ACT|AGENCY'S|REGARDING FUNDING|QUESTIONS REGARDING|PAINTING RULE|BOILER MACT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REPORT TO CONGRESS|WATERS OF THE US|REQUEST INFORMATION|LEAD IN AMMUNITION|HEARING INVITE|FUEL STANDARD|CLEAN AIR ACT|AGENCY'S|REGARDING FUNDING|QUESTIONS REGARDING|PAINTING RULE|BOILER MACT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT CONCERNED|NOMINATION OF CONSTITUENT|CONSTITUENT CONCERN|MR.|NOMINATION OF", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT CONCERNED|NOMINATION OF CONSTITUENT|CONSTITUENT CONCERN|MR.|NOMINATION OF", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ENERGY STAR PROGRAM", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ENERGY STAR PROGRAM", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ENERGY STAR PROGRAM", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU FOR|AWARDS ANNOUNCEMENT", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU FOR|AWARDS ANNOUNCEMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF|GRANT APPLICATION FOR|UNIVERSITY|TOWNSHIP OF|GRANT PROGRAM|GRANT APPLICATION SUBMITTED BY ", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF|GRANT APPLICATION FOR|UNIVERSITY|TOWNSHIP OF|GRANT PROGRAM|GRANT APPLICATION SUBMITTED BY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SUPERFUND", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SUPERFUND", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SUPERFUND", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EXTEND THE DETAIL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EXTEND THE DETAIL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("EXTEND THE DETAIL", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT))%>%
  mutate(TYPE = ifelse(!str_detect(TYPE,"[0-9]")& str_detect(SUBJECT, "COUNTY"), 3, TYPE))%>%
  mutate(CERTAINTY = ifelse(!str_detect(CERTAINTY,"[0-9]")& str_detect(SUBJECT, "COUNTY"),1, CERTAINTY))%>%
  mutate(POLICY_EVENT = ifelse(!str_detect(POLICY_EVENT, "[:alnum:]")& str_detect(SUBJECT, "COUNTY"),"grant", POLICY_EVENT))
  
     return(data)  
  
}
