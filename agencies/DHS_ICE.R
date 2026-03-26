                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   # This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# Complete. All matched. 

# file.name <- "DHS_ICE" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  # look <- filter(data, is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create chamber variable from three columns with member names 
  
  # past chamber info into member/commitee col 
  data$'Member/Committee (HOR)' <- paste(str_extract(data$FROM, "^Rep"), data$FROM) |> str_remove("NA ") 
  
  data$'Member/Committee (Senate)' <- paste(str_extract(data$FROM, "^Sen"), data$FROM) |> str_remove("NA ") 
  
  # define chamber var 
  data$chamber <- ifelse(is.na(data$'Member/Committee (HOR)') & !is.na(data$'Member/Committee (Senate)'),
                         "Senate", 
                         NA )
  data$chamber <- ifelse(is.na(data$'Member/Committee (Senate)') & !is.na(data$'Member/Committee (HOR)'),
                         "House", 
                         data$chamber )
  

  # make everything except DATE and congress character types 
  data %<>% mutate_at(names(data)[which(!names(data) %in% c("DATE", "congress"))], as.character)
  
  data %<>% mutate(FROM = coalesce(FROM, `Member/Committee (Senate)`, `Member/Committee (HOR)`))
  
  # # NAMES 
  # # create two different datasets for different name formats
  # data1 <- filter(data, is.na(FROM))
  # data2 <- filter(data, !is.na(FROM))
  # 
  # data1 %<>% 
  #   mutate(FROM = ifelse(is.na(`Member/Committee (HOR)`) & is.na(FROM),  
  #                        `Member/Committee (Senate)`, 
  #                        `Member/Committee (HOR)`) ) 
  
  # data %<>% 
  #   mutate(FROM = ifelse(is.na(FROM))),
  #   na
  
  # create variable for first and last name
  #data1 %<>% getFirstLast.Comma("FROM")
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }  
  
  #data <- full_join(data2, data1)
  
  

  


  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM,  chamber, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  data %<>%
  mutate(SUBJECT = paste(SUBJECT,Category)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Special Immigrant VISA|ENTRY ISSUE|BENEFITS ISSUE|UNSPECIFIED|(b)(6)|CASE OF|MARRIAGE|NATURALIZATION ISSUE|GREEN CARD|VISA ISSUE|ALIEN SEEKING|GENERAL QUESTION|CONSTITUENT COMPLAINT|);|DENIED|REIMBURSEMENT|TIP|CASEWORK",SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ENTRY ISSUE|BENEFITS ISSUE|UNSPECIFIED|(b)(6)|CASE OF|MARRIAGE|NATURALIZATION ISSUE|GREEN CARD|VISA ISSUE|ALIEN SEEKING|GENERAL QUESTION|CONSTITUENT COMPLAINT|);|DENIED|REIMBURSEMENT|TIP|CASEWORK",SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DETENTION FACILITIES|COLLEGE",SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DETENTION FACILITIES|COLLEGE",SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SECURE COMMUNITIES",SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SECURE COMMUNITIES",SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SECURE COMMUNITIES",SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(NOTES = ifelse (!grepl("[A-Z]", NOTES) & grepl("SECURE COMMUNITIES",SUBJECT, ignore.case = TRUE), "SECURE COMMUNITIES IS A PARTNERSHIP B/W LOCAL GOV'TS/LAW ENFORCEMENT AND THE ICE", NOTES)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STATS|PD PANEL|STATISTICS|HATIAN|CLOSURE|CHINESE REMOVALS|REQUESTS INFORMATION",SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STATS|PD PANEL|STATISTICS|HATIAN|CLOSURE|CHINESE REMOVALS|REQUESTS INFORMATION",SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEETING",SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEETING",SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("MEETING",SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("MEETING",SUBJECT, ignore.case = TRUE), "MEETING", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COUNTY",SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COUNTY",SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("COUNTY",SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HIRING",SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HIRING",SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("HIRING",SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) 
  
  data %<>% mutate(CONSTITUENT_TYPE = ifelse(!grepl("[A-z]", CONSTITUENT_TYPE) & str_detect(str_squish(SUBJECT), ".b..6...b..7|Special Immigrant VISA"),
                                             "Immigrant-General",
                                             CONSTITUENT_TYPE)
  )

  return(data)
  
}
