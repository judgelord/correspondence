# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "FHFA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  

  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- data$`Modified O`
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # # create variable for full name
  # data$FROM <- gsub("Tanko", "Tonko", data$FROM)
  # data <- extractMemberName(data, members,"FROM")
  #data %<>% 
    #rename(FROM = Status)

  # FIXME 
  # Is this right? Are there no other places where names appear where FROM is NA?
  unfoundnames <- data %<>%
    filter(is.na(FROM))
  
  data2 %<>% 
    # FIXME
    # sometimes member names ended up in SUBJECT (where FROM is "Closed")
    # maybe it would be better to just coppy subject into FROM in these cases
    filter(FROM == "Closed") %>%
    extractMemberName(members, 'SUBJECT')
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  # FIXME 
  # better done with str_split and unnest. See DOL_SOL and FDA
  
  data %<>%
    str_split(FROM, ";") %>%
    unnest(FROM)
  
  #for(i in 1:nrow(data)){
    #if(grepl(";", data$FROM[i])) {
      
     # new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      #new$FROM <- unlist(str_split(data$FROM[i], ";"))
      
      #data <- rbind(data, new)
      
    #}
  #}
  
  # FIXME 
  # dropping these observations is risky and maybe not necessary if we use unnest(FROM) above
  
  #data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  
  # Remove extra white space...there is a function for this 
  data$FROM %<>% str_remove_all("^ |^  | $|  $", "")
  
  data %<>% filter(!FROM == "") # removes blank observations
  ################
  
  
  # FIXME 
  # We should aim to run the extractMemberNames or getFirstLast.Comma or whatever name detection function we are using only once per column of names. 
  data <- getFirstLast.Comma(data, 'FROM')
  # data2 <- data[data$FROM == "Closed",]
  
  # extract last name from the subject column
  i <- 1
  for (i in 1:length(members$id)) {
    data2 %<>% 
      mutate(last_name = ifelse(is.na(last_name) & grepl(paste("( |^)", members$last_name[i], "( |$|,)", sep = ""), SUBJECT, ignore.case = T), members$last_name[i], last_name))
  }
  # add first name info based on last_name
  data2$first_name <- addFirst(data2$first_name,data2$last_name)
  
  data <- full_join(data2, data)
  data <- data[!(is.na(data$first_name)&is.na(data$last_name)&data$ID<88),]
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, first_name, last_name, everything())
  
  # apply codebook to type 
  data %<>%
  mutate(SUBJECT = paste(SUBJECT,DATE)) %>%
  mutate(SUBJECT = paste(SUBJECT,SYSTEM)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|LOAN MODIFICATION|EVICTION|FOIA REQUEST|GOLDEN PARACHUTE|PRIVATE TRANSFER FEE|REPURCHASE|HARP|REQUEST FOR ASSISTANCE|MULTIFAMILY|TERMITE|TRANSFER FEES|PRIVACY|QUALIFIED|LANGUAGE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|LOAN MODIFICATION|EVICTION|FOIA REQUEST|GOLDEN PARACHUTE|PRIVATE TRANSFER FEE|REPURCHASE|HARP|REQUEST FOR ASSISTANCE|MULTIFAMILY|TERMITE|TRANSFER FEES|PRIVACY|QUALIFIED|LANGUAGE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LIFE INSURANCE COMPANIES|PRIVATE LAW", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LIFE INSURANCE COMPANIES|PRIVATE LAW", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FSOC|FHLB|MERKLEY-LEVIN|STRATEGIC PLAN", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FSOC|FHLB|MERKLEY-LEVIN|STRATEGIC PLAN", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FORECLOSURE|PACE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FORECLOSURE|PACE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU|APOLOGY", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU|APOLOGY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EVERBANK|BANK OF AMERICA", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EVERBANK|BANK OF AMERICA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}






