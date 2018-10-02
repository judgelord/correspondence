# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# file.name <- "SSA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # Remove rows containing NA in both FROM and SUBJECT column
  data <- data[!(is.na(data$FROM)&is.na(data$SUBJECT)),]
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Some names contained in the DATE column
  # data %<>% extractMemberName(members,"DATE")
  
  
  # Format date, year, Congress, member name etc. 
  data$originalDATE <- data$DATE
  data %<>% select(originalDATE, DATE, everything())
  data$DATE <- gsub(" .*","",data$DATE)
  data$DATE <- gsub("/200","/0",data$DATE)
  data$DATE <- gsub("/201","/1",data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")

  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Duplicates need fixing, commas appear on non-duplicates (go back and fix after manual cleaning)
  ###############
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";", data$FROM[i])) {

      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";"))

      data <- rbind(data, new)

    }
  }
  data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  
  
  
  # state
  data %<>%
    mutate(state = ifelse(grepl(".*\\(.*([A-Z]{2}).*\\).*", data$FROM),
                          gsub(".*\\(.*([A-Z]{2}).*\\).*", '\\1', data$FROM),
                          NA))# %>% 
    # mutate(state= ifelse(grepl("(^| )Rep\\. Cartwright( |$)",data$FROM), "PA", state)) %>% 
    # mutate(state= ifelse(grepl("(^| )Sen\\. Shelby( |$)",data$FROM), "AL", state)) %>% 
    # mutate(state= ifelse(grepl("(^| )Sen\\. Boozman( |$)",data$FROM), "AR", state)) %>% 
    # mutate(state= ifelse(grepl("(^| )Rep. Hudson( |$)",data$FROM), "NC", state))
  
  
  data$state <- stateFromLower(data$state)
  
  
  data$FROM <- gsub("(^| )(Burr|Richard Burr)( |$)", " Richard Burr ",data$FROM)
  
  # create name variable for names with only last name info
  data %<>%
    mutate(name =  ifelse(grepl("^(Rep\\.|Sen\\.|\\w|\\w\\.) (\\w{2,})(| )($|\\(.*)", data$FROM),
                          gsub("^(Rep\\.|Sen\\.|\\w|\\w\\.) (\\w{2,})(| )($|\\(.*)", '\\2', data$FROM),
                          NA)) %>% 
    mutate(name = ifelse(grepl("^(\\w{2,})($| \\(.*)", data$FROM),
                         gsub("^(\\w{2,})($| \\(.*)", '\\1', data$FROM),
                         name)) %>% 
    select(FROM, name, state ,everything())
  
  
  data$name <- formatLastName(data, 'name')

  # member name
  data %<>% extractMemberName(members,"FROM") 
  data %<>%
    mutate(last_name = ifelse(grepl("(^| )Burr( |$)",data$FROM), "BURR", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Burr( |$)",data$FROM), "Richard", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Manchin( |$)",data$FROM), "MANCHIN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Manchin( |$)",data$FROM), "Joe", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Isakson( |$)",data$FROM), "ISAKSON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Isakson( |$)",data$FROM), "Johnny", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Cartwright( |$)",data$FROM), "CARTWRIGHT", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Cartwright( |$)",data$FROM), "Matt", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Shelby( |$)",data$FROM), "SHELBY", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Shelby( |$)",data$FROM), "Richard", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Boozman( |$)",data$FROM), "BOOZMAN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Boozman( |$)",data$FROM), "John", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Collins \\(GA-(9|09)\\)",data$FROM), "COLLINS", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Collins \\(GA-(9|09)\\)",data$FROM), "Doug", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Heck \\(NV-3\\)",data$FROM), "HECK", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Heck \\(NV-3\\)",data$FROM), "Joe", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Hudson($| )", data$FROM), "HUDSON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Hudson($| )",data$FROM), "Richard", first_name)) %>% 
    mutate(last_name = ifelse(grepl("S Brown \\(OH\\)", data$FROM), "BROWN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("S Brown \\(OH\\)",data$FROM), "Sherrod", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Brat \\(VA-7\\)", data$FROM), "BRAT", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Brat \\(VA-7\\)",data$FROM), "David", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )McCaskill", data$FROM), "McCASKILL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )McCaskill",data$FROM), "CLAIRE", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Benishek($| )", data$FROM), "BENISHEK", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Benishek($| )",data$FROM), "Dan", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Cotton($| )", data$FROM), "COTTON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Cotton($| )",data$FROM), "Tom", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Nugent($| )", data$FROM), "NUGENT", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Nugent($| )",data$FROM), "Richard", first_name)) %>% 
    mutate(last_name = ifelse(grepl("(^| )Crawford($| )", data$FROM), "CRAWFORD", last_name)) %>% 
    mutate(first_name = ifelse(grepl("(^| )Rick($| )",data$FROM), "Rick", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Rogers \\(KY-5\\)", data$FROM), "ROGERS", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Rogers \\(KY-5\\)",data$FROM), "Harold", first_name)) %>% 
   
     # only last name info, no first name
    mutate(last_name = ifelse(is.na(last_name)& !is.na(name), name, last_name)) %>% 
    mutate(last_name = ifelse(last_name %in% members$last_name, last_name, NA))
    
  
  
  data$first_name <- addFirst(data$first_name, data$last_name)

  
  
  data %<>%
  mutate(SUBJECT = paste(SUBJECT,ACTION)) %>% 
  mutate(SUBJECT = paste(SUBJECT, CCRS.Specialist)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNDERPAYMENT|INITIAL CLAIM|CITIZENSHIP|OVERPAYMENT|CONSTITUENT|DISABILITY|DENIED|STATUS ON CLAIM|RETIREMENT|WIDOW|BILLING|HIRING PRACTICE|WITHDRAWAL|2ND REQUEST|SUICIDE|INITIAL AWARD|ATTORNEY FEE|ADDITIONAL PAYMENT|GARNISHMENT ISSUE|WOUNDED WARRIORS|DECEASED|THEFT|BIPOLAR|WORK AND EARNINGS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNDERPAYMENT|INITIAL CLAIM|CITIZENSHIP|OVERPAYMENT|CONSTITUENT|DISABILITY|DENIED|STATUS ON CLAIM|RETIREMENT|WIDOW|BILLING|HIRING PRACTICE|WITHDRAWAL|2ND REQUEST|SUICIDE|INITIAL AWARD|ATTORNEY FEE|ADDITIONAL PAYMENT|GARNISHMENT ISSUE|WOUNDED WARRIORS|DECEASED|THEFT|BIPOLAR|WORK AND EARNINGS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FIRST DRAFT|HEARING|RULE|ADCLCA TO CONTROL|REQUEST FOR INFORMATION|ALJ DECISION|RECONSIDERATION DETERMINATION|REQUEST SSA|DISPUTE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FIRST DRAFT|HEARING|RULE|ADCLCA TO CONTROL|REQUEST FOR INFORMATION|ALJ DECISION|RECONSIDERATION DETERMINATION|REQUEST SSA|DISPUTE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("FIRST DRAFT", SUBJECT, ignore.case = TRUE), "LEGISLATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OFFICIAL BUSINESS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OFFICIAL BUSINESS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING", SUBJECT, ignore.case = TRUE), "HEARING", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CLAIM|BENEFITS|COMPLAINT|APPEAL PENDING|STAFFER|PERSONNEL|REINSTATE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CLAIM|BENEFITS|COMPLAINT|APPEAL PENDING|STAFFER|PERSONNEL|REINSTATE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("RULE|REQUEST SSA", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("ADCLCA|ALJ DECISION|RECONSIDERATION DETERMINATION|DISPUTE", SUBJECT, ignore.case = TRUE), "DECISION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DEBT RECOVERY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DEBT RECOVERY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DEBT RECOVERY", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("REQUEST FOR INFORMATION", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LUMBERTON", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LUMBERTON", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
}






