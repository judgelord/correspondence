# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 63 mismatches


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
  
  # data %<>%
  #   mutate(DATE = ifelse(is.na(DATE)&))
  # 
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # Duplicates need fixing, commas appear on non-duplicates (go back and fix after manual cleaning)
  # ###############    
  # # Creates duplicate rows for lines with multiple representatives
  # for(i in 1:nrow(data)){
  #   if(grepl(",", data$FROM[i])) {
  #     
  #     new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ",") + 1))
  #     new$FROM <- unlist(str_split(data$FROM[i], ","))
  #     
  #     data <- rbind(data, new)
  #     
  #   }
  # }
  # data <- data[-grep(",", data$FROM),] # removes orginal row with all data
  # data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  # data <- data[!data$FROM == "",] # removes blank observations
  # ################
  
  
  # member name
  data %<>% extractMemberName(members,"FROM")
  
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






