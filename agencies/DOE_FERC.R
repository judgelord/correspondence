 # This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Duplicate members in some rows needs to be addressed (a few are comma separated)
# Many spelling errors need to be addressed

# source("setup.R")
# file.name <- "DOE_FERC Extended" # for testing

clean <- function(file.name) {
  
  load("data/DOE_FERC-letters-coded.Rdata")
  
  data <- FERC_letters
 
  data <- ungroup(FERC_letters)

  # create agency column
  data$agency <- "DOE_FERC"
  
  # Format date, year, Congress, member name etc. 
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # chamber
  data %<>%
    mutate(chamber = ifelse(grepl("(^Sen)",members), 'Senate', NA)) %>% 
    mutate(chamber = ifelse(grepl("(^Rep)",members), 'House', chamber)) %>% 
    mutate(chamber = ifelse(is.na(chamber) & grepl("(Senate|Senator)",SUBJECT), 'Senate', chamber)) %>% 
    mutate(chamber = ifelse(is.na(chamber) & grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", SUBJECT), "House", chamber)) %>% 
    mutate(chamber = ifelse(is.na(chamber) & grepl("Sen",SUBJECT), 'Senate', chamber)) %>% 
    mutate(chamber = ifelse(is.na(chamber) & grepl("Rep", SUBJECT), "House", chamber)) %>%
    mutate(chamber = ifelse(is.na(chamber) & grepl("(Senate|Senator)",text_clean), 'Senate', chamber)) %>% 
    mutate(chamber = ifelse(is.na(chamber) & grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", text_clean), "House", chamber)) 
  
  look <- filter(data, 
                 is.na(chamber)  & !is.na(members) & members != "NA") %>% select(members)
  
  # FROM = members (drops old FROM)
  data %<>% mutate(FROM = str_remove(members, "^Rep. |^Rep.|^Sen. |^Sen.|")) %>% 
    select(-members)
  
# SPLIT DATA IN TWO TO EXTRACT MEMBER NAMES
  ## extract member names from the letter texts (members is only for 110th - 118th)
  ## (NOTE: with purrr, extractMemberName shuold not break, the problem is that pasteing to long a string breaks, but applying earlier names to other agencies will make things slow and get false matches. What we should do is trim down member list to congresses in the data being matched before running this function)
  #FIXME
  #d1 <- data %>% filter(congress>109) %>% extractMemberName(members = members, col_name = "FROM")
  #d2 <- data %>% filter(congress<110) %>% extractMemberName(members = members_106to109th, col_name = "FROM")
  
  sum(!is.na(d2$last_name))
  
  sum(!is.na(d2$last_name)&d2$year==2001)
  
  data <- full_join(d1, d2)
  
  sum(!is.na(data$last_name))
  
  sum(!is.na(data$last_name)&data$year==2001)
  
  # arrange columns for hand coding
  data %<>% select(ID, FROM, SUBJECT, text_clean, everything())

  
  
  
##Categorizing Type in FERC data based on string patterns
  ##first row is rule, second row is certainty level, third row (if uncertain) is an alternative type or can hold policy event information
###################################################################################################################################################
  
##for variable SUBJECT 

data %<>%
  filter(is.na(TYPE)) %>% 
  #string "further actions"
  mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("further actions", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("further actions", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%  
      mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("further actions", SUBJECT, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
      mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("further actions", SUBJECT, ignore.case = TRUE), "ELOO-95", EVENT_NAME)) %>%
      #mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("further actions", SUBJECT, ignore.case = TRUE), "13-Jun-2001", EVENT_DATE)) %>%
      #June 12 and 13
  #string "Rulemaking"
  mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
  #string "Proposed"
  mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("Proposed|proposed", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Proposed|proposed", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "individual"
  mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("individual", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "Constituent"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "Rockies Express Pipeline" or "Electric Generator Project"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
      mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  #string "Comments of US Senator"..etc. 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "City of"..etc. 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "New Coal"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
     mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  #string "Public Utilities Comission" 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  

##for variable text_clean

  data %<>%
    #string "further actions"
     mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("further actions", text_clean, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("further actions", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
          mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("further actions", text_clean, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("further actions", text_clean, ignore.case = TRUE), "ELOO-95", EVENT_NAME)) %>%
            #mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("further actions", text_clean, ignore.case = TRUE), "13-Jun-2001", EVENT_DATE)) %>%
            #June 12 and 13
   # ELOO-95
    #string "Rulemaking"
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
    #string "Proposed"
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("Proposed|proposed", text_clean, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Proposed|proposed", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "individual"
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("individual", text_clean, ignore.case = TRUE), "1", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "Constituent"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "Rockies Express Pipeline" or "Electric Generator Project"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "5", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "3", CERTAINTY)) %>%
        mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "2", ALT_TYPE)) %>%
    #string "Comments of US Senator"..etc. 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", text_clean, ignore.case = TRUE), "5", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "City of"..etc. 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", text_clean, ignore.case = TRUE), "3", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "New Coal"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "4", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "2", CERTAINTY)) %>%
        mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    #string "Public Utilities Comission" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", text_clean, ignore.case = TRUE), "3", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", text_clean, ignore.case = TRUE), "1", CERTAINTY)) 



#TO DO
#find 1 
#find 5
#figure out miscoding of 1 with constituents 
#open example files of letters that type as 1 
  
temp <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean, EVENT_NAME) %>% 
  filter(str_detect(SUBJECT, "further actions"))

 
# #temp <- data %>% 
#   select(ID, SUBJECT, TYPE, text_clean, url) %>% 
#   filter(str_detect(SUBJECT, "individual"))


# #temp2 <- data %>% 
#  select(ID, SUBJECT, TYPE, text_clean) %>% 
#  filter(str_detect(SUBJECT, "Proposed|proposed")) %>% 
#  filter(!str_detect(SUBJECT, "Rulemaking|rulemaking"))
# 


  return(data)

} ## END CLEAN FUNCTION



