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

    
    
#cleaning up ProBusiness
data %<>% 
  mutate(ProBusiness = ifelse(ProBusiness %in% c("N/A","Na","NO","Clean Water Act", "NA", "Consumers", "none"), NA, ProBusiness))

#cleaning up ProProject
data %<>% 
  mutate(ProProject= ifelse(ProProject %in% c("N/A","Na","NO","Clean Water Act", "NA", "Consumers", "none"), NA, ProProject))

#determining that ProBusiness and ProProject that aren't n/a are TYPE 2 
data %<>%  
  mutate(TYPE = ifelse(tolower(Constituent) == "yes", 1, TYPE)) %>% 
  mutate(TYPE = ifelse(!is.na(ProBusiness)|!is.na(ProProject), 2, TYPE))


  
##Categorizing Type in FERC data based on string patterns
  ##first row is rule, second row is certainty level, third row (if uncertain) is an alternative type or can hold policy event information
###################################################################################################################################################
  
##for variable SUBJECT 

data %<>%
  #string "on behalf of an individual" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of an individual|behalf of individual|on behalf of constituent|on behalf of his constituent|on behalf of her constituent", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of an individual|behalf of individual", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "EL00-95" #huge variety of letters under this 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%  
      mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
      mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "EL00-95", EVENT_NAME)) %>%
      mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "19-Jun-2001", EVENT_DATE)) %>%
  #string "Rulemaking"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
  #string "individual"
  mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "Constituent"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|constituent's", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|constituent's", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "Rockies Express Pipeline" or "Electric Generator Project"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
      mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  # #string "Comments of US Senator"..etc. #check this?
  # mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "City of"..etc. 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "New Coal"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
     mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  #string "Public Utilities Comission" 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string RTO WEST 
   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "RTO WEST", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "18-Sep-2002", EVENT_DATE)) %>% 
  #string PL18-1 - Certfication of New Interstate Gas Facilities 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("Pl18-1", SUBJECT, ignore.case = TRUE), "Certification of New Interstate Gas Facilities ", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "19-Apr-2018", EVENT_DATE)) %>% 
  #string ER18-1314
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Er18-1314", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE))
  #string ER16-307-- New England Electricity Sector 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
               mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "Federal Power Act, ISO New England", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "08-Jan-2016", EVENT_DATE)) %>% 
  #extend a public comment period 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("extend|comment period", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("extend|comment period", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("extend|comment period", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT))



data %<>%
    #string "on behalf of an individual"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of an individual|behalf of individual|on behalf of constituent|on behalf of his constituent|on behalf of her constituent", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of an individual|behalf of individual", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "EL00-95" #huge variety of letters under this
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EL00-95", text_clean, ignore.case = TRUE), "5", TYPE)) %>%
       mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EL00-95", text_clean, ignore.case = TRUE), "2", CERTAINTY)) %>%
         mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("EL00-95", text_clean, ignore.case = TRUE), "decision", POLICY_EVENT)) %>%
           mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("EL00-95", text_clean, ignore.case = TRUE), "EL00-95", EVENT_NAME)) %>%
           mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("EL00-95", text_clean, ignore.case = TRUE), "19-Jun-2001", EVENT_DATE)) %>%
    #string "Rulemaking"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", text_clean, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
    # #string "individual"
    # mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("individual|individual's", text_clean, ignore.case = TRUE), "1", TYPE)) %>% 
    #     mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual|individual's", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "Constituent"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|constituent's", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|constituent's", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "Rockies Express Pipeline" or "Electric Generator Project"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "5", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "3", CERTAINTY)) %>%
        mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", text_clean, ignore.case = TRUE), "2", ALT_TYPE)) %>%
    # #string "Comments of US Senator"..etc. 
    # mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", text_clean, ignore.case = TRUE), "5", TYPE)) %>%
    #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "City of"..etc. 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", text_clean, ignore.case = TRUE), "3", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
    #string "New Coal"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "4", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "2", CERTAINTY)) %>%
        mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", text_clean, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    #string "Public Utilities Comission" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", text_clean, ignore.case = TRUE), "3", TYPE)) %>%
      mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    #string "RTO WEST"
     mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RTO West", text_clean, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RTO West", text_clean, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RTO West", text_clean, ignore.case = TRUE), "4", ALT_TYPE)) %>% 
            mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("RTO West", text_clean, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("RTO West", text_clean, ignore.case = TRUE), "RTO WEST", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("RTO West", text_clean, ignore.case = TRUE), "18-Sep-2002", EVENT_DATE))
    





##could write some could with "on the behalf of" after the "on the behalf of individual/constituent" andn find TYPE 2,3 


#TO DO
###############
#figure out the 1, personal service, and 5, policy
#figure out any miscoding, old coding that might be overbroad 
#look through letters and find key concepts, events, and their dates 

#specifically...
  #recode "further actions" better
  #

#Notes
###################
#sum(!na) (data$)
  #whereever the data is you're working with
##for variable text_clean
#filter(is.na(TYPE))
#behalf of individual|behalf of individuals|an individual's|on behalf of Mr|on behalf of Dr
#determining rule
##if taking comments they are doing rule 



#looking through smaller full data
showme <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, url) %>% 
  filter(is.na(TYPE))
  

#extend a public comment period 
extend <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(text_clean, "extend"))

#testing for suggested events 
tempEVENT <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "ER16-307"))


tempEVENT <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "ISO-NE"))

temp7 <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(text_clean, "concern"))

tempER <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "ER18-1314"))

temp9 <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "support collaboration"))

temp10 <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "ER16-307"))

 
#write one on EL00-95
#write one on in regards to....wihtout constituent 

temp2 <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "on behalf of")) %>%
  filter(is.na(TYPE))


temp9 <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean, ProBusiness, ProProject, url) %>% 
  filter(str_detect(SUBJECT, "RTO West"))




# temp2 <- data %>% 
#   select(ID, SUBJECT, TYPE, text_clean) %>% 
#   filter(str_detect(SUBJECT, "Proposed|proposed")) %>% 
#   filter(!str_detect(SUBJECT, "Rulemaking|rulemaking"))
#if reading letter and the letter is commenting on their district, note that they were in district 


  return(data)

} ## END CLEAN FUNCTION



