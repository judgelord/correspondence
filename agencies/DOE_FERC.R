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


#Cleaning Up Columns     
#################################
  
#cleaning up ProBusiness
data %<>% 
  mutate(ProBusiness = ifelse(ProBusiness %in% c("N/A","Na","NO", "NA", "none"), NA, ProBusiness))

#cleaning up AntiBusiness
data %<>% 
  mutate(AntiBusiness = ifelse(AntiBusiness %in% c("N/A","Na","NO", "NA", "none"), NA, AntiBusiness))


#cleaning up ProProject
data %<>% 
  mutate(ProProject= ifelse(ProProject %in% c("N/A","Na","NO","Clean Water Act", "NA", "Consumers", "none"), NA, ProProject))

#cleaning up Constituent 
data %<>% 
  mutate(Constituent = ifelse(Constituent %in% c("N/A","Na","NO","No","NA", "none"), "no", Constituent)) %>% 
  mutate(Constituent = ifelse(Constituent %in% c("YES","Yes"), "yes", Constituent))

#in district 
#should be yes no or n/a

#determining Constituent "yes" is TYPE 1 that ProBusiness and ProProject that aren't n/a are TYPE 2 
data %<>%  
  mutate(TYPE = ifelse(tolower(Constituent) == "yes", 1, TYPE)) %>% 
  mutate(TYPE = ifelse(!is.na(ProBusiness)|!is.na(ProProject), 2, TYPE))

  
##Categorizing Type in FERC data based on string patterns
  ##first row is rule, second row is certainty level, third row (if uncertain) is an alternative type or can hold policy event information
###################################################################################################################################################
  
##for variable SUBJECT 

data %<>%
  #string "on behalf" constituent, string "on behalf" constituent 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of an individual|behalf of individual|on behalf of constitutent|on behalf of a constitutent|on behalf of his constitutent|
                                                         on behalf of her constitutent|on behalf of constitutents|on behalf of a resident of|on behalf of residents|
                                                         on behalf of constituent|on behalf of a constituent|on behalf of his|on behalf of her|on behalf of constituents|
                                                         behalf of concerned citizens|behalf of citzens|behalf of a number of", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of an individual|behalf of individual|on behalf of constitutent|on behalf of his constitutent|
                                                        on behalf of her constitutent|on behalf of a resident of|on behalf of residents|
                                                                      behalf of concerned citizens|behalf of citzens of|behalf of a number of", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "behalf of Company" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of .*Company|on behalf of .*Co|on behalf of .*Corp|on behalf of .*Company|
                                                        on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of .*Company|on behalf of .*Co|on behalf of .*Corp|on behalf of .*Company|
                                                        on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
          mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("on behalf of .*Company|on behalf of .*Co|on behalf of .*Corp|on behalf of .*Company|
                                                        on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>% 
  #string "behalf of" uppercase, constitutent names 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of [[:upper:]]", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of [[:upper:]]", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "Constitutent"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUTENT|constitutent's|constituent|constituent's", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUTENT|constitutent's", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "on behalf" government and nonprofit 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("behalf of the|behalf of City|behalf of Town|behalf of efforts|
                                                        behalf of .*assn|behalf of .*association|behalf of .*district|behalf of .*selectmen|
                                                        behalf of .*project|behalf of .*operation|on behalf .* council", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("behalf of the|behalf of City|behalf of Town|behalf of efforts|
                                                                      behalf of .*assn|behalf of .*Association|behalf of .*district|behalf of .*selectmen|
                                                                      behalf of .*project|behalf of .*operation|on behalf .* council", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  #string "EL00-95" #huge variety of letters under this #Enron
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%  
      mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
      mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "EL00-95", EVENT_NAME)) %>%
      mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "19-Jun-2001", EVENT_DATE)) %>%
  #string "Rulemaking"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
  #string "Rockies Express Pipeline" or "Electric Generator Project"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
      mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
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
  #string RTO WEST  -- stage 2 submissions from RTO West creation(?) and filings
   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "RTO WEST", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("RTO West", SUBJECT, ignore.case = TRUE), "18-Sep-2002", EVENT_DATE)) %>% 
  #string PL18-1 -- Certfication of New Interstate Gas Facilities 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("Pl18-1", SUBJECT, ignore.case = TRUE), "Certification of New Interstate Gas Facilities ", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("PL18-1", SUBJECT, ignore.case = TRUE), "19-Apr-2018", EVENT_DATE)) %>% 
  #string ER18-1314-- Support collaborations  between grid operators and state government 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Er18-1314", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>% 
  #string ER16-307-- Condition of the New England Electricity Sector 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
               mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "Federal Power Act, ISO New England", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "08-Jan-2016", EVENT_DATE)) %>% 
  #extend a public comment period 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
  #string "individual"
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))



  
##for variable text_clean
  
data %<>%
    # #string "on behalf of an individual"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of an individual|behalf of individual|on behalf of constitutent|on behalf of his constitutent|
                                                        on behalf of her constitutent|on behalf of a constituent", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of an individual|behalf of individual|on behalf of constituent|
                                                                      on behalf of his constituent|on behalf of her constituent|on behalf of a constituent", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
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
    #string "Constituent"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUTENT|constitutent's", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
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
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("RTO West", text_clean, ignore.case = TRUE), "18-Sep-2002", EVENT_DATE)) %>% 
    #extend a public comment period 
      mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("extend the comment|extension to the comment", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("extend the comment|extension to the comment", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
          mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("extend the comment|extension to the comment", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT))
    


#Event Labeling 


#2003 request for refunds from San Diego Gas & Electric Company; Pacific Gas and Electric Company; Reliant Resources, Inc.
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>% 
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "RTO WEST", EVENT_NAME)) %>%
        mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("case for refunds", SUBJECT, ignore.case = TRUE), "18-Sep-2002", EVENT_DATE)) %>% 


#Corrections to AntiBusiness Hand Coding
######################################################

# #using case when to move Antibusiness to Probusiness based off of ID number 
# data %<>%
#   mutate(ProBusiness = case_when(
#     ID == "20170201-0009" ~ AntiBusiness,
#     AntiBusiness == "none" ~ NA
#   )) %>% 
#   mutate(AntiBusiness = case_when(
#     ID == "20170201-0009" ~ NA
#   )) 

#using a vector of the ID numbers and ifelse to fix miscoded AntiBusiness to ProBusiness
#switching
  #ID- 20170201-0009, review application in a timely manner for Midcontinent Independent System 
miscoded <- c("20170201-0009","20190311-0022","20190311-0022")

data %>% 
  mutate(ProBusiness = ifelse(ID %in% miscoded, AntiBusiness, ProBusiness)) %>% 
  mutate(AntiBusiness = ifelse(ID %in% miscoded, NA, AntiBusiness)) %>% 

  
  
#Old Code Removed
###################

  # #string "Comments of US Senator"..etc. #check this?
  # mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%


#NOTES
######################################################################################################################


##working on 
showme <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, Constituent, AntiBusiness,url) %>% 
  filter(is.na(TYPE))
  
#filtering to find 4 problem 
PROBLEM <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, Constituent, AntiBusiness,url) %>% 
  mutate(onbehalf = str_extract(SUBJECT, "on behalf of.*")) %>%
      drop_na(onbehalf) %>% 
  filter(TYPE == 4)

#extend a public comment period 
extend <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(text_clean, "extend"))

#testing for suggested events 
tempEVENT <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "comments of US Senator"))


##could write some code with "on the behalf of" after the "on the behalf of individual/constituent" andn find TYPE 2,3 
onbehalf <- data %>% 
  select(SUBJECT, TYPE, Constituent, ProBusiness, ProProject) %>% 
  mutate(onbehalf = str_extract(SUBJECT, "on behalf of.*")) %>%
      drop_na(onbehalf) %>% 
      filter(is.na(TYPE))
    
#missing subject and ID text_clean, url

comments <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "new coal"))


################Trying to figure out...want on the behalf of that isn't a 1 and if it talks about ...its a 2 and if it talks about... its  a 3
##if type is not equal to 1,5,4 and the subject is on the behalf of...then it is a 3 if detect __ and a 2 if detect ____
#if then, this or 

#if(TYPE != 1, 5, 4 & str_detect(SUBJECT "on behalf|on the behalf"))
  #then (ifelse((!grepl("[0-9]", TYPE) & grepl("project|Corportation|Corps|Corps'", text_clean, ignore.case = TRUE), "2", TYPE))))
  #mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & TYPE != 1|4|5 & grepl("on behalf|on the behalf&project|Corportation|Corps|Corps'", text_clean, ignore.case = TRUE), "2", "3")) %>% 
  #if TYPE != 1, then (stringdetect "project|Corportation|Corps|Corps'" then 2 else "City of|"



########Figuring out code for if yes for pro business, and no constituent and no antibusinesss


check <- data %>% 
  select(SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness) %>% 
  filter(Constituent == "no", grepl(".", ProBusiness), is.na(AntiBusiness)) 


#TO DO:
#why would they send in letter going against a business if they aren't for a consituent and they aren't supporting a project that is going against?
checkAntiBusiness <- data %>% 
  select(ID, SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, text_clean, url) %>% 
  filter(Constituent == "no", is.na(ProBusiness), is.na(ProProject), grepl(".", AntiBusiness)) 


#check if marked as a 2 and not marked under probusiness
check2 <- data %>% 
  select(SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, text_clean, url) %>% 
  filter(TYPE == 2, is.na(ProBusiness), is.na(ProProject))

#######end of notes 

  return(data)

} ## END CLEAN FUNCTION



