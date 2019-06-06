 # This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Duplicate members in some rows needs to be addressed (a few are comma separated)
# Many spelling errors need to be addressed

# source("setup.R")
# file.name <- "DOE_FERC Extended" # for testing

#clean <- function(file.name) {
  
  load("data/DOE_FERC-letters-coded.Rdata")

  data <- FERC_letters
  sum(!is.na(data$TYPE))
 
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
  
  #FIXME
  #Correcting major member name errors 
 data %<>% 
    #Bob Graham 
    mutate(FROM = ifelse (grepl("Bob Graham", FROM, ignore.case = TRUE), "Daniel Graham", FROM)) %>% 
    #Strom Thurmond
    mutate(FROM = ifelse (grepl("Strom Thurmond", FROM, ignore.case = TRUE), "James Thurmond", FROM)) %>% 
    #Michael A Arcuri
    mutate(FROM = ifelse (grepl("Michael A. Arcuri", FROM, ignore.case = TRUE), "Michael Arcuri", FROM)) %>% 
    #WJ "Billy" Tauzin
    mutate(FROM = ifelse (grepl("W.J. \"Billy\" Tauzin|WJ Billy Tauzin|W.J \"Billy\" Tauzin|Chairman W.J \"Billy\ Tauzin", FROM, ignore.case = TRUE), "Wilbert Tauzin", FROM))


  
# SPLIT DATA IN TWO TO EXTRACT MEMBER NAMES
  ## extract member names from the letter texts (members is only for 110th - 118th)
  ## (NOTE: with purrr, extractMemberName shuold not break, the problem is that pasteing to long a string breaks, but applying earlier names to other agencies will make things slow and get false matches. What we should do is trim down member list to congresses in the data being matched before running this function)
  #FIXME
  d1 <- data %>% filter(congress>109) %>% extractMemberName(members = members, col_name = "FROM")
  d2 <- data %>% filter(congress<110) %>% extractMemberName(members = members_106to109th, col_name = "FROM")
  
  sum(!is.na(d2$last_name))
  
  sum(!is.na(d2$last_name)&d2$year==2001)
  
  data <- full_join(d1, d2)
  
  sum(!is.na(data$last_name))
  
  sum(!is.na(data$last_name)&data$year==2001)
  
  # arrange columns for hand coding
  data %<>% select(ID, FROM, SUBJECT, text_clean, TYPE, ALT_TYPE, CERTAINTY, everything())

  sum(!is.na(data$TYPE))

  
  
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
  mutate(Constituent = ifelse(Constituent %in% c("N/A","Na","NO","No","NA", "none", NA), "no", Constituent)) %>% 
  mutate(Constituent = ifelse(Constituent %in% c("YES","Yes"), "yes", Constituent))

#cleaning up ProSide, cleaning up Antiside 
data %<>% 
  mutate(ProSide = ifelse(ProSide %in% c("N/A","Na","NA", "none"), NA, ProSide)) %>% 
  mutate(AntiSide = ifelse(AntiSide %in% c("N/A","Na","NA", "none"), NA, AntiSide))

#cleaning up docket
data %<>% 
  mutate(docket = ifelse(docket %in% c("NONE-000", "none", "NONE"), NA , docket))

#cleaning Place_district
data %<>% 
  mutate(Place_District = ifelse(Place_District %in% c("N/A","Na","NA", "none"), NA, Place_District)) %>% 
  mutate(Place_District = ifelse(Place_District %in% c("YES","Yes"), "yes", Place_District)) %>% 
  mutate(Place_District = ifelse(Place_District %in% c("NO", "No"), "no", Place_District))

#fixing the miscoding of AntiBusiness
miscoded <- c("20170201-0009")

data %<>% 
  mutate(ProBusiness = ifelse(ID %in% miscoded, AntiBusiness, ProBusiness)) %>% 
  mutate(AntiBusiness = ifelse(ID %in% miscoded, NA, AntiBusiness)) 

#fixing miscoded Business

data %<>% 
  #20170324-0041
  mutate(AntiBusiness = ifelse (grepl("20170324-0041", ID, ignore.case = TRUE), "PennEast Pipeline Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20170324-0041", ID, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20170324-0041", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20180619-0014
  mutate(AntiBusiness = ifelse (grepl("20180619-0014", ID, ignore.case = TRUE), "Kinder Morgan, Inc.", AntiBusiness)) %>% 
  mutate(ProBusiness = ifelse (grepl("20180619-0014", ID, ignore.case = TRUE), "Tennessee Gas Pipeline Company, LLC.", ProBusiness)) %>% 
  #20121010-0026
  mutate(AntiBusiness = ifelse (grepl("20121010-0026", ID, ignore.case = TRUE), "Millenium Pipeline Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20121010-0026", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20121010-0026", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20180619-0014
  mutate(AntiBusiness = ifelse (grepl("20121010-0026", ID, ignore.case = TRUE), "Millenium Pipeline Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20121010-0026", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20121010-0026", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20151001-0033
  mutate(AntiBusiness = ifelse (grepl("20151001-0033", ID, ignore.case = TRUE), "Tennessee Gas Pipeline Company, LLC.", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20151001-0033", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20151001-0033", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20110803-0012, 20110803-0013
  mutate(AntiBusiness = ifelse (grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "Sabine Pipe Line, LLC.", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20110803-0012, 20110803-0013
  mutate(AntiBusiness = ifelse (grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "Sabine Pipe Line, LLC.", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20110803-0012|20110803-0013", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20110209-0010
  mutate(ProBusiness = ifelse (grepl("20110209-0010", ID, ignore.case = TRUE), "Northern Natural Gas Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20110209-0010", ID, ignore.case = TRUE), "4", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20110209-0010", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20090928-0078
  mutate(ProBusiness = ifelse (grepl("20090928-0078", ID, ignore.case = TRUE), "Cheyenne Plains Natural Gas Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20090928-0078", ID, ignore.case = TRUE), "4", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20090928-0078", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20090508-0078 WORK ON
  mutate(ProBusiness = ifelse (grepl("20090508-0078", ID, ignore.case = TRUE), "Cheyenne Plains Natural Gas Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20090508-0078", ID, ignore.case = TRUE), "4", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20090508-0078", ID, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #20080107-0097
  mutate(AntiBusiness = ifelse (grepl("20080107-0097", ID, ignore.case = TRUE), "Fort Dodge Hydro Development Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20080107-0097", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20080107-0097", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20070918-0205
  mutate(AntiBusiness = ifelse (grepl("20070918-0205", ID, ignore.case = TRUE), "Northern Natural Gas Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20070918-0205", ID, ignore.case = TRUE), "4", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20070918-0205", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20070604-0167
  mutate(ProBusiness = ifelse (grepl("20070604-0167", ID, ignore.case = TRUE), "Public Service Company of Colorado", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20070604-0167", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20070604-0167", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20070308-0006
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20070308-0006", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20070308-0006", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20050127-0025
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20050127-0025", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20050127-0025", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20030707-0223
  mutate(AntiBusiness = ifelse (grepl("20030707-0223", ID, ignore.case = TRUE), "Pacific Gas and Electric Company", AntiBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20030707-0223", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20030707-0223", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20030516-0025
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20030516-0025", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20030516-0025", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20021127-0045
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20021127-0045", ID, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20021127-0045", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20011011-0250
  mutate(ProBusiness = ifelse (grepl("20011011-0250", ID, ignore.case = TRUE), "East Tennessee Natural Gas Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20011011-0250", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20011011-0250", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20001228-0374
  mutate(ProBusiness = ifelse (grepl("20001228-0374", ID, ignore.case = TRUE), "Algonquin Gas Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20001228-0374", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20001228-0374", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20000828-0126
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20000828-0126", ID, ignore.case = TRUE), "1", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20000828-0126", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #20000828-0071
  mutate(ProBusiness = ifelse (grepl("20001228-0374", ID, ignore.case = TRUE), "Independence Pipeline Company", ProBusiness)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20000828-0071", ID, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20000828-0071", ID, ignore.case = TRUE), "1", CERTAINTY))



#############################################################
# CODING TYPE 
##############################################################

#determining Constituent "yes" is TYPE 1 that ProBusiness and ProProject that aren't n/a are TYPE 2 
data %<>%  
  mutate(TYPE = ifelse(tolower(Constituent) == "yes", 1, TYPE)) %>% 
  mutate(TYPE = ifelse(!is.na(ProBusiness)|!is.na(ProProject), 2, TYPE))

data %>% count(TYPE)

##Categorizing Type in FERC data based on string patterns
  ##first row is rule, second row is certainty level, third row (if uncertain) is an alternative type or can hold policy event information
###################################################################################################################################################
 

  # to restore data
#####################################
#helpful so don't have to run from the beginning 
#uncoded <- data 
#data <- uncoded

#run to show the problems
  #filter(data, ID%in%problemIDs)
#filter(data, ID%in%problemIDs)



##for variable SUBJECT
######################
 
###TYPE 5
data %<>%
  #string "Rulemaking"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("Rulemaking|rulemaking", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>%
  #extend a public comment period 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("extend.*comment", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT))
  

##TYPE 1
data %<>%
  #string "on behalf" constituent, string "on behalf" constituent TYPE 1
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl(str_c("on behalf of an individual","behalf of indivudal", "on behalf of constituent", "behalf of a number of",
                                                            "on behalf of a constituent", "on behalf of his constituent", "on behalf of her constituent",
                                                            "on behalf of a resident", "on behalf of resident", "behalf of citizen", "behalf of concerned citizens",
                                                            "on behalf of his constitutent", "on behalf of her constitutent", "on behalf of constitutent",
                                                            "on behalf of, the residents of",
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl(str_c("on behalf of an individual", "behalf of indivudal", "on behalf of constituent", 
                                                            "on behalf of his constiuent", "on behalf of her constituent", "behalf of a number of",
                                                            "on behalf of a resident", "on behalf of resident", "behalf of citizen", "behalf of concerned citizens",
                                                            "on behalf of his constitutent", "on behalf of her constitutent", "on behalf of constitutent", 
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
 
###Type 2 
data %<>%
  #string "behalf of Company" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of .*Company|on behalf of .*Co.$|on behalf of .*Corp|on behalf of .*Company|on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of .*Company|on behalf of .*Co|on behalf of .*Corp|on behalf of .*Company|on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
          mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("on behalf of .*Company|on behalf of .*Co|on behalf of .*Corp|on behalf of .*Company|on behalf of .*Inc", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE))
  
  
###Type 3
data %<>%
  #string "on behalf" government and nonprofit 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl(str_c("behalf of City", "behalf of Town", "behalf of efforts", 
                                                            "behalf of .*assn", "behalf of .*association", "behalf of .*district",
                                                            "behalf of .*selectmen", "behalf of .*project", "behalf of .*operation", "on behalf .* council",
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "3", TYPE)) %>% 
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl(str_c("behalf of City", "behalf of Town", "behalf of efforts", 
                                                            " behalf of .*assn", "behalf of .*association", "behalf of .*district",
                                                            "behalf of .*selectmen", "behalf of .*project", "behalf of .*operation", "on behalf .* council",
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>% 
  #string "City of"..etc.
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl(str_c("city of.*application", "application .*city of", "on behalf of .*county", 
                                                            "county .*application", "district .* application",
                                                            sep = "|"), SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl(str_c("city of.*application", "application .*city of", "on behalf of .*county", 
                                                                        "county .*application", "district .* application",
                                                                        sep = "|"), SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "Public Utilities Comission" 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))


###TYPE 4
data %<>%
  #string "behalf of Company" 
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl(str_c("on behalf of .*Company", "on behalf of .*Co.$l", "on behalf of .*Corp", 
                                                            "on behalf of .*Inc", 
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl(str_c("on behalf of .*Company", "on behalf of .*Co.$l", "on behalf of .*Corp", 
                                                            "on behalf of .*Inc", 
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
          mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl(str_c("on behalf of .*Company", "on behalf of .*Co.$l", "on behalf of .*Corp", 
                                                            "on behalf of .*Inc", 
                                                             sep = "|"), SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>% 
 #string "New Coal"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
     mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%  
 #string ER18-1314-- Support collaborations  between grid operators and state government 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER18-1314", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
           mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Er18-1314", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE))


##TYPE 5 (lower level)
  data %<>% 
  #submits additional 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%  
      mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
      mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "EL00-95", EVENT_NAME)) %>%
      mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "19-Jun-2001", EVENT_DATE)) %>% 
  #string "EL00-95" #huge variety of letters under this #Enron
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%  
      mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "decision", POLICY_EVENT)) %>% 
      mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "EL00-95", EVENT_NAME)) %>%
      mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("EL00-95", SUBJECT, ignore.case = TRUE), "19-Jun-2001", EVENT_DATE)) %>%
  #string "Rockies Express Pipeline" or "Electric Generator Project"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
      mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
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
  #string ER16-307-- Condition of the New England Electricity Sector 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
               mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
        mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "Federal Power Act, ISO New England", EVENT_NAME)) %>%
            mutate(EVENT_DATE = ifelse(!grepl("[0-9]", EVENT_DATE) & grepl("ER16-307|ICR", SUBJECT, ignore.case = TRUE), "08-Jan-2016", EVENT_DATE)) 

  
###TYPE 1 (lower level) 
data %<>%
  #string "behalf of" uppercase, constitutent names 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of [[:upper:]]", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of [[:upper:]]", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "Constitutent"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("constituent|consituent|constiuent", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("constituent|consituent|constiuent", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  #string "individual"
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("individual|individual's", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))

###TYPE 3 (lower level) 
data %<>%
  #string "application"
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("county .* application|district .* application", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("county .* application|district .* application", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) 


#for variable text_clean
#########################
  
data %<>%
    # #string "on behalf of an individual"
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("on behalf of an individual|behalf of individual|on behalf of constitutent|on behalf of his constitutent|on behalf of her constitutent|on behalf of a constituent", text_clean, ignore.case = TRUE), "1", TYPE)) %>%
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("on behalf of an individual|behalf of individual|on behalf of constituent|on behalf of his constituent|on behalf of her constituent|on behalf of a constituent", text_clean, ignore.case = TRUE), "1", CERTAINTY)) %>%
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


#Fixing Docket Variable
############################

#check if these are miscoded and if not add the docket number 
docket1 <- data %>% 
select(ID, SUBJECT, TYPE, EVENT_NAME, EVENT_DATE, POLICY_EVENT, docket, text_clean, url) %>% 
filter(POLICY_EVENT == "rule", !grepl("rulemaking", SUBJECT, ignore.case = TRUE))

docket2 <- data %>% 
  select(ID, SUBJECT, TYPE, EVENT_NAME, EVENT_DATE, POLICY_EVENT, docket, text_clean, url) %>% 
  filter(POLICY_EVENT == "rule", is.na(docket))




#Event Labeling for Type 5
##############################

data %<>% 
#FIXME
  #DEVIN LOOK AT, know what the issue is but why wouldn't I be able to put subject there? 
      #Distributed Energy Resources
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM18-9", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM18-9", SUBJECT, ignore.case = TRUE), "Distributed Energy Resources", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT) & grepl("RM18-9", SUBJECT, ignore.case = TRUE), "11-Apr-2018", EVENT_DATE)) %>% 
      #Electric Storage Participation in Markets Operated by Regional Transmission Organizations and Independent System Operators
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM16-23", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(!grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM16-23", SUBJECT, ignore.case = TRUE), "Electric Storage Participation in Markets Operated by Regional Transmission Organizations and Independent System Operators", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(!grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM16-23", SUBJECT, ignore.case = TRUE), "17-Nov-2016", EVENT_DATE)) %>% 
      #Grid Reliability and Resilience Pricing
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM18-1", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM18-1", SUBJECT, ignore.case = TRUE), "Grid Reliability and Resilience Pricing", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM18-1", SUBJECT, ignore.case = TRUE), "08-Jan-2018", EVENT_DATE)) %>% 
      #Transmission Planning and Cost Allocation by Transmission Owning and Operating Public Utilities
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM10-23", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM10-23", SUBJECT, ignore.case = TRUE), "Transmission Planning and Cost Allocation by Transmission Owning and Operating Public Utilities", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM10-23", SUBJECT, ignore.case = TRUE), "08-Jan-2018", EVENT_DATE)) %>% 
      #Reliability Standard for Transmission System Planned, Performance for Geomagnetic Disturbance Events
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM15-11", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM15-11", SUBJECT, ignore.case = TRUE), "Reliability Standard for Transmission System Planned, Performance for Geomagnetic Disturbance Events", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM15-11", SUBJECT, ignore.case = TRUE), "19-Oct-2017", EVENT_DATE)) %>% 
      #Revision to Electric Reliability Organization Definition of Bulk Electric System
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM09-18", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM09-18", SUBJECT, ignore.case = TRUE), "Revision to Electric Reliability Organization Definition of Bulk Electric System", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM09-18", SUBJECT, ignore.case = TRUE), "18-Nov-2010", EVENT_DATE)) %>% 
      #Hydroelectric Licensing under the Federal Power Act
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM02-16", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM02-16", SUBJECT, ignore.case = TRUE), "Hydroelectric Licensing under the Federal Power Act", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM02-16", SUBJECT, ignore.case = TRUE), "03-Jul-2003", EVENT_DATE)) %>% 
      #Remedying Undue Discrimination Docket through Open Access Transmission Service and Standard Electricity Market Design, White Paper in the Pacific Northwest
      ##many
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RT01-12", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RT01-12", SUBJECT, ignore.case = TRUE), "Remedying Undue Discrimination Docket through Open Access Transmission Service and Standard Electricity Market Design", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RT01-12", SUBJECT, ignore.case = TRUE), "07-Aug-2003", EVENT_DATE)) %>% 
      #Remedying Undue Discrimination Docket through Open Access Transmission Service and Standard Electricity Market Design
      ##terminate the proceedings in 2002-2003 for the Standard Market Design that was proposed 
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM01-12", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM01-12", SUBJECT, ignore.case = TRUE), "Remedying Undue Discrimination Docket through Open Access Transmission Service and Standard Electricity Market Design", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM01-12", SUBJECT, ignore.case = TRUE), "19-Jul-2005", EVENT_DATE)) %>% 
      #Pipeline Service Obligations and Revisions to Regulations Governing Self-Implementing Transportation Under Part 284 of the Commission's Regulations
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM91-11", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM91-11", SUBJECT, ignore.case = TRUE), "Pipeline Service Obligations and Revisions to Regulations Governing Self-Implementing Transportation Under Part 284 of the Commission's Regulations", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM91-11", SUBJECT, ignore.case = TRUE), "08-Apr-1992", EVENT_DATE)) %>% 
      #Reliability Standards for Geomagnetic Disturbances
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM12-22", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM12-22", SUBJECT, ignore.case = TRUE), "Reliability Standards for Geomagnetic Disturbances", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM12-22", SUBJECT, ignore.case = TRUE), "16-May-2013", EVENT_DATE)) %>% 
      #Promoting Transmission Investment through Pricing Reform
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "Promoting Transmission Investment through Pricing Reform", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "20-Jul-2006", EVENT_DATE)) %>% 
      #Promoting Transmission Investment through Pricing Reform
      mutate(POLICY_EVENT = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "rule", POLICY_EVENT)) %>% 
        mutate(EVENT_NAME = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "Promoting Transmission Investment through Pricing Reform", EVENT_NAME)) %>%
          mutate(EVENT_DATE = ifelse(grepl("rulemaking", SUBJECT, ignore.case = TRUE) & grepl("RM06-4", SUBJECT, ignore.case = TRUE), "20-Jul-2006", EVENT_DATE))


#Place_District
###############

#code to test which have no Place State but have a Place District
    #continue checking this
fixingDistrict <- data %>% 
select(ID, SUBJECT, TYPE, Place_State,ProProject, Place_District, url, ProBusiness, Freelancer) %>% 
filter(is.na(Place_State), str_detect(Place_District, "yes"))
      
#all of the "yes" place district without states that have an attached project
fixingDistrict1 <- data %>% 
select(ID, SUBJECT, TYPE, Place_State,ProProject, Place_District, url, ProBusiness, Freelancer) %>% 
filter(is.na(Place_State), str_detect(Place_District, "yes"), str_detect(ProProject, "."))

#fixing Place_District "4th district", fixing "2nd District", fixing "Ohio", fixing "bad files"
data %<>% 
  mutate(Place_District = ifelse(Place_District %in% c("4th District"), "yes", Place_District)) %>% 
  mutate(Place_District = ifelse(Place_District %in% c("2nd district"), "yes", Place_District)) %>%
  mutate(Place_District = ifelse(Place_District %in% c("Ohio"), "yes", Place_District)) %>% 
  #assuming because constituent and alliance pipeline goes through iowa
  mutate(Place_District = ifelse(Place_District %in% c("bad files"), "yes", Place_District)) %>% 
  #assuming because it is about disaster in state
  mutate(Place_District = ifelse(Place_District %in% c("unsure, not a letter to FERC"), "yes", Place_District))

#adding to fixing Place_District "unsure, not a letter to FERC" 
data %<>% 
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("slamming the federal", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
        mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("slamming the federal", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
            mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("slamming the federal", SUBJECT, ignore.case = TRUE), "disaster", POLICY_EVENT)) %>% 
            mutate(EVENT_NAME = ifelse(!grepl("[0-9]", EVENT_NAME) & grepl("slamming the federal", SUBJECT, ignore.case = TRUE), "Hurricane Katrina", EVENT_NAME))
  

#Testing
######################################################################################################################


###
membersCHECK <- data %>% 
  select(ID, Summary, FROM2, first_name, last_name, SUBJECT, TYPE, ProBusiness, ProProject) %>% 
  filter(is.na(first_name), is.na(last_name))



#Forwarding
###########

#forwarding if ProSide
forward1 <- data %>% 
  select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, url, ProSide) %>% 
  filter(str_detect(SUBJECT, "forwards|fwds|fwd"), str_detect(text_clean, "support"), grepl(".", ProSide))

#forwarding if AntiSide 
forward2 <- data %>% 
  select(ID, SUBJECT, TYPE, AntiBusiness, AntiProject, text_clean, url, AntiSide) %>% 
  filter(str_detect(SUBJECT, "forwards|fwds|fwd"), grepl(".", AntiSide))

#come up with a meter?
#what does forwarding mean, are all forwards just things that allign anyway?
#anticompany letters , just sending this along but wink wink i dont care 
#want data that is forwarded and pro or anti business
#trying to determine if forwarded information from constiuents is supported 
#are they reaching out after?

forward3 <- data %>% 
  select(ID, SUBJECT, TYPE, AntiBusiness, AntiProject, text_clean, url) %>% 
  filter(str_detect(SUBJECT, "forwards|fwds|fwd"), grepl(".", AntiBusiness))


#Type 2 that aren't under probusiness or proproject
###################################################
#fix this
#ID number, or company name
#company name

Type2 <- data %>% 
select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, AntiBusiness,url) %>% 
filter(TYPE == "2", is.na(ProBusiness), is.na(ProProject))


#AntiBusiness but not ProBusiness, notes
#########################################

#checking
checkAntiBusiness <- data %>% 
  select(ID, SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, text_clean, Notes, url) %>% 
  filter(Constituent == "no", is.na(ProBusiness), is.na(ProProject), grepl(".", AntiBusiness)) 

#adding notes on specific cases 
data %<>% 
  mutate(Notes = ifelse (grepl("20130124-0015", ID, ignore.case = TRUE), "Environmental Impact Study, commmunity has questions on public health concerns", Notes)) %>% 
  mutate(Notes = ifelse (grepl("20090629-0029", ID, ignore.case = TRUE), "coexisting projects (hydroelectic and quarry)", Notes)) %>% 
  mutate(Notes = ifelse (grepl("20060628-0019", ID, ignore.case = TRUE), "Enron, obligation to compensation to people of the Pacific Northwest", Notes)) %>% 
  mutate(Notes = ifelse (grepl("20140914-0007", ID, ignore.case = TRUE), "Environmental Impact Study, extension to comment period", Notes))

#fixing specific found error, 20170324-0041
data %<>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("20170324-0041", ID, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("20170324-0041", ID, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  mutate(AntiBusiness = ifelse (grepl("20170324-0041", ID, ignore.case = TRUE), "PennEast Pipeline Company", AntiBusiness))


  

#NOTES
######################################################################################################################

#run to show the problems
  #filter(data, ID%in%problemIDs)
  
##constant working on 
showme <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, Constituent, AntiBusiness,url) %>% 
  filter( grepl("environmental impact statement", SUBJECT, ignore.case = TRUE)) 

showme2 <- data %>% 
  select(ID, SUBJECT, TYPE, FROM2, first_name, last_name) %>% 
  filter(grepl("Bob Graham", FROM2, ignore.case = TRUE)) 

showme3 <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, Constituent, AntiBusiness,url) %>% 
  filter( grepl("rulemaking", SUBJECT, ignore.case = TRUE)) 


#checking to see if errors in business 
businesscheck <- data %>% 
  select(Notes, ID, SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, url, Freelancer) %>% 
  filter(str_detect(SUBJECT, "corporation|Company|corp.|company"), !str_detect(SUBJECT, "constituent"), is.na(ProBusiness), is.na(ProProject), is.na(AntiBusiness))

#check the variables 
unique(data$Place_District)

#Useful to check through misnamed 
#looking at members
countMembers <- data %>% 
  filter(is.na(last_name)) %>% 
  count(FROM2) %>% 
  arrange(-n)

#PERCENT OF MISSED BUSINESS
businesscheck <- data %>% 
  select(ID, SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, text_clean, Notes, url, Freelancer) %>% 
  filter(str_detect(SUBJECT, "corporation|Company|corp.|company"), !str_detect(SUBJECT, "constituent"), is.na(ProBusiness), is.na(ProProject), is.na(AntiBusiness))


#Type 2 do they have an associated pro business or pro project?
Type2 <- data %>% 
select(ID, SUBJECT, TYPE, ProBusiness, ProProject, text_clean, AntiBusiness,url) %>% 
filter(TYPE == "2", is.na(ProBusiness), is.na(ProProject))
  

#checking "rulemaking" 
rulemaking <- data %>% 
  select(ID, SUBJECT, TYPE, EVENT_NAME, EVENT_DATE, text_clean, url) %>% 
  filter(grepl("rulemaking",SUBJECT, is.na(EVENT_NAME)))

#Checking for situation where antibusiness and not pro business 
#why would they send in letter going against a business if they aren't for a consituent and they aren't supporting a project that is going against?
checkAntiBusiness <- data %>% 
  select(ID, SUBJECT, TYPE, Constituent, ProBusiness, ProProject, AntiBusiness, text_clean, url) %>% 
  filter(Constituent == "no", is.na(ProBusiness), is.na(ProProject), grepl(".", AntiBusiness)) 


######################################################################################################################end of notes 

  return(data)

#} ## END CLEAN FUNCTION


#Useful Search Tools
######################


#Used to check through misnamed members
#looking at members count
countMembers <- data %>% 
  select(ID, FROM2, last_name, ProBusiness) %>% 
  filter(is.na(last_name), grepl(".", ProBusiness, ignore.case = TRUE)) %>% 
  count(FROM2) %>% 
  arrange(-n)

##Used to check through whole dataset to find trends for TYPE
showme <- data %>% 
  select(ID, SUBJECT, TYPE, text_clean,ProBusiness, ProProject, Constituent, AntiBusiness,url) %>% 
  filter( grepl("environmental impact statement", SUBJECT, ignore.case = TRUE)) 

check <- data %>% 
  select(FROM, first_name, last_name) %>% 
  filter(grepl("Muray", FROM))


#duplicates, where they don't join
anti_join(FERC_letters, data) 
  
