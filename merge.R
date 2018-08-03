# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# note for MERGING: all columns in d are class character except DATE, year, and congress (see clean.R)

########################
# Master list of data: #
########################
# Departments and agencies are listed A-Z
# 1 agency = the title of the R script for cleaning these data
# 2 status = c("coded", "recoded", "not coded"), NA if not yet hand-coded
# 3 coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets

data_list <- as.data.frame(matrix(c(
# Agency, c(coded, not coded, recoded), coders,
"Amtrak", "not coded", NA, # complete but no subjects to code
"DHHS_ACF", "not coded", NA, # complete and rich, needs more coding
"DHHS_ACL", "not coded", NA,
"DHHS_CDC", "not coded", NA, # rolling release, rich subjects, will eventually be complete
"DHHS_HRSA", "not coded", NA,
# DHS
"DHS_HQ", "coded", "Anna", # "Katie", "Megha") # Anna took over Katie's sheet and Megha's work is missing, complete 
"DHS_ICE", "not coded", NA, # not much to code
# DOC
"DOC_EDA", "not coded", NA, # NEEDS TO HAVE MULTI-MEMBER LINES BROKEN OUT 
"DOC_IOS", "coded", "Aaron",
"DOC_MBDA", "not coded", NA, # very few dates can be extracted from the text
"DOC_NIST", "not coded", NA,
"DOC_NOAA", "not coded", NA,
"DOC_OCPA", "not coded", NA,
"DOC_OS", "not coded", NA, # DOC-OS-2017-000958
"DOC_SBA", "not coded", NA, # no records before 2010
# DOD
"DOD_DeCA", "coded", "Devin", # only some are on drive 
"DOD_DFAS", "not coded", NA,
"DOD_DLA_Aviation", "not coded", NA,
"DOD_Navy", "coded", "Delaney", # records post 2013
# "DOD_OIG", "not coded", NA, # waiting for records back from Upwork Joe
"DOD_OSDJS", "not coded", NA, # waiting on remaining records
"DOD_USACE", "not coded", NA, # no records before fall 2013
# "DOD_USMC", "not coded", NA, # waiting on foia DON-USMC-2018-004141
# DOE
"DOE_FERC", "not coded", NA,
# DOI 
"DOI_BOEM", "coded", "Aaron",
"DOI_BSEE", "not coded", NA,
"DOI_NPS", "not coded", NA,
"DOI_USGS", "not coded", NA,
# DOJ 
"DOJ_CIV", "not coded", NA,
# DOL 
"DOL_EBSA", "not coded", NA,
"DOL_MSHA", "not coded", NA, # NEED MULTI-MEMBER LINES SPLIT, COMPLETE - HIGH PRIPRITY
"DOL_OCFO", "coded", "Devin",
"DOL_OFCCP", "not coded", NA,
"DOL_OSHA", "not coded", NA,
"DOL_VETS", "not coded", NA,
"DOL_OWCP", "not coded", NA,
# DOS 
# "DOS", "not coded", NA, # waiting on dept of state foia 
# DOT 
"DOT_FAA", "coded", "Sam",
"DOT_FHWA", "not coded", NA, # complete, but incomplete on drive (only some were excel), upwork joe working on others
 "DOT_FTA", "not coded", NA, 
"DOT_SLSDC", "coded", "Aaron",
# Education
"ED", "not coded", NA,
# EPA
"EPA", "coded", "Aaron", # c("Adam", "Avery"),
# FCA
 "FCA", "not coded", NA, # not many member names to extract, only 100 obs, but full time period
# FCC
"FCC", "coded", "Devin",
# FDA
"FDA", "not coded", NA,  # 2012-2018 now on drive, waiting on 2007-2011, Sarah B. Kotler email 
# FHFA
# "FHFA", "not coded", NA,
# FMC
# "FMC", "not coded", NA,   # no members contacts, just OMB and reports to congress
# GSA
# "GSA", "not coded", NA, # 6k entries 2007-2017, but only some member names in subject, filed for others july 2018
# NASA
 "NASA", "not coded", NA, # needs cleanup, esp of dates 
# NCPC
# "NCPC", "not coded", NA,
"NLRB" , "not coded", NA,
# PRC
"PRC", "not coded", NA, # no responsive records for FY 2007 or FY 2008. Tracking did not start until FY 2009
# RRB
"RRB", "not coded", NA, # not much subject content
# SSA
"SSA", "not coded", NA, # revisit merge and remove NAs?
# STB
# "STB", "not coded", NA, # need to finish merge script; only 2015-2017?
# Treasury
"Treasury_Fiscal", "not coded", NA,
# "Treasury_Mint", "not coded", NA, # rich and complete, but not on drive needs to be assembled
"Treasury_OCC", "coded", "Aaron",
# USDA 
"USDA", "not coded", NA,
"USDA_ERS", "not coded", NA,
"USDA_FS", "not coded", NA,
"USDA_NASS", "coded", "Robert", # c("Robert", "Henry"),
"USDA_NRCS", "not coded", NA,
"USDA_RD", "not coded", NA,
"USDA_RMA", "not coded", NA, # no records before 2010 - 7 year retention 
# USPS
"USPS", "not coded", NA
), ncol = 3, byrow = T))
names(data_list) <- c("agency", "status", "coders")
data_list

##################
# clean one file #
##################

i = 1 # initialize for full merge (default)

# or choose one agency
# i <- which(data_list$agency == "DOT_FTA") 

d1 <- clean.agency(agency = data_list[i, 1],
                     status = data_list[i, 2],
                     coders = data_list[i, 3])
d1 %<>% # and merge with voteview data
  left_join(members) %>%
  select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES) %>% 
  left_join(members)

# if continuing with merge 
d <- d1

##################################
# Repeat merge while successful: #
##################################

# data_list %<>% filter(!(agency %in% d$agency)) # to add new agencies without updating old ones or restart interrupted merge
i = 1
while(length(unique(d$agency) == i)) {
  
  print(data_list[i,1])
  
    d1 <- clean.agency(
      agency = data_list[i, 1],
      status = data_list[i, 2],
      coders = data_list[i, 3]) 
    d1 %<>% 
      left_join(members) %>% 
      select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES) %>% 
      left_join(members)
    
    d %<>% full_join(d1)
    
    i <- i+1
}
library(gmailr)

send_message(mime(
  To = "<16083529144.17152044287.8rPd34m6s7@txt.voice.google.com>",
  From = "correspondenceresearch@gmail.com",
  Subject =  paste("merge.R stopped at", data_list$agency[i]),
  body = paste("merge.R stopped at", data_list$agency[i])))



# fix date-specific member name and party issues. 
# See bad.party object for party switchers to check 
d %<>% 
  mutate(bioname = ifelse(is.na(bioname), "", bioname)) %>% 
  mutate(party_name = ifelse(is.na(party_name), "", party_name)) %>% 
  mutate(chamber = ifelse(is.na(chamber), "", chamber)) %>% 
  filter(bioname != "PAYNE, Donald Milford" | DATE < as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
  filter(bioname != "PAYNE, Donald, Jr." | DATE > as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
  filter(bioname != "SPECTER, Arlen" | party_name != "Democratic Party" | DATE > as.Date("2009-04-28")) %>% # SPECTER, Arlen changed to DEM
  filter(bioname != "SPECTER, Arlen" | party_name != "Republican Party" | DATE < as.Date("2009-04-28")) %>% 
  filter(bioname != "GRIFFITH, Parker" | party_name != "Republican Party" | DATE > as.Date("2009-12-22")) %>% # GRIFFITH, Parker changed to GOP
  filter(bioname != "GRIFFITH, Parker" | party_name != "Democratic Party" | DATE < as.Date("2009-12-22")) %>%
  filter(bioname != "GILLIBRAND, Kirsten" | chamber != "House" | DATE < as.Date("2009-01-26")) %>% # GILLIBRAND APPOINTED TO SENATE FROM HOUSE January 26, 2009
  filter(bioname != "GILLIBRAND, Kirsten" | chamber != "Senate" | DATE > as.Date("2009-01-26")) %>%
  filter(bioname != "MARKEY, Edward John" | chamber != "House" | DATE < as.Date("2013-06-25")) %>% # # Rep Ed Markey elected to Senate in special election June 25, 2013
  filter(bioname != "MARKEY, Edward John" | chamber != "Senate" | DATE > as.Date("2013-06-25")) 
# NEED TO ADD LIEBERMAN

d$department <- gsub("_.*", "", d$agency) # name dept
d %<>% mutate(id = paste(agency, ID))

# names that match more than one member - false positives
bad.names1 <- d %>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% 
  mutate(n = n()) %>% filter(n>1) %>% ungroup() %>%
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>% arrange(n) %>% 
  select(ID, agency, DATE, FROM, first_name, last_name, bioname, party_code, chamber, congress, SUBJECT, TYPE)

# names that don't match - potentially typos / false negatives
bad.names2 <- d %>% 
  filter(is.na(bioname) | bioname == "") %>% 
  select(ID, agency, DATE, FROM, first_name, last_name,  chamber, state, congress, SUBJECT, TYPE)

# date typos 
bad.dates <- d %>% 
  filter(year > 2018 | year < 2000) %>% 
  select(ID, agency, DATE, FROM, first_name, last_name, chamber, state, congress, SUBJECT, TYPE)

d %<>% filter(year < 2018 & year > 2006)

# party discrepencies between stewart and voteview data
bad.party <- d %>% 
  left_join(committees) %>% 
  filter(party != party_code) %>% 
  select(bioname, chamber, DATE, congress, party, party_code, icpsr) %>% 
  distinct()


# identify timeframe and completeness for each agency
d %<>% group_by(agency) %>% mutate(timeframe = paste(sort(unique(year)), collapse = ":")) %>%
  mutate(timeframe = paste(agency, timeframe)) %>%
  mutate(complete = ifelse(
    grepl("2008", timeframe) & 
      grepl("2009", timeframe) &
      grepl("2010", timeframe) &
      grepl("2011", timeframe) &
      grepl("2012", timeframe) &
      grepl("2013", timeframe) &
      grepl("2014", timeframe) &
      grepl("2015", timeframe) &
      grepl("2016", timeframe) # & grepl("2017", timeframe)
    , T, F)) %>% ungroup()

unique(d$timeframe)


d %<>% ungroup()
df <- filter(d, !is.na(icpsr)) # select only voteview-matched observations

# numeric to text 
 df$Type <- NA
 df$Type[is.na(df$TYPE)] <- "To be coded"
 df$Type[df$TYPE == 0] <- "To be coded"
 df$Type[df$TYPE == 1] <- "Indiv. Constituent"
 df$Type[df$TYPE == 2] <- "Corp. Constituent"
 df$Type[df$TYPE == 3] <- "501c3 or Local Gov."
 df$Type[df$TYPE == 4] <- "Corp. Policy"
 df$Type[df$TYPE == 5] <- "Policy"
 df$Type[df$TYPE == 6] <- "To be coded"

df$party <- NA 
df$party[df$party_code == 100] <- "(D)"
df$party[df$party_code == 200] <- "(R)"
df$party[df$party_code == 328] <- "(I)"

committees %<>% select(-party) # drop Canon Nelson Stewart committee data party codes 

# transformation vars 
df %<>% 
  mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n()) %>% ungroup() %>% 
  mutate(cal.month = format(DATE, "%m(%b)")) %>% 
  mutate(name_state = as.factor(paste(bioname, party, "-", state_abbrev))) %>% 
  mutate(name_state = factor(name_state, levels=rev(levels(name_state)))) %>% 
  mutate(name_agency = paste(name_state, agency)) %>%
  mutate(name_dept = paste(name_state, department))


# merge committee data to one obs per letter per committee
dcommittees <- df %>% left_join(committees)
dcommittees$assigneddate %<>% as.Date()
dcommittees$terminationdate %<>% as.Date()

# lump inst positions
dcommittees %<>% 
  mutate(position = ifelse(10 < seniorstatus & seniorstatus < 17, "Chair", NA)) %>% 
  mutate(position = ifelse(20 < seniorstatus & seniorstatus < 24, "Ranking Minority", position))  %>% 
  mutate(position = ifelse(seniorstatus == 0 | seniorstatus > 24, NA, position))

# some committe names are upper and some sentence case 
dcommittees %<>% 
  mutate(committeename = toupper(committeename)) # combine upper and lower case stewart committee names

# short committee name
dcommittees %<>% mutate(committee = gsub(" AND .*|, .*|\\(.*", "", committeename))

# year first assigned to a committee
dcommittees %<>% mutate(member_committee = paste(bioname, committee)) 
dcommittees %<>% group_by(member_committee) %<>% 
  mutate(firstassigneddate = min(assigneddate, na.rm = TRUE)) %>% ungroup()
dcommittees %<>% mutate(firstassigned = as.numeric(substring(firstassigneddate, 1, 4)))
# assigned chair
dcommittees %<>% mutate(assignedchairdate = as.Date(assigneddate))
dcommittees$assignedchairdate[dcommittees$position != "Chair"] <- NA
dcommittees$assignedchairdate[is.na(dcommittees$position)] <- NA
dcommittees %<>% group_by(member_committee) %>% 
  mutate(firstassignedchairdate = min(assignedchairdate, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(firstassignedchair = as.numeric(substring(firstassignedchairdate, 1, 4)))
dcommittees %<>% 
  mutate(chair = paste(firstassignedchair,  bioname, party)) %>%
  mutate(member_party = paste(bioname, party)) 


# Select only Comittee Chairs
chairs <- filter(dcommittees, member_committee %in% c(unique(dcommittees$member_committee[which(dcommittees$position == "Chair")]))) 

chairs %<>% 
  mutate(daysAsChair = subtract(DATE, firstassignedchairdate) ) %>%
  mutate(yearsAsChair = daysAsChair/365) %>%
  mutate(monthsAsChair = daysAsChair/30) %>%
  group_by(month, bioname) %>% mutate(permonth_permember = n()) %>% ungroup() %>%
  mutate(committee_member = paste(committee, "-", last_name, firstassignedchair))

# add committee chair data to df (still on obs per letter, unlike dcommittees)



save.image(paste("correspondence.RData"))






# upload google sheet of obs failing to match with voteview

#  problem.names2 %>% filter(TYPE %in% c(2,4,5)) %>% write.csv("mismatch.csv") # saving file locally is faster
#  drive_rm(paste0("Correspondence/mismatch")) # remove old recode file
#  drive_upload(mismatch, path = paste0("Correspondence/mismatch"), type = "spreadsheet")
#  file.remove("mismatch.csv") # remove local file
 

  