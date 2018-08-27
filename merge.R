# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions

source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

 # note for MERGING: 
# all columns in d are class character except DATE, year, and congress (see clean.R)
# in df, TYPE is numeric, Type is a factor, and Type2 is types collapsed into Policy and Constituent Service

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
"DOC_NIST", "not coded", NA, # NO MEMBER NAMES--FOLLOW UP FOIA
"DOC_NOAA", "not coded", NA, 
"DOC_OCPA", "not coded", NA,
"DOC_OS", "not coded", NA, # DOC-OS-2017-000958
"DOC_SBA", "not coded", NA, # no records before 2010
# DOD
"DOD_DeCA", "coded", "Devin", # only some are on drive  # FIX MISSING DATES
"DOD_DFAS", "not coded", NA,
"DOD_DLA_Aviation", "not coded", NA,
"DOD_Navy", "coded", "Delaney", # no records before 2013
# "DOD_OIG", "not coded", NA, # waiting for records back from Joe
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
# IRS 
 "IRS", "not coded", NA, # rolling release
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

# initialize for full merge (default)
i <- 1 
# or choose one agency
# i <- which(data_list$agency == "IRS") 
d1 <- clean.agency(agency = data_list[i, 1],
                     status = data_list[i, 2],
                     coders = data_list[i, 3])
d1 %<>% # and merge with voteview data
  left_join(members) %>% # merge on common variables (may differ)
  select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR) %>% 
  left_join(members) %>% # merge again now that we have selected only certian bits of agency data 
  distinct()

d <- d1
####################






##################################
# Repeat merge while successful: #
##################################

# data_list %<>% filter(!(agency %in% df$agency)) # to add new agencies without updating old ones or restart interrupted merge
i = 1
while(!is.na(data_list[i,1])) {
  
  print(data_list[i,1])
  
    d1 <- clean.agency(
      agency = data_list[i, 1],
      status = data_list[i, 2],
      coders = data_list[i, 3]) 
    d1 %<>% 
      left_join(members) %>% 
      select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR) %>% 
      left_join(members)%>% 
      distinct()
    
    d %<>% full_join(d1)
    
    i <- i+1
}
paste("Missing:" , data_list %>% filter(!(agency %in% d$agency)) )
library(gmailr)
send_message(mime(
  To = "<16083529144.17152044287.8rPd34m6s7@txt.voice.google.com>",
  From = "correspondenceresearch@gmail.com",
  Subject =  paste("merge.R stopped at", data_list$agency[i]),
  body = paste("merge.R stopped at", data_list$agency[i])))

##############################
#########################################################################################










###############
# FIX ERRORS #
##############
# fix date-specific member name and party issues. 
# See bad.party object for party switchers to check 
d$icpsr %<>% as.numeric()
d %<>% fix.member.date.coding # edit MemberNameDateCorrections.R script in members folder
d %<>% filter(!is.na(DATE)) # Remove observation with missings DATE
d %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP - this is now done better in fix.member.data.coding function, hopefully
d %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP  - this is now done better in fix.member.data.coding function, hopefully
##############



#######################
# ERRORS we can't fix #
#######################

# Reoccuring problem names
names <- list(a= c("Eleanor","Norton"),b= c("Sally",'Jewell'),c= c('Gregorio','Sablan'), d= c('Stacey','Plaskett'),
              e= c('Amata','Radewagen'),f= c("Donna",'Christensen|Christianson'),g= c('Pedro','Pierluisi'),h= c('Madeleine','Bordallo'),
              i= c('Eni','Faleomavaega'),j= c('(^| )Tia( |$)','Johnson'), k=c('Nelson','Peacock'),l=c('Brian','De Va(|ll)ance'),
              m=c('Peggy','Sherry'),n=c('Donald', 'Kent'))

for(i in 1:length(names)){
  d %<>%
    mutate(ERROR = ifelse(grepl(names[[i]][1], FROM, ignore.case=T)&grepl(names[[i]][2], FROM, ignore.case=T), "Don't include", ERROR))
}

d %<>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% mutate(n = n()) %>% 
  mutate(ERROR = ifelse(n >1 & (bioname == "ROGERS, Mike Dennis" | bioname == "ROGERS, Mike"), "2 Mike Rogers's", ERROR)) %>%  # 2 different members with name Mike Rogers
  mutate(ERROR = ifelse(n >1 & (bioname == "JOHNSON, Timothy Peter (Tim)" | bioname == "JOHNSON, Timothy V."), "2 Tim Johns", ERROR)) %>% 
  mutate(ERROR =  ifelse(grepl("(^| )Biden( |$)", FROM)& DATE > as.Date('2009-01-19'), "Joe is VP", ERROR)) %>% 
  mutate(ERROR = ifelse((grepl("Eleanor|Holmes", FROM)&grepl("Norton", FROM))|(grepl("Eleanor", FROM)&grepl("Holmes", FROM)), "Non-voting DC Rep", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^White House$", FROM, ignore.case=T), "White House", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^Miscellaneous$", FROM, ignore.case=T), "Miscellaneous", ERROR))


#########################
# ERRORS to investigate #
#########################

# date typos 
bad.dates <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(year > 2018 | year < 2000) %>% 
  select(ID, agency, DATE, FROM, first_name, last_name, chamber, state, congress, SUBJECT, TYPE, NOTES, ERROR)

# select  timeframe
d %<>% filter(year < 2018 & year > 2006)

# names that match more than one member - false positives
bad.names.1 <- d %>% 
  filter(is.na(ERROR)) %>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% 
  mutate(n = n()) %>% filter(n>1) %>% ungroup() %>%
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>% arrange(n) %>% 
  select(ID, agency, DATE, FROM, first_name, last_name, bioname, party_code, chamber, congress, SUBJECT, TYPE, NOTES, ERROR) 

# names that don't match - potentially typos / false negatives
bad.names.2 <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(is.na(bioname) | bioname == "") %>% 
  select(ID, agency, DATE, FROM, first_name, last_name,  chamber, state, congress, SUBJECT, TYPE, NOTES, ERROR)
bad.names.2 %>% group_by(agency) %>% summarise(n = n()) %>% arrange(-n)

# party discrepencies between stewart and voteview data
bad.party <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(bioname != "LIEBERMAN, Joseph I.") %>% # Considered Dem and Independent. Voteview party (dem) will override
  left_join(committees) %>% 
  filter(party != party_code) %>% 
  select(bioname, chamber, DATE, congress, party, party_code, icpsr, NOTES, ERROR) %>% 
  distinct()


####################################################################################

####################################################################################
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#########################################################################
#
#               DATA TRANSFORMATIONS 
#               df = one obs per letter matching ICPSR
#               dcommittees = one obs per letter per committee position
#
#
#
#
#
#
#
#
#
#
#
#
#
#
###############################################
# Create df with transformations for analysis #
###############################################

d %<>% ungroup()
df <- filter(d, !is.na(icpsr), !is.na(year), chamber %in% c("House", "Senate")) # select only voteview-matched observations
committees %<>% select(-party) # drop Stewart committee data party codes 



# TIMESERIES COMPLETENESS 
# identify timeframe and completeness for each agency
df %<>% group_by(agency) %>% mutate(timeframe = paste(sort(unique(year)), collapse = ":")) %>%
  mutate(timeframe = paste(agency, timeframe)) %>%
  mutate(complete = ifelse(grepl("2007", timeframe) &
    grepl("2008", timeframe) & 
      grepl("2009", timeframe) &
      grepl("2010", timeframe) &
      grepl("2011", timeframe) &
      grepl("2012", timeframe) &
      grepl("2013", timeframe) &
      grepl("2014", timeframe) &
      grepl("2015", timeframe) &
      grepl("2016", timeframe)  #& grepl("2017", timeframe)
    , T, F)) %>% ungroup()
# Timeframe:
unique(cbind(df$complete ,df$timeframe))
# Problems: 
data_list %>% filter(!(agency %in% df$agency)) 

####################################################################################
# yearly totals for core APSA2018 model 
df %<>% group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() %>%
  mutate(bioname_year = paste(bioname, year))
df$year %<>% as.numeric()
# members with zero letters in a year
zeros <- data_frame(
  year = as.numeric(rep(unique(df$year), n_distinct(members$bioname))), 
  bioname =  rep(unique(members$bioname), n_distinct(df$year)),
  permemberyear = 0) %>%
  mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) %>% 
  left_join(members) %>% 
  filter(!is.na(icpsr), chamber %in% c("House", "Senate")) %>% 
  mutate(bioname_year = paste(bioname, year)) %>% 
  filter(!bioname_year %in% df$bioname_year)

df %<>% full_join(zeros)

############
# New vars #
############
df$department <- gsub("_.*", "", df$agency) # name dept
df %<>% mutate(id = paste(agency, ID)) # unique ID

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
 

 df$Type %<>% as.factor()
 df$Type <- factor(df$Type, c("Indiv. Constituent", "Corp. Constituent", "501c3 or Local Gov.", 
                                 "Corp. Policy", "Policy", "To be coded"))   
 
 
 df %<>% 
   mutate(Type2 = ifelse(Type %in% c("Policy", "Corp. Policy"), "Policy", NA)) %>%
   mutate(Type2 = ifelse(Type %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", Type2)) 

df$party <- NA 
df$party[df$party_code == 100] <- "(D)"
df$party[df$party_code == 200] <- "(R)"
df$party[df$party_code == 328] <- "(I)"

df %<>%
  mutate(member_party = paste(bioname, party)) %>%  
  mutate(member_state = paste(bioname, party, state_abbrev))  %>% 
  mutate(member_state = gsub(",.*\\("," \\(", member_state))


# transformation vars 
df %<>% 
  mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n()) %>% ungroup() %>% 
  mutate(cal.month = format(DATE, "%m(%b)")) %>% 
  mutate(name_state = as.factor(paste(bioname, party, "-", state_abbrev))) %>% 
  mutate(name_state = factor(name_state, levels=rev(levels(name_state)))) %>% 
  mutate(name_agency = paste(name_state, agency)) %>%
  mutate(name_dept = paste(name_state, department))

#################
# District vars #
#################
df %<>% left_join(read.csv("districts/states.csv") )
df %<>% mutate(state_pop2010_millions = pop2010/1000000)








###############
######################################################





######################################################
# member vars #
###############
# shorten party name
df$party_name <- gsub(" Party", "", df$party_name)

# president's party
df %<>% 
  mutate(presidents_party = ifelse(year > 2000 & year < 2009 & party == "(R)", 1, 0)) %>% 
  mutate(presidents_party = ifelse(year > 2008 & year < 2017 & party == "(D)", 1, presidents_party)) %>% 
  mutate(presidents_party = ifelse(year > 2016 & year < 2021 & party == "(R)", 1, presidents_party)) 

# election cycle 
df %<>% 
  mutate(election_year = ifelse(chamber == "Senate" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 6, yearelected+12, yearelected+18, yearelected+24, yearelected+30), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) %>%
  mutate(election_year = ifelse(chamber == "House" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 2, yearelected+4, yearelected+6, yearelected+8, yearelected+10, yearelected+12, yearelected+14, yearelected+16, yearelected+18, yearelected+20), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) 

# gender for those where we have the data from LEP # WE HAVE BETTER DATA, NEEDS TO BE MERGED IN
df$icpsr %<>% as.numeric()

df %<>% left_join(
  read.csv("members/LEP111to113.csv") %>% select(icpsr, female) %>% distinct() %>% filter(icpsr %in% df$icpsr) %>% mutate(icpsr = as.numeric(icpsr))
)

# TOTALS
df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df %<>% fix.member.date.coding()
df %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP
df %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP








#############################################################################
# create dcommittees #
######################
# merge committee data to one obs per letter per committee
committees %<>% select(-partystatus)
committees %<>% filter(!is.na(icpsr))
committees %<>%  filter(!is.na(congress)) 
dcommittees <- df %>% full_join(committees) %>% filter(!is.na(DATE)) # select committee data matching obs

dcommittees %<>% mutate(committee_dept = paste(committee, department)) 
# Compare DATE and assigned date
dcommittees %<>% group_by(member_committee) %>% 
  mutate(firstassignedchairdate = min(assignedchairdate, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(firstassignedchair = as.numeric(substring(firstassignedchairdate, 1, 4)))  %>%
  mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
  mutate(yearsAsChair = daysAsChair/365) %>%
  mutate(monthsAsChair = daysAsChair/30) %>%
  mutate(chair = ifelse(chair_since_2007 == T, paste(firstassignedchair,  bioname, party), NA) ) # note this overwrites 0/1 chair variable


#####################
###########################################################################










#####################
# df Committee Vars #
#####################
# add committee chair data to df (still one observation per letter, unlike dcommittees)
# run after creating dcommittees because below df vars are across committees, e.g. chair = if chair of ANY committee in that congress


# FIXME
# JUST UNTIL WE FIX THESE IN COMMITTEE DATA via committees.R
# df$chair[df$icpsr==94910] # fixed in committees.R
# missing Critz in the 111th
# 
###########################################

# leadership positions
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = chair) %>% distinct()
  ) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, ranking_minority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = ranking_minority) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_leader) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_leader) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_whip) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_whip) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, speaker) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = speaker) %>% distinct()
) %>% filter(!is.na(bioname))


# FIXME 
# ADD BELOW TO MemberNameDateCorrections.R fix.member.dates function:
#  mutate(party = ifelse(name == "Specter, Arlen" & assigneddate < as.Date("2009-04-28"), 200, party)) %>% # THIS IS INSUFICIENT
#  mutate(icpsr = ifelse(name == "Specter, Arlen" & assigneddate > as.Date("2009-04-28"), 94110, icpsr)) %>%  # NEED TO CORRECT MEMBERSHIP ETC
# need to add Kennedy Joe, Jr and III to MemberNameDateCorrections.R
# /FIXME


# chair variable to text
df %<>% 
  mutate(position = ifelse(chair ==1, "Chair", NA)) %>%
  mutate(position = ifelse(ranking_minority == 1, "Ranking Minority", position)) 

bad.committees.2 <- filter(df, is.na(chair)) %>% group_by(icpsr, bioname, congress) %>% summarise(n = n()) %>% arrange(-n)

# partystatus
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, majority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = majority) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% mutate(partystatus = ifelse(majority == 1, "Majority", "All Others"))

# prestige committees
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige_chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige_chair) %>% distinct()
) %>% filter(!is.na(bioname))

# all committee names, sep = "|"
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, committees) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = committees) %>% distinct()
) %>% filter(!is.na(bioname))

# chairs committee names
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chair_of) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = chair_of) %>% distinct() %>% filter(!is.na(chair_of))
) %>% filter(!is.na(bioname))

# year elected 
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, yearelected) %>% distinct() 
) %>% filter(!is.na(bioname))

# Those who served as Chairs at some point
df %<>% mutate(chair_since_2007 = ifelse(bioname %in% c(unique(df$bioname[which(df$position == "Chair")])), T, F) )
  # mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
  # mutate(yearsAsChair = daysAsChair/365) %>%
  # mutate(monthsAsChair = daysAsChair/30) 

df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df %<>% fix.member.date.coding() #  should have dealt with party switchers (Arlen)


#####################

########################################################################
df %<>% filter(!is.na(agency)) # drop any NAs resulting from other merges before merging oversight data 

# Add agency names by acronym from the FOIA List google sheet
df %<>% left_join(
  gs_title("FOIA List") %>% gs_read() %>% select(Department, agency) %>% filter(!is.na(agency)) %>% distinct() #%>% group_by(agency) %>% tally()
)

df %<>% left_join(
  # From Lewis and Seldin AJPS
  data <- read.csv("committees/ACUS.csv") %>% select(Agency, Reporting.Committees, Number.of.Committees, Committeesconfirmingapps, Employees, Independent.Funding, Rulemaking) %>% filter(!is.na(Number.of.Committees)) %>% rename(Department = Agency)
)

# match to committee list 
df$oversight_committee <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$chair_of[i]) & 
     !is.na(df$Reporting.Committees[i]) & 
     grepl(df$committees[i], df$Reporting.Committees[i], ignore.case = T) ) {
    df$oversight_committee[i] <- 1
  } } 
sum(df$oversight_committee)

# match to committee chair (excludes library and printing)
df$oversight_committee_chair <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$chair_of[i]) & 
     !is.na(df$Reporting.Committees[i]) & 
     grepl(df$chair_of[i], df$Reporting.Committees[i], ignore.case = T) ) {
    df$oversight_committee_chair[i] <- 1
  } } 
sum(df$oversight_committee_chair)
########################################################################################################
# add to dcommittees
# chairs committee names
dcommittees %<>% full_join(
  df %>% dplyr::select(icpsr,congress, agency, oversight_committee) %>% 
    group_by(icpsr, congress, agency) %>% top_n(1, wt = oversight_committee) %>% distinct() %>% filter(!is.na(oversight_committee))
) %>% filter(!is.na(bioname))

dcommittees %<>% full_join(
  df %>% dplyr::select(icpsr,congress, agency, oversight_committee_chair) %>% 
    group_by(icpsr, congress, agency) %>% top_n(1, wt = oversight_committee_chair) %>% distinct() %>% filter(!is.na(oversight_committee_chair))
) %>% filter(!is.na(bioname))

















###########################
# remove temp data / vars #
###########################
df %<>% dplyr::select(-n)
rm(d1, data, conglist, electionlist, chairs, file.name, names, requires, to_install, i, Chamber, oversight.committees)
# rm(bad.committees.2, bad.dates, bad.names.1, bad.names.2, bad.party)

#  # all agencies made it into d?
length(unique(d$agency)) == length(unique(data_list$agency))

# all agencies made it through merge into df?
length(unique(df$agency)) == length(unique(d$agency)) 

# save if all data merged 
if(length(unique(df$agency)) == length(unique(data_list$agency))){
save.image("gh-pages/correspondence.RData")
}

