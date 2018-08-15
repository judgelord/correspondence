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
# i <- which(data_list$agency == "USPS") 

d1 <- clean.agency(agency = data_list[i, 1],
                     status = data_list[i, 2],
                     coders = data_list[i, 3])
d1 %<>% # and merge with voteview data
  left_join(members) %>%
  select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR) %>% 
  left_join(members) %>% 
  distinct()

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



# fix date-specific member name and party issues. 
# See bad.party object for party switchers to check 
d %<>% fix.member.date.coding # edit MemberNameDateCorrections.R script in members folder



#######################
# ERRORS we can't fix #
#######################

# Reoccuring problem names
names <- list(a= c("Eleanor","Norton"),b= c("Sally",'Jewell'),c= c('Gregorio','Sablan'), d= c('Stacey','Plaskett'),
              e= c('Amata','Radewagen'),f= c("Donna",'Christensen|Christianson'),g= c('Pedro','Pierluisi'),h= c('Madeleine','Bordallo'),
              i= c('Eni','Faleomavaega'),j= c('(^| )Tia( |$)','Johnson'))

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

# party discrepencies between stewart and voteview data
bad.party <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(bioname != "LIEBERMAN, Joseph I.") %>% # Considered Dem and Independent. Voteview party (dem) will override
  left_join(committees) %>% 
  filter(party != party_code) %>% 
  select(bioname, chamber, DATE, congress, party, party_code, icpsr, NOTES, ERROR) %>% 
  distinct()


d$department <- gsub("_.*", "", d$agency) # name dept
d %<>% mutate(id = paste(agency, ID)) # unique ID

# identify timeframe and completeness for each agency
d %<>% group_by(agency) %>% mutate(timeframe = paste(sort(unique(year)), collapse = ":")) %>%
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
unique(cbind(d$complete ,d$timeframe))
# Problems: 
data_list %>% filter(!(agency %in% d$agency)) 



####################################################################################














d %<>% ungroup()
df <- filter(d, !is.na(icpsr)) # select only voteview-matched observations
committees %<>% select(-party) # drop Stewart committee data party codes 

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
   mutate(Type2 = ifelse(Type %in% c("policy", "Corp. Policy"), "Policy", NA)) %>%
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



# merge committee data to one obs per letter per committee
dcommittees <- df %>% full_join(committees) %>% filter(!is.na(DATE)) # select committee data matching obs
dcommittees$assigneddate %<>% as.Date()
dcommittees$terminationdate %<>% as.Date()

# totals
dcommittees %<>%   
  group_by(month, bioname) %>% mutate(permonth_permember = n()) %>% ungroup() 


# some committe names are upper and some sentence case 
dcommittees %<>% 
  mutate(committeename = toupper(committeename)) # combine upper and lower case stewart committee names

dcommittees %<>% mutate(committee_dept = paste(committee, department))

# year first assigned to a committee
dcommittees %<>% mutate(member_committee = paste(bioname, committee)) 
dcommittees %<>% group_by(member_committee) %<>% 
  mutate(firstassigneddate = min(assigneddate, na.rm = TRUE)) %>% ungroup()
dcommittees %<>% mutate(firstassigned = as.numeric(substring(firstassigneddate, 1, 4))) %>%
  mutate(committee_member = paste(committee, "-", last_name, firstassigned))

# assigned chair
dcommittees %<>% mutate(assignedchairdate = as.Date(assigneddate))
dcommittees$assignedchairdate[dcommittees$position != "Chair"] <- NA
dcommittees$assignedchairdate[is.na(dcommittees$position)] <- NA

# ID Comittee Chairs
dcommittees %<>% mutate(chair_since_2007 = ifelse(member_committee %in% c(unique(dcommittees$member_committee[which(dcommittees$position == "Chair")])), T, F) )

dcommittees %<>% group_by(member_committee) %>% 
  mutate(firstassignedchairdate = min(assignedchairdate, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(firstassignedchair = as.numeric(substring(firstassignedchairdate, 1, 4)))  %>%
  mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
  mutate(yearsAsChair = daysAsChair/365) %>%
  mutate(monthsAsChair = daysAsChair/30) %>%
  mutate(chair = ifelse(chair_since_2007 == T, paste(firstassignedchair,  bioname, party), NA) ) %>% 
  mutate(committee_chair = ifelse(chair_since_2007 == T, paste(committee, "-", last_name, firstassignedchair), NA))













# add committee chair data to df (still one observation per letter, unlike dcommittees)
# run after creating dcommittees because below df vars are across committees, e.g. chair = if chair of ANY committee
committees %<>% filter(!is.na(icpsr))
committees %<>%  filter(!is.na(congress)) 

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

df %<>% 
  mutate(position = ifelse(chair ==1, "Chair", NA)) %>%
  mutate(position = ifelse(ranking_minority == 1, "Ranking Minority", position)) 

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

  

# Those who served as Chairs at some point
df %<>% mutate(chair_since_2007 = ifelse(bioname %in% c(unique(df$bioname[which(df$position == "Chair")])), T, F) )
  # mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
  # mutate(yearsAsChair = daysAsChair/365) %>%
  # mutate(monthsAsChair = daysAsChair/30) 

df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df %<>% fix.member.date.coding()
df %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP
df %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP






# District vars 
df %<>% left_join(read.csv("districts/states.csv") )
df %<>% mutate(pop2010_millions = pop2010/1000000)







# shorten party name
df$party_name <- gsub(" Party", "", df$party_name)

# president's party
df %<>% 
  mutate(presidents_party = ifelse(year > 2000 & year < 2009 & party == "(R)", 1, 0)) %>% 
  mutate(presidents_party = ifelse(year > 2008 & year < 2017 & party == "(D)", 1, 0)) %>% 
  mutate(presidents_party = ifelse(year > 2016 & year < 2021 & party == "(R)", 1, 0)) 
  

# yearly totals for core APSA model 
df %<>% group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() %>% 
  mutate(bioname_congress = paste(bioname, congress))

# remove temp data / vars
df %<>% dplyr::select(-n)
rm(d1, file.name, names, requires, to_install, i)
save.image("gh-pages/correspondence.RData")
