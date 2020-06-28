# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive
Yes # just in case R asks if we want to install dependencies 



########################
# Master list of data: #
########################
# Departments and agencies are listed A-Z
# agency = the title of the R script for cleaning these data
# status = c("coded", "recoded", "not coded"), NA if not yet hand-coded
# coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets


data_list <- tribble(
  ~agency, ~status, ~coders,   
# Agency sheet name, status = c("coded", "not coded", "recoded"), coders = c("coder1", "coder2", ...),
"ABMC", "not coded", NA, 
"Amtrak", "not coded", NA, # complete but no subjects to code
"CNCS", "not coded", NA,
"CSOSA", "coded", "Julia",
"DHHS_ACF", "coded", "Hope", # complete and rich, needs more coding
"DHHS_ACL", "not coded", NA,
"DHHS_CDC", "not coded", NA, # rolling release, rich subjects, will eventually be complete
"DHHS_CMS", "coded", "Rochelle", #153
"DHHS_HRSA", "not coded", NA,
"DHHS_IHS", "coded", "Rochelle", #
"DHHS_NIH", "coded", "Rochelle", #101
# "DHHS_SAMHSA", "not coded", NA, # DATA PASTED IN GOOGLE SHEET WRONG, ISSUE #119
# DHS
"DHS_HQ", "coded", "Anna", # "Katie", "Megha") # Anna took over Katie's sheet and Megha's work is missing, complete 
"DHS_ICE", "not coded", NA, # not much to code
# DOC
"DOC_EDA", "not coded", NA,  
"DOC_IOS", "coded", "Aaron", 
"DOC_MBDA", "not coded", NA, # very few dates can be extracted from the text # Missing most dates
"DOC_NIST", "not coded", NA, # NO MEMBER NAMES--FOLLOW UP FOIA 
"DOC_NOAA", "not coded", NA, 
"DOC_NTIA", "not coded", NA, 
"DOC_OCPA", "not coded", NA,
#"DOC_OC", "not coded", NA, # No dates
"DOC_OS", "not coded", NA, # DOC-OS-2017-000958
"DOC_SBA", "not coded", NA, # no records before 2010
# DOD
"DOD_DeCA", "coded", "Devin", # only some are on drive  # FIX MISSING DATES
"DOD_DFAS", "not coded", NA,
"DOD_DLA_Aviation", "coded", "Fatima",
"DOD_Navy", "coded", "Delaney", # no records before 2013
"DOD_OIG", "coded", "Fatima", # is this everything? only last name info --> 600+ non matches
"DOD_OSDJS", "not coded", NA, # some records are in text files to be merged #45, waiting on remaining records
"DOD_USACE", "coded", "Fatima", # no records before fall 2013
# "DOD_USMC", "not coded", NA, #  DON-USMC-2018-004141 needs to be converted from pdf and added to drive
# DOE
"DOE_FERC", "coded", "Devin",
# DOI #25 we are missing scripts for new DOI agencies e.g. DOI OS, sometimes just called DOI, but we should avoid that 
# "DOI_BIA", "coded", "Rochelle", #184
"DOI_BOEM", "coded", "Aaron",
"DOI_BSEE", "coded", "Hope",
"DOI_NPS", "not coded", NA,
# OSMRE
"DOI_OSMRE","not coded", NA,
"DOI_SOL", "coded", "Hope",
"DOI_USGS", "coded", "Hope",
# DOJ 
"DOJ_CIV", "not coded", NA, # WHY IS THIS NOT CODED?
"DOJ_ENRD", "coded", "Julia",
"DOJ_EOIR", "coded", "Julia", 
# "DOJ_ExecSec", "not coded", NA, # waiting on FOIA fom DOJ_JMD/OLA
# "DOJ_INTERPOL", "not coded", NA, # logs cover 2012-2018 but many lack dates--may be same as we will get form DOJ_ExecSec
# DOL 
"DOL_EBSA", "not coded", NA,
"DOL_MSHA", "coded", "Hope", 
"DOL_OCFO", "coded", "Devin",
"DOL_OFCCP", "coded", "Rochelle",
# "DOL_OALJ", "not coded", NA, # ???
"DOL_OASAM", "coded", "Rochelle", #190
"DOL_OSHA", "coded", "Rochelle",
"DOL_OWCP", "coded", "Rochelle",
"DOL_SOL", "coded", "Rochelle", 
"DOL_VETS", "coded", "Rochelle",
# DOS 
# "DOS", "not coded", NA, # waiting on dept of state foia 
# DOT 
"DOT_FAA", "coded", "Sam",
"DOT_FHWA", "coded", "Rochelle", # complete, multiple data sources merged
"DOT_FRA", "coded", "Rochelle", #
"DOT_FTA", "coded", "Rochelle", 
"DOT_PHMSA", "coded", "Hope",
"DOT_SLSDC", "coded", "Aaron",
# Education
"ED", "not coded", NA,
"EEOC", "coded", "Rochelle", #108
"EOP_CEQ", "not coded", NA,
"EOP_USTR", "coded", "Hope", #c("Hope", "Julia"), 
# EPA
"EPA", "coded", "Aaron", # c("Adam", "Avery"),
# FCA
 "FCA", "not coded", NA, # 30 or so out of 100 bad names, but full time period
# FCC
"FCC", "coded", "Devin",
# FDA
"DHHS_FDA", "coded", "Rochelle",  # 2007-2018 now on drive, debug issue #97
# FHFA
"FHFA", "not coded", NA, #
# FMC
# "FMC", "not coded", NA,   # no members contacts, just OMB and reports to congress 
#FTC
"FTC", "not coded", NA,
# GSA
# "GSA", "not coded", NA, # 6k entries 2007-2017 on drive, but only some member names in subject, filed for others july 2018 
# HUD
"HUD_HQ", "not coded", NA,
# NARA
"NARA", "coded", "Rochelle",
# NASA
"NASA", "coded", "Rochelle", # 200+ bad names, handful of wrong dates
# NCPC
"NCPC", "not coded", NA,
# NCUA
"NCUA", "not coded", NA, 
# NIGC
"DOI_NIGC", "coded", "Fatima",
# NLRB
"NLRB" , "not coded", NA,
# NWTRB
"NWTRB", "not coded", NA,
# PRC
"PRC", "not coded", NA, # no responsive records for FY 2007 or FY 2008. Tracking did not start until FY 2009
# RRB
"RRB", "not coded", NA, # not much subject content
# SSA
"SSA", "coded", "Rochelle", # fair amount of bad names that coding won't help much
# STB
# "STB", "not coded", NA, # need to finish merge script; only 2015-2017?
# Treasury
"Treasury_Fiscal", "coded", "Julia", 
# IRS 
"Treasury_IRS", "not coded", NA, #28
# "Treasury_Mint", "coded", "Rochelle", #59
"Treasury_OCC", "coded", "Aaron",
"TVA", "not coded", NA,
# USDA 
"USDA", "not coded", NA,
# "USDA_ARS", "not coded", NA, # No script, data doesn't have dates
"USDA_ERS", "not coded", NA, 
"USDA_FS", "not coded", NA,
"USDA_NASS", "coded", "Robert", # c("Robert", "Henry"),
"USDA_NIFA", "not coded", NA, 
"USDA_NRCS", "not coded", NA,
"USDA_RD", "not coded", NA,
"USDA_RMA", "not coded", NA, # no records before 2010 - 7 year retention 
# USPS
"USPS", "not coded", NA,
"VA_CEM", "coded", "Fatima",
"VA", "coded", "Rochelle" # no data before 2008
)
data_list


# log in to google drive
# with cached key (unclear why this is not working)
drive_auth(email = "correspondenceresearch@gmail.com",
           path = "drive-key.json")

# with browser (this is tricky on the linux server)
drive_auth(email = "correspondenceresearch@gmail.com")
1 # if it askes which email to use, use correspondenceresearch since you may have more than on sheet with a given name
googlesheets4::gs4_auth(email = "correspondenceresearch@gmail.com")

# if authorized, this should work
drive_get("RRB")


# check that each agency matches exactly one file on google drive
if(F){
map_dfr(
    paste(data_list$agency, data_list$coders) %>% str_remove(" NA"), 
    gs_title) %>% 
  add_count(name) %>%  
  filter(n != 1) %>% 
  select(name, path)
}


######CLEAN ############
# clean one file #
##################

# Test one agency
i <- which(data_list$agency == "ABMC")
i
d1 <- clean.agency(
  agency = as.character(data_list[i, 1]),
  status = as.character(data_list[i, 2]),
  coders = as.character(data_list[i, 3])
  ) %>% distinct()

# check result 
d1 %>% count(congress, is.na(last_name))

# merge with voteview data to initiate d (unfiltered data)
suppressMessages(
d <- d1 %>%
  left_join(members) %>% # merge on common variables (may differ)
  select(LetterID, ID, DATE, year, congress, FROM, pattern, bioname, agency, 
         SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, 
         #CONSTITUENT_TYPE, CONSTITUENT_RACE, 
         NOTES, ERROR) %>% 
  #left_join(members) %>% # merge again now that we have selected only certian bits of agency data 
  left_join(members) %>% # merge on common variables (may differ)
  distinct()
)

d %>% mutate(NAs = is.na(last_name)) %>% count(congress, NAs)

d %>% filter(!is.na(icpsr)) %>% count(year)
####################




##################################
# Repeat merge while successful: #
##################################
# FIXME use purrr safely() to capture warnings as a few obs are being dropped due to parse failures


# data_list <- data_list[i:nrow(data_list),]
# data_list %<>% filter(!(agency %in% d$agency)) # to add new agencies without updating old ones or restart interrupted merge
# data_list %<>% filter(!agency %in% (list.files("data/agencies") %>% str_remove(".Rdata")))
head(data_list$agency)

i <- 1
while(!is.na(data_list[i,1])) {
  
  base::message(inverse("----", data_list$agency[i], "----"))
  
  d1 <- clean.agency(
    agency = as.character(data_list[i, 1]),
    status = as.character(data_list[i, 2]),
    coders = as.character(data_list[i, 3]))
  
  suppressMessages(
  d1 %<>% 
    left_join(members) %>% 
    select(ID, DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR) %>% 
    left_join(members)%>% 
    distinct()
  )
  
  d1$DATE <- as.Date(d1$DATE)
  
    file.name <- str_c("data/agencies/", 
                       unique(d1$agency), 
                       ".Rdata")
  
  save(d1, file = file.name)
  
  i <- i + 1
}

stopped <- data_list$agency[i]

base::message(white(paste("merge stopped at", stopped)))

###################
# load saved data #
###################
files <- str_c("data/agencies/", list.files(here("data/agencies"))) %>% 
  set_names(list.files(here("data/agencies")))

combine <- function(file){
  load(file)
  d %<>% full_join(d1) 
  return(d)
}

dim(d)
d <- map_dfr(files, combine) 
dim(d)
## Missing any agencies? 
str_c("Missing: " , str_c(data_list %>% filter(!(agency %in% d$agency)) %>% select(agency) ), sep = "; ")



# ## Text Devin - this broke with google's auth update 
# library(gmailr)
# send_message(mime(
#   To = "<16083529144.17152044287.8rPd34m6s7@txt.voice.google.com>",
#   From = "correspondenceresearch@gmail.com",
#   Subject =  paste("merge.R stopped at", data_list$agency[i]),
#  body = paste("merge.R stopped at", data_list$agency[i])))

##############################
#########################################################################################
nrow(d)
# archive raw version of merged data 
draw <- d
nrow(draw)

#FIXME We should drop all unecessary vars and add them back in later to make post-merge processing go faster

save(draw, file = "draw.Rdata")
# load("draw.Rdata")
d <- draw

###############
# FIX ERRORS #
##############
# fix date-specific member name and party issues. 
# See bad.party object for party switchers to check 
d$icpsr %<>% as.numeric()

d %<>% filter(!is.na(DATE)) # Remove observation with missings DATE

# constituent type and class codes 
d$CONSTITUENT_TYPE <- NA
d$CONSTITUENT_CLASS <- NA
source("functions/constituent_types.R")

# party switchers etc
# FIXME
# Jeffords switched parties fix in MemberNameDateCorrections.R
d %<>% fix.member.date.coding() # edit MemberNameDateCorrections.R script in members folder

#######################
# ERRORS we can't fix #
#######################

# Reoccuring problem names
# FIXME 
# Rewrite with purrr
names <- list(a= c("Eleanor","Norton"),b= c("Sally",'Jewell'),c= c('Gregorio','Sablan'), d= c('Stacey|Stacy','Plaskett'),
              e= c('Amata','Radewagen'),f= c("Donna",'Christensen|Christianson'),g= c('Pedro','Pierluisi'),h= c('Madeleine','Bordallo'),
              i= c('Eni','Faleomavaega'),j= c('(^| )Tia( |$)','Johnson'), k=c('Nelson','Peacock'),l=c('Brian','De Va(|ll)ance'),
              m=c('Peggy','Sherry'),n=c('Donald', 'Kent'), o=c('Ann','Schneider'), p=c('Katherine', 'Archuleta'), q=c('Tom|Thomas','Vilsack'), 
              r=c('Luis','Fortuno'))

for(i in 1:length(names)){
  d %<>%
    mutate(ERROR = ifelse(grepl(names[[i]][1], FROM, ignore.case=T)&grepl(names[[i]][2], FROM, ignore.case=T), "Don't include", ERROR))
}

d %<>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% mutate(n = n()) %>% 
  mutate(ERROR = ifelse(n >1 & (bioname == "ROGERS, Mike Dennis" | bioname == "ROGERS, Mike"), "FOIA 2 Mike Rogers's", ERROR)) %>%  # 2 different members with name Mike Rogers
  mutate(ERROR = ifelse(n >1 & (bioname == "JOHNSON, Timothy Peter (Tim)" | bioname == "JOHNSON, Timothy V."), "FOIA 2 Tim Johns", ERROR)) %>% 
  mutate(ERROR =  ifelse(grepl("(^| )Biden(,| |$)", FROM)& DATE > as.Date('2009-01-19'), "Joe is VP", ERROR)) %>% 
  mutate(ERROR = ifelse((grepl("Eleanor|Holmes", FROM)&grepl("Norton", FROM))|(grepl("Eleanor", FROM)&grepl("Holmes", FROM)), "Non-voting DC Rep", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^White House$", FROM, ignore.case=T), "White House", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^Miscellaneous$", FROM, ignore.case=T), "Miscellaneous", ERROR))



#########################
# ERRORS to investigate #
#########################

# date typos 
bad.dates <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(!is.na(FROM) & FROM != "") %>% 
  filter(year > 2019 | year < 1999 | pattern == "Date out of range") %>% 
  filter(!(year < 1999 & agency == "DOE_FERC")) %>% # FERC data extend befor 2000
  arrange(DATE) %>% 
  select(LetterID, ID, agency, DATE, FROM, bioname, SUBJECT, TYPE, NOTES, ERROR)


d %<>% 
  # drop obs out of timeframe 
  #FIXME when we get complete data through 2020
  filter(year < 2019 & year > 1999) %>% 
  # drop bad dates (dates where the member did not serve)
  filter(DATE != "Date out of range")

# names that match more than one member - false positives
bad.names.1 <- d %>% 
  filter(is.na(ERROR)) %>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% 
  mutate(n = n()) %>% filter(n>1) %>% ungroup() %>%
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>% arrange(n) %>% 
  select(ID, agency, DATE, FROM, first_name, last_name, bioname, party_code, chamber, congress, SUBJECT, TYPE, NOTES, ERROR) 
 
# names that don't match - potentially typos / false negatives
bad.names.2 <- d %>% 
  ungroup() %>% 
  filter(is.na(ERROR)) %>% 
  filter(is.na(bioname) | bioname == "") %>% 
  select(LetterID, ID, agency, DATE, congress, FROM, chamber, state, TYPE, NOTES)

worst.agencies <- bad.names.2 %>% ungroup() %>% drop_na(FROM) %>% count(agency)  %>%  arrange(-n) %>% top_n(10)

worst.names <- bad.names.2 %>% 
  # filter(agency != "DHS_HQ")  %>% 
  ungroup() %>% drop_na(FROM) %>% filter(FROM != "NA", FROM != "") %>% 
  group_by(FROM) %>%
  summarise(n = n(),
            agency = str_c(unique(agency), collapse = ";"),
            congress = str_c(unique(congress), collapse = ";") ) %>% distinct() %>%
  arrange(-n)  %>% top_n(100)

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
nrow(d)
# FIXME
# This is where observations that failed to match in Voteview get dropped. 
df <- filter(d, !is.na(icpsr), !is.na(year), chamber %in% c("House", "Senate")) # select only voteview-matched observations
nrow(df)
committees %<>% 
  select(-party) # drop Stewart committee data party codes 

# are all agencies here? 
data_complete()

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
#FIXME
df %<>% 
  mutate(election_year = ifelse(chamber == "Senate" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 6, yearelected+12, yearelected+18, yearelected+24, yearelected+30), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) %>%
  mutate(election_year = ifelse(chamber == "House" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 2, yearelected+4, yearelected+6, yearelected+8, yearelected+10, yearelected+12, yearelected+14, yearelected+16, yearelected+18, yearelected+20), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) 



# TOTALS PER YEAR 
df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df$icpsr %<>% as.numeric()
df %<>% fix.member.date.coding()
df %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP
df %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP


# MEMBER DEMOGRAPHICS 

# gender for those where we have the data from LEP # WE HAVE BETTER DATA, NEEDS TO BE MERGED IN 
df %<>% 
  # merge LEP data into df 
  left_join(
    # read in the LEP data 
    read_csv("members/LEP111to113.csv") %>% 
      # just grabbing female variable for now
      select(icpsr, female) %>% 
      # distinct icpsr-gender combinations
      distinct() %>% 
      #make ICPSR numbers numeric to merge with df
      mutate(icpsr = as.numeric(icpsr))
  )





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

# df %<>% select(-department, -Department)

# Add agency names by acronym from the FOIA List google sheet
foiaList <-  read_csv("data/_FOIA_list.csv") %>% 
  mutate(agency = str_remove(agency, "_$"))
foiaList %>% filter(agency == "DHHS_FDA")

df %<>% left_join(foiaList)

df %<>% mutate(department = str_remove(agency, "_.*"))

# corrections
df %<>% mutate(Department = ifelse(department == "DHS", "Department of Homeland Security", Department))
df %<>% mutate(Department = ifelse(department == "DOC", "Department of Commerce", Department))
df %<>% mutate(Department = ifelse(department == "DOD", "Department of Defense", Department))
df %<>% mutate(Department = ifelse(department == "DOT", "Department of Transportation", Department))
df %<>% mutate(Department = ifelse(department == "DOI", "Department of Interrior", Department))
df %<>% mutate(Department = ifelse(department == "DHHS", "Department of Health and Human Services", Department))
df %<>% mutate(Department = ifelse(department == "EOP", "Executive Office of the President", Department))
df %<>% mutate(Department = ifelse(department == "USDA", "Department of Agriculture", Department))
df %<>% mutate(Department = ifelse(department == "HUD", "Department of Housing and Urban Development", Department))

df %>% select(agency, department, Department) %>% distinct() %>% filter(is.na(Department))

df %>% select(agency, department, Department) %>% distinct()


df %<>% left_join(
  # From Lewis and Seldin AJPS
  read.csv("committees/ACUS.csv") %>% select(Agency, Reporting.Committees, Number.of.Committees, Committeesconfirmingapps, Employees, Independent.Funding, Rulemaking) %>% filter(!is.na(Number.of.Committees)) %>% rename(Department = Agency)
)

# match to committee list 
df$oversight_committee <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$committees[i]) & 
     !is.na(df$Reporting.Committees[i]) &  
     # if the agency reports to their committee
     grepl(df$committees[i], df$Reporting.Committees[i], ignore.case = T) ) {
    # then oversight committee = 1, otherwise 0
    df$oversight_committee[i] <- 1
  } } 
sum(df$oversight_committee)

# match to committee chair (excludes library and printing)
df$oversight_committee_chair <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$chair_of[i]) & # if member is a chair
     !is.na(df$Reporting.Committees[i]) & 
     # and the agency reports to the committee they chair 
     grepl(df$chair_of[i], df$Reporting.Committees[i], ignore.case = T) ) {
    df$oversight_committee_chair[i] <- 1
  } } 
sum(df$oversight_committee_chair)
########################################################################################################
# add to dcommittees
# chairs committee names
dcommittees %<>% full_join(
  df %>% 
    dplyr::select(icpsr,congress, agency, oversight_committee) %>% 
    group_by(icpsr, congress, agency) %>% 
    top_n(1, wt = oversight_committee) %>% 
    distinct() %>% 
    filter(!is.na(oversight_committee))
  ) %>% filter(!is.na(bioname) )

dcommittees %<>% full_join(
  df %>% 
    dplyr::select(icpsr,congress, agency, oversight_committee_chair) %>% 
    group_by(icpsr, congress, agency) %>% 
    top_n(1, wt = oversight_committee_chair) %>% 
    distinct() %>% 
    filter(!is.na(oversight_committee_chair))
) %>% filter(!is.na(bioname))

















###########################
# remove temp data / vars #
###########################
df %<>% dplyr::select(-n)
rm(d1, data, conglist, electionlist, chairs, file.name, names, requires, to_install, Chamber, oversight.committees)


# merge new data with old? 
if(F){
load("data/all_contacts.RData")
df %<>% full_join(all_contacts)
load("data/all_contacts_committees.Rdata")
dcommittees %<>% full_join(all_contacts_committees)
}


# save if all data sources merged, save data files
if(length(unique(df$agency)) == length(unique(data_list$agency))){

  all_contacts <- df
  save(all_contacts, file = "data/all_contacts.RData")
  
  all_contacts_committees <- dcommittees
  save(all_contacts_committees, file = "data/all_contacts_committees.Rdata")
  
  write_csv(bad.names.1, "data/bad.names.1.csv")
  save(bad.names.2, file = "data/bad.names.2.csv")
  bad.names.2 %>% 
    drop_na(TYPE, FROM, SUBJECT) %>% #FIXME when this is smaller, we can preview more on github limit 500kb csv preveiw
    select(ID, agency, DATE, FROM, TYPE, SUBJECT, NOTES) %>% 
    arrange(agency) %>% 
    write_csv("data/bad.names.2.csv")
  worst.agencies %>% write_csv("data/worst.agencies.csv")
  worst.names %>% write.csv("data/worst.names.csv")
  bad.dates %>% write_csv("data/bad.dates.csv")
  bad.party %>% write.csv("data/bad.party.csv")
  #FIXME
  # save(bad.committees.1, file = "data/bad.committees.1.RData")
  # save(bad.committees.2, file = "data/bad.committees.2.RData")
  d %>% filter(str_detect(NOTES, "FOIA")) %>%
    select(ID, agency, FROM, DATE, SUBJECT, NOTES) %>% write_csv(path = "data/LETTERS_TO_FOIA.csv")
}

# counts per agency - check if this matches google sheet 
look <- df %>% count(agency, Department) %>% full_join(data_list %>% select(agency))
look %>% filter(is.na(Department))

# Check that FERC data is complete:
df %>% filter(agency == "DOE_FERC") %>% count(year)

# If everything looks good, update data summary table 
# source("agencies/_FOIA_response_table.R")

data_complete()







