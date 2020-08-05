# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive


# Vars from members data to keep and merge in
members %<>% select(congress, pattern, bioname, 
                   first_name, last_name, icpsr, common_name,
                   party_name, party_code, state, state_abbrev, chamber, party_size,
                   seo_name, district_code, id, cqlabel, bioImgURL, 
                   district_code, nominate.dim2, nominate.dim1, nominate.geo_mean_probability) %>% 
  distinct()

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
"DHHS_CDC", "not coded", NA, # rolling release, rich subjects, fair amount auto-coded
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
"DOL_EBSA", "coded", "Rochelle",
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
# VA
"VA_CEM", "coded", "Fatima",
"VA", "coded", "Rochelle" # no data before 2008
)
data_list


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
i <- which(data_list$agency == "EPA")
i

d1 <- clean.agency(
  agency = as.character(data_list[i, 1]),
  status = as.character(data_list[i, 2]),
  coders = as.character(data_list[i, 3]))

d1$chamber


d1 %>% filter(pattern != "404error", is.na(bioname)) %>% count(pattern, congress, sort = T)

suppressMessages(
  d1 %<>% 
    left_join(members) %>% 
    select(LetterID, ID, 
           DATE, year, congress, 
           FROM, pattern, bioname, agency, 
           SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, 
           CONSTITUENT_TYPE, CONSTITUENT_CLASS, 
           NOTES, ERROR) %>% 
    left_join(members)%>% 
    distinct()
)

d1$DATE %<>% as.Date()


d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(congress, NAs) %>% spread(key = NAs, value = n)

d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(agency, NAs) %>% spread(key = NAs, value = n) %>% kable()

# if this yeilds anything, something is wrong (obs are failing to match in the members file)
d1 %>% filter(is.na(chamber), pattern != "404error") %>% count(pattern, congress)

missing_data <- d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% 
  add_count(agency, NAs) %>% 
  filter(NAs == "missing")

# redo extractmembernames
missing_data %<>% select(agency, DATE, FROM,congress, LetterID, ID, ERROR) %>% extractMemberName(members, "FROM")
missing_data %>% count(FROM, congress, sort = TRUE)  %>% top_n(20) %>% kable()

# if this yeilds anything, something is wrong (obs are failing to match in the members file)
missing_data %>% filter(pattern != "404error")


####################
####################
# Save 
file.name <- str_c("data/agencies/", 
                   unique(d1$agency), 
                   ".Rdata")

save(d1, file = file.name)



##################################
# Repeat merge while successful: #
##################################
# FIXME use purrr safely() to capture warnings as a few obs are being dropped due to parse failures


# data_list <- data_list[i:nrow(data_list),]
# data_list %<>% filter(!(agency %in% d$agency)) # to add new agencies without updating old ones or restart interrupted merge

## Resume 
# data_list %<>% filter(row_number() > which(data_list$agency == "EPA")) 
data_list
# subset by date
if(F){
  files <- str_c("data/agencies/", list.files(here("data/agencies"))) %>% 
    set_names(list.files(here("data/agencies"))) %>%
    file.info() %>% 
    as_tibble(rownames = "file") %>% 
    filter(mtime < as.Date("2020-06-28")) %>% # date criteria
    distinct() 
  
  files$file
  
  data_list %<>% filter(agency %in% str_remove_all(files$file, ".*/|.Rdata"))
}

head(data_list)

i <- 1 # FIXME with purr walk + error handeling
while(!is.na(data_list[i,1])) {
  
  base::message(inverse("----", data_list$agency[i], "----"))
  
  d1 <- clean.agency(
    agency = as.character(data_list[i, 1]),
    status = as.character(data_list[i, 2]),
    coders = as.character(data_list[i, 3]))
  
  suppressMessages(
  d1 %<>% 
    left_join(members) %>% 
    select(ID, LetterID, agency, 
           DATE, year, congress, 
           FROM, bioname, 
           SUBJECT, TYPE, ALT_TYPE, CERTAINTY, 
           CONSTITUENT_TYPE, CONSTITUENT_CLASS,
           POLICY_EVENT, EVENT_NAME, EVENT_DATE, 
           NOTES, ERROR) %>% 
    left_join(members)%>% 
    distinct()
  )
  
  d1 %<>% select(LetterID, ID, agency, DATE, year, congress, FROM, pattern, bioname, 
                SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR, 
                CONSTITUENT_TYPE, CONSTITUENT_CLASS,
                first_name, last_name, icpsr, party_name, party_code, state, state_abbrev, chamber, 
                district_code, nominate.dim2, nominate.dim1)
  
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


# function to combine rdata files
combine <- function(file){
  load(file)
  d %<>% full_join(d1) 
  return(d)
}

# initialize 
load(files[1])
d <- d1
dim(d)

# COMBINE FILES 
d <- map_dfr(files, combine)
d %<>% distinct()
dim(d)
## Missing agencies:
data_list %>% filter(!(agency %in% d$agency)) %>% select(agency)
# Check for NAs in LetterID
d %>% filter(is.na(LetterID)) %>% count(agency) %>% arrange(agency) %>% kable()

# check for consistant ID digets
unique(nchar(d$LetterID))
filter(d, nchar(LetterID) != 6) %>% select(agency) %>% distinct()

#FIXME can remove, as this is now above
d %<>% select(LetterID, ID, agency, DATE, year, congress, FROM, pattern, bioname, 
              SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR, 
              CONSTITUENT_TYPE, CONSTITUENT_CLASS,
              first_name, last_name, icpsr, party_name, party_code, state, state_abbrev, chamber, 
              district_code, nominate.dim2, nominate.dim1)
d$year %<>% as.numeric()
d$icpsr %<>% as.numeric()
# ## Text Devin - this broke with google's auth update 
# library(gmailr)
# send_message(mime(
#   To = "<16083529144.17152044287.8rPd34m6s7@txt.voice.google.com>",
#   From = "correspondenceresearch@gmail.com",
#   Subject =  paste("merge.R stopped at", data_list$agency[i]),
#  body = paste("merge.R stopped at", data_list$agency[i])))

##############################
#########################################################################################
# COMPARE TO LAST RUN 
nrow(d)
load("draw.Rdata")
nrow(draw)

change <- full_join(d %>% 
            group_by(agency) %>% 
            filter(!is.na(icpsr)) %>% 
            count(name = "d"),
          draw %>% 
            group_by(agency) %>% 
            filter(!is.na(icpsr)) %>% 
            count(name = "draw") ) %>%
  mutate(change = d-draw) %>% 
  arrange(change) %>% 
  filter(change != 0) 

change %>% kable()


changed <- full_join(draw %>%
                       filter(!is.na(icpsr)) %>% 
                       select(agency, FROM, pattern) %>% 
                       distinct() %>% mutate(in_draw = TRUE),
                     d %>%
                       filter(!is.na(icpsr)) %>% 
                       select(agency, FROM) %>% 
                       distinct() %>% mutate(in_d = TRUE) )

missing <- changed %>% filter(is.na(in_d))

# actual problems 
missing %>% filter(agency %in% (data_list %>% 
                              filter(row_number() <= which(data_list$agency == "DOI_SOL")) %>% 
                              .$agency ) ) %>% 
  count(agency, str_sub(FROM, 1, 40), sort = T) %>% 
  filter(agency %in% (data_list %>% 
                        filter(row_number() <= which(data_list$agency == "DOI_SOL")) %>% 
                        .$agency ) )%>%
  head(200) %>%
  kable()

# names that could cause big problems 
draw %>% filter(!is.na(icpsr), FROM %in% missing$FROM) %>% 
  count(agency, str_sub(FROM, 1, 40), sort = T) %>% 
  filter(agency %in% (data_list %>% 
           filter(row_number() <= which(data_list$agency == "DOI_SOL")) %>% 
           .$agency ) )%>%
  head(200) %>%
  kable()

# broken
missing %>% 
  add_count(agency, sort = T, name = "per_agency") %>% count(per_agency, agency, FROM, sort = T) %>% 
  write_csv("changed_names.csv")
  # top_n(100) %>% kable()

# fixed 
changed %>% filter(agency == "DHHS_CMS", is.na(d )) 
#FIXME We should drop all unecessary vars and add them back in later to make post-merge processing go faster


names(d)

# if things look good, save new raw file
# archive raw version of merged data 
draw <- d
nrow(draw)

save(draw, file = "draw.Rdata")
# load("draw.Rdata")
d <- draw

###############
# FIX ERRORS #

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
  group_by(agency, ID, DATE, FROM, SUBJECT, icpsr) %>% mutate(n = n()) %>% 
  mutate(ERROR = ifelse(n >1 & (bioname == "ROGERS, Mike Dennis" | bioname == "ROGERS, Mike"), "FOIA 2 Mike Rogers's", ERROR)) %>%  # 2 different members with name Mike Rogers
  mutate(ERROR = ifelse(n >1 & (bioname == "JOHNSON, Timothy Peter (Tim)" | bioname == "JOHNSON, Timothy V."), "FOIA 2 Tim Johns", ERROR)) %>% 
  # these are commented out because they risk matching real observations--can be more precice by looking at bad names 2
  #mutate(ERROR =  ifelse(grepl("(^| )Biden(,| |$)", FROM)& DATE > as.Date('2009-01-19'), "Joe is VP", ERROR)) %>% 
  #mutate(ERROR = ifelse((grepl("Eleanor|Holmes", FROM)&grepl("Norton", FROM))|(grepl("Eleanor", FROM)&grepl("Holmes", FROM)), "Non-voting DC Rep", ERROR)) %>% 
  # These are specific enough, that they are fine errors
  mutate(ERROR = ifelse(grepl("^White House$", FROM, ignore.case=T), "White House", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^Miscellaneous$", FROM, ignore.case=T), "Miscellaneous", ERROR)) %>% 
  ungroup()



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
nrow(bad.dates)

##############
# fix date-specific member name and party issues. 
# See bad.party object for party switchers to check


d %<>% filter(!is.na(DATE)) # Remove observation with missings DATE
nrow(d)

############################################################################
# Fix chamber and party switchers that were double-matched in members file
# FIXME
# Jeffords switched parties fix in MemberNameDateCorrections.R
nrow(d)
d %<>% fix.member.date.coding() # edit MemberNameDateCorrections.R script in members folder
nrow(d) # should go down by a bit

# inspect
d %>% filter(nchar(as.character(DATE))  < 9 | year > 2020) %>% distinct(DATE, agency) %>% kable()

# TIME RANGE 
nrow(d)
d %<>% 
  # drop obs out of timeframe 
  #FIXME when we get complete data through 2020
  filter(year < 2019 & year > 1999) # %>% 
  # drop bad dates (dates where the member did not serve)
  # filter(DATE != "Date out of range")

nrow(d) # SHOULD GO DOWN 

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
  ungroup() %>% drop_na(FROM) %>% filter(FROM != "NA", FROM != "") %>% 
  mutate(FROM = str_squish(FROM)) %>% select(FROM, agency, congress) %>% 
  group_by(FROM) %>% 
  add_count() # new n

worst.names.sheet <- gs_title("worst.names") %>% 
  gs_read()  %>%
  select(-n) %>%  # drop old n, but keep old problems
  mutate(congress = str_split(congress, ";")) %>%
  unnest(congress) %>%
  mutate(congress = as.numeric(congress)) %>%
  mutate(agency = str_split(agency, ";")) %>% 
  unnest(agency) 

worst.names %<>% full_join(worst.names.sheet)



worst.names %>% 
  group_by(FROM) %>% 
  summarise_all(combine_strings) %>% 
  distinct() %>%
  mutate(n = as.numeric(n)) %>% 
  arrange(-n)   %>% 
  filter(n>5) # 5 mismatches 

# push to google drive
if(update){
sheet_write(worst.names, gs_title("worst.names"), sheet = Sys.Date())
}

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


d %<>% ungroup()
nrow(d)
# Drop observations that failed to match in Voteview
d %<>% filter(!is.na(icpsr)) 
nrow(d) # SHOULD GO DOWN 

d %<>% filter(!is.na(year))
nrow(d) # SHOULD NOT GO DOWN 

d %<>% filter(chamber %in% c("House", "Senate"))
nrow(d) # SHOULD NOT GO DOWN 

#FIXME - some duplicates were created when different FROM columns were created, I think just in CDC, dropping them here, but should be fixed in the CDC script
nrow(d)
d %<>% select(-FROM) %>% distinct()
nrow(d) # SHOULD PROBABLY NOT GO DOWN

d %>% filter(is.na(LetterID)) %>% count(agency, sort = T)
d %>% filter(is.na(ID)) %>% count(agency, sort = T)

# inspect one party switcher 
d %>% filter(bioname == "SPECTER, Arlen", congress == 111) %>% 
  add_count(DATE, SUBJECT, agency, bioname, icpsr) %>% 
  filter(n>1) %>% 
  arrange(DATE) %>% distinct() %>% 
  select(agency, bioname, DATE, SUBJECT, TYPE, n)

nrow(d)
# clean up problems with party switchers etc. that may have come in with merge 
d %<>% fix.member.date.coding() #  should have dealt with party switchers (Arlen)
nrow(d) # n should go down

# inspect one party switcher 
d %>% filter(bioname == "SPECTER, Arlen", congress == 111) %>% 
  add_count(DATE, SUBJECT, agency, bioname, icpsr) %>% 
  filter(n>1) %>% 
  arrange(DATE) %>% distinct() %>% 
  select(agency, bioname, DATE, SUBJECT, TYPE, n)


duplicates <- d %>% 
  group_by(DATE, agency, bioname, SUBJECT) %>% # with the same icpsr and date
  add_count() %>% 
  filter(n>1) %>% 
  summarise_all(combine_strings) 

duplicates %<>% distinct() %>% ungroup()

duplicates %>% count(agency, sort = T) 
duplicates %>% count(agency, SUBJECT,sort = T) 

d %>% 
  filter(agency == "VA") %>% 
  count(agency, DATE, SUBJECT, bioname, sort = T) 


head(duplicates)
max(duplicates$n)
nrow(duplicates)
duplicates$n %<>% as.numeric()
sum(duplicates$n)

# inspect potential problems
duplicate_coding <- duplicates %>%  
  filter(str_detect(TYPE, ";;;")|str_detect(ALT_TYPE, ";;;") ) #|str_detect(CERTAINTY, ";;;")) #|str_detect(POLICY_EVENT, ";;;")|str_detect(EVENT_NAME, ";;;")|str_detect(NOTES, ";;;"))
duplicate_coding %<>% 
  select(DATE, agency, bioname, SUBJECT, TYPE, ALT_TYPE) %>% distinct() %>% arrange(agency)
duplicate_coding

if(update){
# write_csv(duplicate_coding %>% filter(agency != "DOE_FERC"), path = "duplicate_coding.csv")
sheet_write(duplicate_coding, gs_title("duplicate_coding"), as.character(Sys.Date()))
}

duplicate_chambers <- duplicates %>%  
  filter(str_detect(chamber, ";;;") )
duplicate_chambers %>% select(congress, bioname, party_code, icpsr, chamber) %>% distinct()

duplicate_party <- duplicates %>%  
  filter(str_detect(party_code, ";;;") )
duplicate_party %>% select(congress, bioname, party_name, icpsr) %>% distinct()

duplicate_icpsr <- duplicates %>%  
  filter(str_detect(icpsr, ";;;") )
duplicate_icpsr  %>% select(congress, bioname, party_name, icpsr)  %>% distinct()



if(update){
write_csv(duplicates, path = "data/likely_duplicates.csv")
}

duplicates %>% 
  arrange(DATE) %>% 
  select(bioname, DATE, agency, SUBJECT, n)
max(duplicates$n)



nrow(d)
##FIXME Collapse unique name, Date, agency, subject?--could over-collapse some agences with no SUBJECT if a member wrote more than one letter on a date...
# Drops uncoded observations? 
d %>% group_by(DATE, agency, SUBJECT, icpsr, chamber) %>% top_n(1, TYPE) %>% 
  summarise_all(combine_strings)


d %>% group_by(LetterID, DATE, agency, SUBJECT, icpsr, chamber) %>% top_n(1, TYPE) %>% 
  summarise_all(combine_strings)
nrow(d) # MIGHT GO DOWN

d %<>% select(-n)


#FIXME THERE SHOULD NOT BE MORE THAN ONE pattern PER DATE! 
filter(d, str_detect(pattern, ";")) %>% .$pattern
# look <- filter(d, str_detect(bioname, ";"))

## If we wanted to drop all potential dupicates: 
# nrow(d)
# d %<>% anti_join(look %>% select(bioname, DATE, agency, SUBJECT, icpsr, chamber) %>% distinct())
# nrow(d)
# 











#FIXME constituent type and class codes 
#d$CONSTITUENT_TYPE <- NA
#d$CONSTITUENT_CLASS <- NA
source("functions/constituent_types.R")

# inspect 
constituent_coding <- d %>% 
  ungroup() %>% 
  filter(!is.na(CONSTITUENT_TYPE)|!is.na(CONSTITUENT_CLASS)) %>% 
  select(agency, SUBJECT, TYPE, 
         CONSTITUENT_TYPE, CONSTITUENT_CLASS,NOTES, ERROR) %>% 
  group_by(agency, SUBJECT) %>% add_count() %>%
  summarise_all(combine_strings) %>% arrange(agency)
constituent_coding

if(update){
sheet_write(constituent_coding, gs_title("constituent_coding"), as.character(Sys.Date()))
}









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
#################################################
# augmented data, df, should have the same n as d 
n <- nrow(d)
df <- d
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
df %<>% mutate(bioname_year = paste(bioname, year))
df$year %<>% as.numeric()




############
# New vars #
############
df$department <- gsub("_.*", "", df$agency) # name dept
df %<>% mutate(id = paste(agency, LetterID, icpsr)) # unique ID
df %<>% mutate(ID = paste(agency, LetterID, icpsr, sep = "-")) %>% distinct() # replace old ID, which is not unique

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




# transformation vars 
df %<>% 
  mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n()) %>% ungroup() %>% 
  mutate(cal.month = format(DATE, "%m(%b)")) %>% 
  mutate(name_agency = paste(bioname, agency)) %>%
  mutate(name_dept = paste(bioname, department))

# election year (based  on year, not congress, so cannot be in members data)
#FIXME
df %<>% 
  left_join(members %>% select(yearelected, bioname, congress, icpsr, party)) %>%
  mutate(election_year = ifelse(chamber == "Senate" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 6, yearelected+12, yearelected+18, yearelected+24, yearelected+30), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) %>%
  mutate(election_year = ifelse(chamber == "House" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 2, yearelected+4, yearelected+6, yearelected+8, yearelected+10, yearelected+12, yearelected+14, yearelected+16, yearelected+18, yearelected+20), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) 

sum(is.na(df$election_year))

# clean up problems with party switchers etc. that may have come in with merge 
nrow(df)
df$icpsr %<>% as.numeric()
df %<>% fix.member.date.coding()
#FIXME specter and parker should be delt with in previous line
df %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP
df %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP
nrow(df)


nrow(df) == n



#############################################################################
# create dcommittees # 
######################
# FIXME can be applied to counts not individual letters, but requires rewrite of summary as well
# merge committee data to one obs per letter per committee
committees %<>% select(-partystatus, -party) # drop Stewart committee data party codes 
committees %<>% filter(!is.na(icpsr))
committees %<>%  filter(!is.na(congress)) 
committees$congress %<>% as.numeric()
dcommittees <- df %>% full_join(committees) %>% filter(!is.na(DATE)) # select committee data matching obs

dcommittees %>% select(vars) %>% filter(is.na(chair))

dcommittees %>% filter(chair ==1, is.na(assignedchairdate)) 

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













# add letter counts per name and icpsr (party switchers have a new icpsr after they switch)
df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() %>%
  group_by(icpsr, year) %>% mutate(per_year_icpsr = n()) %>% ungroup() 

#####################

########################################################################
df %<>% filter(!is.na(agency)) # drop any NAs resulting from other merges before merging oversight data 

# df %<>% select(-department, -Department)

# Add agency names by acronym from the FOIA List google sheet
foiaList <-  read_csv("data/_FOIA_list.csv") %>% 
  mutate(agency = str_remove(agency, "_$"))
foiaList %>% filter(agency == "DHHS_FDA")

df %<>% left_join(foiaList) %>% distinct()

df %<>% mutate(department = str_remove(agency, "_.*"))

# corrections
df %<>% mutate(Department = ifelse(department == "DHS", "Department of Homeland Security", Department))
df %<>% mutate(Department = ifelse(department == "DOC", "Department of Commerce", Department))
df %<>% mutate(Department = ifelse(department == "DOD", "Department of Defense", Department))
df %<>% mutate(Department = ifelse(department == "DOT", "Department of Transportation", Department))
df %<>% mutate(Department = ifelse(department == "DOI", "Department of the Interior", Department))
df %<>% mutate(Department = ifelse(department == "DHHS", "Department of Health and Human Services", Department))
df %<>% mutate(Department = ifelse(department == "EOP", "Executive Office of the President", Department))
df %<>% mutate(Department = ifelse(department == "USDA", "Department of Agriculture", Department))
df %<>% mutate(Department = ifelse(department == "HUD", "Department of Housing and Urban Development", Department))

df %>% select(agency, department, Department) %>% distinct() %>% filter(is.na(Department))

df %>% select(agency, department, Department) %>% distinct()


df %<>% left_join(
  # From Lewis and Seldin AJPS
  read.csv("committees/ACUS.csv") %>% select(Agency, Reporting.Committees, Number.of.Committees, Committeesconfirmingapps, Employees, Independent.Funding, Rulemaking) %>% filter(!is.na(Number.of.Committees)) %>% rename(Department = Agency)
) %>% distinct()

# match to committee list 
df$oversight_committee <- 0

df %<>% left_join(members %>% select(icpsr, congress, committees, chair_of))

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
df %<>% dplyr::select(-n) %>% distinct()

rm(d1, data, conglist, electionlist, chairs, file.name, names, requires, to_install, Chamber, oversight.committees)
nrow(df)

# merge new data with old? 
if(F){
load("data/all_contacts.RData")
df %<>% full_join(all_contacts)
load("data/all_contacts_committees.Rdata")
dcommittees %<>% full_join(all_contacts_committees)
}

dim(df %>% distinct())
df %>% filter(is.na(icpsr))
df %<>% distinct()

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

























