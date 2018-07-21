# This script combines clean log/letter files and merges in other data sources.

# load functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# Departments and agencies are listed A-Z
# 1 agency = the title of the R script for cleaning these data
# 2 status = c("coded", "recoded", "not coded"), NA if not yet hand-coded
# 3 coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets

data_list <- as.data.frame(matrix(c(
# Agency, c(coded, not coded, recoded), coders,
"Amtrak", "not coded", NA,
"DHHS_ACF", "not coded", NA,
"DHHS_ACL", "not coded", NA,
"DHHS_CDC", "not coded", NA,
"DHHS_HRSA", "not coded", NA,
# DHS
"DHS_HQ", "coded", "Anna", # "Katie", "Megha") # Anna took over Katie's sheet and Megha's work is missing
"DHS_ICE", "not coded", NA,
# DOC
"DOC_EDA", "not coded", NA, 
"DOC_IOS", "coded", "Aaron",
"DOC_MBDA", "not coded", NA, # very few dates can be extracted from the text
"DOC_NIST", "not coded", NA,
"DOC_NOAA", "not coded", NA,
"DOC_OCPA", "not coded", NA,
"DOC_OS", "not coded", NA,
"DOC_SBA", "not coded", NA,
# DOD
"DOD_DeCA", "coded", "Devin",
"DOD_DFAS", "not coded", NA,
"DOD_DLA_Aviation", "not coded", NA,
"DOD_Navy", "coded", "Delaney",
"DOD_OSDJS", "not coded", NA,
"DOD_USACE", "not coded", NA,
# DOE
"DOE_FERC", "not coded", NA,
# DOI 
"DOI_BOEM", "not coded", NA, # "coded", "Aaron",
"DOI_BSEE", "not coded", NA,
"DOI_NPS", "not coded", NA,
"DOI_USGS", "not coded", NA,
# DOJ 
"DOJ_CIV", "not coded", NA,
# DOL 
"DOL_EBSA", "not coded", NA,
"DOL_MSHA", "not coded", NA,
"DOL_OCFO", "coded", "Devin",
"DOL_OFCCP", "not coded", NA,
"DOL_OSHA", "not coded", NA,
"DOL_VETS", "not coded", NA,
# DOT 
"DOT_FAA", "coded", "Sam",
"DOT_FHWA", "not coded", NA,
"DOT_SLSDC", "not coded", NA,
# Education
"ED", "not coded", NA,
# EPA
"EPA", "coded", "Aaron", # c("Adam", "Avery"),
# FCA
# "FCA", "not coded", NA, # not many member names to extract, only 100 obs, but full time period
# FCC
"FCC", "coded", "Devin",
# FDA
"FDA", "not coded", NA,
# FHFA
# "FHFA", "not coded", NA,
# FMC
# "FMC", "not coded", NA,
# GSA
# "GSA", "not coded", NA,
# NASA
# "NASA", "not coded", NA, # needs cleanup, esp of dates 
# NCPC
# "NCPC", "not coded", NA,
# PRC
"PRC", "not coded", NA,
# RRB
"RRB", "not coded", NA, # not much subject content
# SSA
"SSA", "not coded", NA,
# Treasury
"Treasury_Fiscal", "not coded", NA,
"Treasury_OCC", "coded", "Aaron",
# USDA 
"USDA", "not coded", NA,
"USDA_ERS", "not coded", NA,
"USDA_FS", "not coded", NA,
"USDA_NASS", "coded", "Robert", # c("Robert", "Henry"),
"USDA_NRCS", "not coded", NA,
"USDA_RD", "not coded", NA,
"USDA_RMA", "not coded", NA,
# USPS
"USPS", "not coded", NA
), ncol = 3, byrow = T))
names(data_list) <- c("agency", "status", "coders")

# clean one file
i = 1
d <- clean.agency(agency = data_list[i, 1],
                     status = data_list[i, 2],
                     coders = data_list[i, 3])

# merge with voteview data
d %<>% 
  left_join(members) %>%
  select(DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ID) %>% 
  left_join(members)

# repeat merge while successful
# i <- i -1 # to resume merge 
# data_list %<>% filter(!(agency %in% d$agency)) # to add new agencies without updating old ones
while(length(unique(d$agency) == i)) {
  
  print(data_list[i,1])
  
    dt <- clean.agency(
      agency = data_list[i, 1],
      status = data_list[i, 2],
      coders = data_list[i, 3]) %>% 
      left_join(members) %<>% 
      select(DATE, year, congress, FROM, bioname, agency, SUBJECT, TYPE, ID) %>% 
      left_join(members)
    
    d %<>% full_join(dt)
    
    i <- i+1
}



# identify timeframe and completeness for each agency
d %<>% group_by(agency) %>% mutate(timeframe = paste(unique(year), collapse = ":")) %>%
  mutate(complete = ifelse(nchar(timeframe) > (10*4+8), T, F))

unique(cbind(d$agency, d$complete, d$timeframe))



# fix member names and parties
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

d$department <- gsub("_.*", "", d$agency) # name dept

# names that match more than one member - false positives
bad.names1 <- d %>% 
  group_by(agency, ID, DATE, FROM, first_name, last_name) %>% 
  mutate(n =) %>% filter(n>1) %>% 
  select(agency, DATE, FROM, first_name, last_name, bioname, party_code, chamber, congress)

# names that don't match - potentially typos / false negatives
bad.names2 <- d %>% 
  filter(is.na(bioname)) %>% 
  select(agency, DATE, FROM, first_name, last_name,  chamber, state, SUBJECT, TYPE)

# date typos 
bad.dates <- d %>% 
  filter(year > 2018 | year < 2000) %>% 
  select(agency, DATE, FROM, first_name, last_name, chamber, state, SUBJECT, TYPE)

# party discrepencies between stewart and voteview data
bad.partyfoul <- d %>% 
  left_join(committees) %>% 
  filter(party != party_code) %>% 
  select(bioname, chamber, DATE, congress, party, party_code, icpsr) %>% 
  distinct()

# one obs per letter per committee assignment 
dcommittees <- d %>% left_join(committees)
dcommittees$assigneddate %<>% as.Date()
dcommittees$terminationdate %<>% as.Date()

# save.image(paste("correspondence", Sys.Date(), ".RData"))

# upload google sheet of obs failing to match with voteview
for (type in c(2,4)) { 
  mismatch <- "mismatch.csv"
  problem.names2 %>% filter(TYPE == type) %>% write.csv(mismatch) # saving file locally is faster
  drive_rm(paste0("Correspondence/", "mismatch", type)) # remove old recode file
  drive_upload(mismatch, path = paste0("Correspondence/", "mismatch", type), type = "spreadsheet")
  file.remove(mismatch) # remove local file
} 
