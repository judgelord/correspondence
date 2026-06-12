########################
# Master list of data: #
########################
# Departments and agencies are listed A-Z
# agency = the title of the R script for cleaning these data
# status = c("coded", "recoded", "not coded"), NA if not yet hand-coded
# coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets

data_list <- tribble(
  ~agency, ~status, ~coders,   ~years_to_include,
  # Agency sheet name, status = c("coded", "not coded", "recoded"), coders = c("coder1", "coder2", ...),
  "ABMC", "not coded", NA, list(2016:2018),
  "Amtrak", "not coded", NA, list(2007:2019), # complete but no subjects to code
  "CNCS", "not coded", NA, list(2009:2019),
  "CSOSA", "coded", "Julia", list(2003:2017),
  "DHHS_ACF", "coded", "Hope",  list(2007:2018), # complete and rich, needs more coding
  "DHHS_ACL", "not coded", NA, list(2017:2018),
  "DHHS_CDC", "not coded", NA, list(2007:2024), # rolling release, rich subjects, fair amount auto-coded
  "DHHS_CMS", "coded", "Rochelle", list(2007:2020), #153
  "DHHS_HRSA", "not coded", NA, list(2007:2017),
  "DHHS_IHS", "coded", "Rochelle", list(2007:2017), #
  "DHHS_NIH", "coded", "Rochelle", list(2007:2019), #101
  "DHHS_SAMHSA", "not coded", NA, list(2009:2018), # DATA PASTED IN GOOGLE SHEET WRONG, ISSUE #119
  # DHS
  "DHS_HQ", "coded", "Anna", list(2003:2017), # "Katie", "Megha") # Anna took over Katie's sheet and Megha's work is missing, complete 
  "DHS_ICE", "not coded", NA, list(2014:2016), # not much to code
  "DHS_USCIS", "not coded", NA, list( c(2016,2018) ) , # autocode? # these data are actually just 2018, but I combine them with 2016 below, so we need 2016 here 
  "DHS_USCIS_2016", "not coded", NA, list(2016), # autocode?
  # DOC
  "DOC_EDA", "not coded", NA, list(2011:2016),  
  "DOC_IOS", "coded", "Aaron", list(2007:2016), 
  "DOC_MBDA", "not coded", NA, list(2011:2015), # very few dates can be extracted from the text # Missing most dates
  "DOC_NIST", "not coded", NA, list(2007:2016), # NO MEMBER NAMES--FOLLOW UP FOIA 
  "DOC_NOAA", "not coded", NA, list(2016), 
  "DOC_NTIA", "not coded", NA, list(2010:2020), 
  "DOC_OCPA", "not coded", NA, list(2016),
  #"DOC_OC", "not coded", NA, list(2007:2019), # no clean script, 100 observations in the sheet and a note about missing dates and more data coming? Where are we at? 
  "DOC_OS", "not coded", NA,  list(2009:2018), # DOC-OS-2017-000958
  "DOC_SBA", "not coded", NA,  list(2010:2017), # no records before 2010
  # DOD
  "DOD_DeCA", "coded", "Devin",  list(2007:2018), # only some are on drive  # FIX MISSING DATES
  "DOD_DFAS", "not coded", NA,  list(2016:2018),
  "DOD_DLA_Aviation", "coded", "Fatima",  list(2009:2016),
  "DOD_Navy", "coded", "Delaney",  list(2013:2017),# no records before 2013
  "DOD_OIG", "coded", "Fatima",  list(2007:2019), # is this everything? only last name info --> 600+ non matches
  "DOD_OSDJS", "not coded", NA,  list(2007:2018), # some records are in text files to be merged #45, waiting on remaining records
  "DOD_USACE", "coded", "Fatima",  list(2013:2018), # no records before fall 2013
  # "DOD_USMC", "not coded", NA,  list(2007:2019), #  DON-USMC-2018-004141 needs to be converted from pdf and added to drive
  # DOE
  "DOE_FERC", "coded", NA,  list(2000:2019), # at one time it was "Devin", but no longer? # Need to to put back on drive see issue. Also get more years from eLibrary
  # DOI #25 we are missing scripts for new DOI agencies e.g. DOI OS, sometimes just called DOI, but we should avoid that 
  "DOI_BIA", "coded", "Rochelle",  list(2013:2018), #184
  "DOI_BOEM", "coded", "Aaron", list(2015:2017),
  "DOI_BSEE", "coded", "Hope",  list(2011:2016),
  "DOI_NPS", "not coded", NA,  list(2008:2018),
  "DOI_NIGC", "coded", "Fatima",  list(2015),
  "DOI_OSMRE","not coded", NA,  list(2015:2016),
  "DOI_SOL", "coded", "Hope",  list(2013:2016),
  "DOI_USGS", "coded", "Hope",  list(2010:2019),
  # DOJ 
  "DOJ_CIV", "not coded", NA,  list(2007:2020), # WHY IS THIS NOT CODED?
  "DOJ_ENRD", "coded", "Julia",  list(2006:2018),
  "DOJ_EOIR", "coded", "Julia",  list(2007:2012), 
  # "DOJ_ExecSec", "not coded", NA,  list(2007:2019), # waiting on FOIA fom DOJ_JMD/OLA
  "DOJ_FBI", "not coded", NA, list(2006:2013), #TODO confirm that this is the right date range. also, this script could use a more careful pass
  # "DOJ_INTERPOL", "not coded", NA,  list(2007:2019), # logs cover 2012-2018 but many lack dates--may be same as we will get form DOJ_ExecSec
  "DOJ_USMS", "coded", "Rochelle",  list(2013:2018),
  # DOL 
  "DOL_EBSA", "coded", "Rochelle",  list(2007:2018),
  "DOL_ETA", "coded", "Rochelle",  list(2007:2018),
  "DOL_MSHA", "coded", "Hope",  list(2007:2018), 
  "DOL_OASAM", "coded", "Rochelle",  list(2007:2017), #190
  "DOL_OFCCP", "coded", "Rochelle",  list(2007:2017),
  "DOL_OCFO", "coded", "Devin",  list(2007:2019),
  "DOL_OCIA", "coded", "Rochelle",  list(2007:2020), # in progress (8/18/2023)
  # "DOL_OALJ", "not coded", NA,  list(2007:2019), # ???
  "DOL_OSHA", "coded", "Rochelle",  list(2005:2018),
  "DOL_OWCP", "coded", "Rochelle",  list(2007:2018),
  "DOL_SOL", "coded", "Rochelle",  list(2007:2018), 
  "DOL_VETS", "coded", "Rochelle",  list(2006:2017),
  # DOS 
  # "DOS", "not coded", NA,  list(2007:2019), # waiting on dept of state foia 
  # DOT 
  "DOT_FAA", "coded", "Sam", list(2007:2017),
  "DOT_FHWA", "coded", "Rochelle",  list(2007:2019), # complete, multiple data sources merged
  "DOT_FRA", "coded", "Rochelle",  list(2007:2019), #
  "DOT_FTA", "coded", "Rochelle",  list(2007:2017), 
  "DOT_PHMSA", "coded", "Hope",  list(2007:2017),
  "DOT_SLSDC", "coded", "Aaron",  list(2008:2017),
  # Education
  "ED", "not coded", NA,  list(2007:2017),
  "EEOC", "coded", "Rochelle",  list(2017:2019), #108
  "EOP_CEQ", "not coded", NA,  list(2009:2018),
  "EOP_USTR", "coded", "Hope",  list(2007:2016), #c("Hope", "Julia"), 
  # EPA
  "EPA", "coded", "Aaron",  list(2007:2018), # c("Adam", "Avery"),
  # FCA
  "FCA", "not coded", NA,  list(2007:2016), # 30 or so out of 100 bad names, but full time period
  # FCC
  "FCC", "coded", "Devin",  list(2013:2019), #FIXME -- 2019 is partial, but still a fiar number of obs, should we omit it? 
  # FDA
  "DHHS_FDA", "coded", "Rochelle",  list(2007:2020),  # 2007-2018 now on drive, debug issue #97
  # FHFA
  "FHFA", "not coded", NA,  list(2007:2017), #
  # FMC
  # "FMC", "not coded", NA,  list(2007:2019),   # no members contacts, just OMB and reports to congress 
  #FTC
  "FTC", "not coded", NA,  list(2007:2018),
  # GSA
  # "GSA", "not coded", NA,  list(2007:2019), # 6k entries 2007-2017 on drive, but only some member names in subject, filed for others july 2018 
  # HUD
  "HUD_HQ", "not coded", NA,  list(2007:2018),
  # NARA
  "NARA", "coded", "Rochelle",  list(2013:2019),
  # NASA
  "NASA", "coded", "Rochelle",  list(2007:2019), # 200+ bad names, handful of wrong dates
  # NCPC
  "NCPC", "not coded", NA,  list(2007:2012),
  # NCUA
  "NCUA", "not coded", NA,  list(2009:2017), # Are we missing 2010, 2011, and the first half of 2014? Or is the unevenness just attention cycle? what's going on here? Also, are the dates from 2003 correct?  
  # NLRB
  "NLRB" , "not coded", NA,  list(2009:2017),
  # NWTRB
  "NWTRB", "not coded", NA,  list(2008:2009),
  # PRC
  "PRC", "not coded", NA,  list(2008:2018), # no responsive records for FY 2007 or FY 2008. Tracking did not start until FY 2009
  # RRB
  "RRB", "not coded", NA,  list(2008:2018), # not much subject content
  # SSA
  "SSA", "coded", "Rochelle",  list(2009:2016), # fair amount of bad names that coding won't help much
  # STB
  # "STB", "not coded", NA,  list(2007:2019), # need to finish merge script; only 2015-2017?
  # Treasury
  "Treasury_Fiscal", "coded", "Julia",  list(2007:2019), # we have some dat from 2018, but it is right-censored, so excluding for now. No observations from 2019
  # IRS 
  "Treasury_IRS", "coded", "Rochelle",  list( c(2007:2017, 2020) ), #28 we have some dat from 2018, but it is right-censored, so excluding for now. No observations from 2019
  "Treasury_Mint", "coded", "Rochelle",  list(2007:2012), #59 # we have data from 2013, but it is right censored, so excluding from counts 
  "Treasury_OCC", "coded", "Aaron",  list(2013:2018),
  "TVA", "not coded", NA,  list(2014:2018),
  # USDA 
  "USDA", "not coded", NA,  list(2007:2019), # 2019 is right censored, but we have at least 3.5 months
  "USDA_ARS", "not coded", NA,  list(2007:2017), # dropping 2005, keeping 2017, even thought it appears partil. a bunch of the data doesn't have dates
  "USDA_ERS", "not coded", NA,  list(2006:2016), #
  "USDA_FS", "not coded", NA,  list(2007:2019), # 2019 is right censored, but still a fair amount of data
  "USDA_NASS", "coded", "Robert",  list(2007:2019), # c("Robert", "Henry"),
  "USDA_NIFA", "not coded", NA,  list(2015:2016), 
  "USDA_NRCS", "not coded", NA,  list(2007:2019),
  "USDA_RD", "not coded", NA,  list(2010:2013),
  "USDA_RMA", "not coded", NA,  list(2010:2018), # no records before 2010 - 7 year retention 
  # USPS
  "USPS", "not coded", NA,  list(2010:2013),
  # VA
  "VA", "coded", "Rochelle", list(2008:2020), # no data before 2008
  "VA_CEM", "coded", "Fatima",  list(2017:2018),
)
