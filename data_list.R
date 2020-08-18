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
  "Treasury_IRS", "coded", NA, #28
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
