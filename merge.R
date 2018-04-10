# This script combines clean data files with other data sources.
source("setup.R")

agency <- "EPA" # the title of the R script for cleaning these data
status <- "recoded" # c("coded", "recoded", NA)
coders <- c("Adam ","Avery ") # coder names that preface the agency name in the title of their google sheet
epa <- clean.agency() # adds a sheet of unresolved coder discrepencies to drive

agency <- "DOD_Navy" 
status <- "NA"
coders <- NA
dod.navy <- clean.agency()

# combine data
data <- full_join(
  epa,
  dod.navy
)

# now merge with voteview etc. ...


#####################################
# clean up workspace before committ #
#####################################
rm(list=ls(all=TRUE)) 
