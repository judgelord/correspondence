source("setup.R")

source("EPA.R")
epa <- clean.EPA("Adam EPA")
source("DOD_Navy.R")
dod.navy <- clean.DOD_NAVY("NAVY.csv")

data <- full_join(
  epa,
  dod.navy
)

# now merge with voteview etc. ...


rm(list=ls(all=TRUE)) # clean up workspace 
