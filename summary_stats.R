# this file is a sketchpad for calculating stats. See summary.html for selected stats 

options(stringsAsFactors = FALSE)
requires <- c("dplyr", "magrittr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
library(dplyr) 
library(magrittr)

# Refresh data? Or load archived data file from https://drive.google.com/drive/u/0/folders/1DSGGZP_v2zwdfxg9Do3Ii4Y8UdXultVg
ifelse( F ,  source("merge.R"), load("correspondence.RData") )

