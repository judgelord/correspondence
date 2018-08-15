  options(stringsAsFactors = FALSE)
  
  requires <- c("gmailr", "dplyr", "ggplot2", "gdata", "magrittr","googlesheets","googledrive","devtools","stringi","stringr", "tidyverse")
  to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
  install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
  
  if(require("Rvoteview")==F) {
    devtools::install_github("voteview/Rvoteview")
  }
  library(tidyverse)
  library(dplyr) # in case tydyverse fails (problem on linux)
  library(ggplot2)
  library(magrittr)
  library(googlesheets)
  library(googledrive)
  library(Rvoteview)
  library(stringi)

  source("functions/clean.R") # data cleaning and intercoder agreement functions 
  source("functions/stateFromLower.R") # format state names
  source("functions/dateMethods.R")
  source("functions/nameMethods.R") # functions for cleaning member names to match the augmented member file
  
  source("members/nameCongress.R") # augments voteview member names
  source("members/MemberNameDateCorrections.R")
  
  source("committees/committees.R")
  
gs_ls() # log in to google

