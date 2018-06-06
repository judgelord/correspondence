options(stringsAsFactors = FALSE)

requires <- c("tidyverse","magrittr","googlesheets","googledrive","devtools","stringi","stringr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(requires[to_install]) 

if(require("Rvoteview")==F) {
  devtools::install_github("voteview/Rvoteview")
}
library(tidyverse)
library(magrittr)
library(googlesheets)
library(googledrive)
library(Rvoteview)
library(stringi)
library(stringr)
source("stateFromLower.R") # format state names
source("clean.R") # data cleaning and intercoder agreement functions 
source("nameCongress.R") # augments voteview member names
source("nameMethods.R") # functions for cleaning member names to match the augmented member file

gs_ls() # log in to google

