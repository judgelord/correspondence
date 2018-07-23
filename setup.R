options(stringsAsFactors = FALSE)

requires <- c("dplyr", "ggplot2", "gdata", "magrittr","googlesheets","googledrive","devtools","stringi","stringr", "tidyverse")
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
library(stringr)
library(gdata)
library(readr)

source("stateFromLower.R") # format state names
source("clean.R") # data cleaning and intercoder agreement functions 
source("members/nameCongress.R") # augments voteview member names
source("members/nameMethods.R") # functions for cleaning member names to match the augmented member file
source("committees/committees.R")
gs_ls() # log in to google

