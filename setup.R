options(stringsAsFactors = FALSE)
# devtools::install_github("voteview/Rvoteview")
library(tidyverse)
library(magrittr)
library(googlesheets)
library(googledrive)
library(Rvoteview)

members <- member_search(congress = c(110:120)) %>% # get voteview data for selected Congresses
  mutate(last_name = gsub(", .*", "", bioname)) %>%
  mutate(first_name = gsub(".+?, ", "", bioname)) # FIX THIS

stateFromLower(members$state)
source("stateFromLower.R") # format state names
source("clean.R") 

gs_ls() # log in to google

