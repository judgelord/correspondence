options(stringsAsFactors = FALSE)
#devtools::install_github("voteview/Rvoteview")
library(tidyverse)
library(magrittr)
library(googlesheets)
library(googledrive)
library(Rvoteview)
library(stringi)

members <- member_search(congress = c(110:120)) %>% # get voteview data for selected Congresses
  mutate(last_name = gsub(", .*", "", bioname)) %>%
  mutate(first_name = gsub("^.*?, |, Jr.|, III| III", "", bioname)) %>%
  mutate(common_name = stringr::str_extract(bioname, "\\(.*\\)")) %>%
  mutate(common_name = gsub("\\)|\\(", "", common_name)) %>%
  mutate(first_name = gsub("\\(.*\\)", "", first_name)) %>%
  mutate(middle_name = stringr::str_extract(first_name, " .*")) %>%
  mutate(middle_name = gsub(" ", "", middle_name)) %>%
  mutate(middle_initial = substr(middle_name, 1, 1)) %>%
  mutate(first_name = gsub(" .*", "", first_name)) %>%
  select(first_name, common_name, middle_name, middle_initial, last_name, everything())

stateFromLower(members$state)
source("stateFromLower.R") # format state names
source("clean.R") 

gs_ls() # log in to google


#####################################
# clean up workspace before commit #
#####################################
rm(list = ls(all = TRUE))
