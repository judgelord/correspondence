options(stringsAsFactors = FALSE)
# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("googlesheets")
# install.packages("googledrive")
# install.packages("devtools")
# install.packages("stringi")
# devtools::install_github("voteview/Rvoteview")
library(tidyverse)
library(magrittr)
library(googlesheets)
library(googledrive)
library(Rvoteview)
library(stringi)
source("stateFromLower.R") # format state names
source("clean.R") # data cleaning and intercoder agreement functions 

gs_ls() # log in to google

members <- member_search(congress = c(110:120)) %>% # get voteview data for selected Congresses
  # format state
  mutate(state = tolower(state)) %>%
  mutate(state = as.character(state)) %>%
  # extract first, middle, last, and common names
  mutate(last_name = gsub(", .*", "", bioname)) %>%
  mutate(first_name = gsub("^.*?, |, Jr.| Jr.|, III| III| IV", "", bioname)) %>%
  mutate(first_name = gsub(", II| II", "", first_name)) %>%
  mutate(common_name = stringr::str_extract(bioname, "\\(.*\\)")) %>%
  mutate(common_name = gsub("\\)|\\(", "", common_name)) %>%
  mutate(first_name = gsub("\\(.*\\)", "", first_name)) %>%
  mutate(middle_name = stringr::str_extract(first_name, " .*")) %>%
  mutate(middle_name = gsub(" ", "", middle_name)) %>%
  mutate(middle_initial = substr(middle_name, 1, 1)) %>%
  mutate(first_name = gsub(" .*", "", first_name)) %>%
  mutate(common_name = ifelse(is.na(common_name), "", common_name)) %>%
  # correct mistakes
  mutate(common_name = ifelse(bioname == "BUNNING, James Paul David", "Jim", common_name)) %>%
  mutate(common_name = ifelse(bioname == "FORBES, J. Randy", "Randy", common_name)) %>%
  mutate(common_name = ifelse(bioname == "GRIFFITH, H. Morgan", "Morgan", common_name)) %>%
  mutate(common_name = ifelse(bioname == "DURBIN, Richard Joseph", "Dick", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BARLETTA, Lou", "Lou", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BUCHANAN, Vernon G.", "Vern", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "SCHAKOWSKY, Janice D.", "Jan", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "KOHL, Herbert H.", "Herb", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "STEARNS, Clifford Bundy", "Cliff", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "SNYDER, Victor F.", "Vic", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "EVERETT, Robert Terry", "Terry", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "WAMP, Zachary Paul", "Zach", common_name)) %>%
  mutate(common_name = ifelse(bioname == "DEAL, John Nathan", "Nathan", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "COCHRAN, William Thad", "Thad", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "GOODLATTE, Robert William", "Bob", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "THOMPSON, Michael", "Mike", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "WILSON, Charlie", "Charles", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "PENCE, Mike", "Michael", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "GAETZ, Matthew L. II", "Matt", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "STABENOW, Deborah Ann", "Debbie", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "VAN HOLLEN, Christopher", "Chris", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DENT, Charles W.", "Charlie", common_name)) %>% 
  
  
  mutate(middle_initial = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BOND, Christopher Samuel (Kit)", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "PENCE, Mike", "R", middle_initial)) %>% 
  
  
  mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>% 
  mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%

  mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name)) 
  
members %<>%
   mutate(common_name = ifelse(members$common_name=="", NA,  members$common_name))

  

  
  # select
  members %<>% select(first_name, common_name, middle_name, middle_initial, last_name, bioname, everything())

# NOTE: 
# Voteview is missing non-voting members:
# American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
# District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
# Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
# Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
# Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
# U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014


members$congresses <- NA # this list format throughs errors in merge





