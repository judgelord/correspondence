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
source("nameMethods.R") # functions for cleaning member names

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
  # common names
  mutate(common_name = ifelse(first_name == "Daniel", "Dan", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Dan")&(common_name==""), "Daniel", common_name)) %>% 
  mutate(common_name = ifelse(first_name == "Michael", "Mike", common_name)) %>% 
  mutate(common_name = ifelse(first_name == "Joe", "Joseph", common_name)) %>% 
  mutate(common_name = ifelse(first_name == "Mike", "Michael", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "David")&(common_name==""), "Dave", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Dave")&(common_name==""), "David", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Thomas")&(common_name==""), "Tom", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Tom")&(common_name==""), "Thomas", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Kathleen")&(common_name==""), "Kathy", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Kathy")&(common_name==""), "Kathleen", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Patrick")&(common_name==""), "Pat", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Pat")&(common_name==""), "Patrick", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "James")&(common_name==""), "Jim", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Jim")&(common_name==""), "James", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Pete")&(common_name==""), "Peter", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Peter")&(common_name==""), "Pete", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Richard")&(common_name==""), "Rich", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Chris")&(common_name==""), "Christopher", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Christopher")&(common_name==""), "Chris", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Robert")&(common_name==""), "Bob", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "William")&(common_name==""), "Bill", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Bill")&(common_name==""), "William", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Melvin")&(common_name==""), "Mel", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Jeffrey")&(common_name==""), "Jeff", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Ben")&(common_name==""), "Benjamin", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Benjamin")&(common_name==""), "Ben", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Charles")&(common_name==""), "Charlie", common_name)) %>% 
  mutate(common_name = ifelse(  (first_name == "Charlie")&(common_name==""), "Charles", common_name)) %>% 
  
  
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
  mutate(common_name = ifelse(bioname == "ROSS, Michael Avery", "Mike", common_name)) %>% 
 
  mutate(common_name = ifelse(bioname == "AKIN, W. Todd", "Todd", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "GRAVES, Samuel", "Sam", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "UPTON, Frederick Stephen", "Fred", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "MURPHY, Timothy", "Tim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARNEY, Chris", "Christopher", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CAMP, David Lee", "Dave", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CRAPO, Michael Dean", "Mike", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "LANGEVIN, James", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "SANDERS, Bernard", "Bernie", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "PAUL, Ronald Ernest", "Ron", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "YOUNG, Donald Edwin", "Don", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "COBURN, Thomas Allen", "Tom", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BUYER, Stephen Earle", "Steve", common_name)) %>%  
  mutate(common_name = ifelse(bioname == "WYDEN, Ronald Lee", "Ron", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DELAHUNT, Bill", "William", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BOYD, F. Allen, Jr.", "Allen", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "HOEKSTRA, Peter", "Pete", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "REICHERT, David G.", "Dave", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BOREN, Daniel David", "Dan", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "MACK, Connie, IV", "Connie", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "HALVORSON, Deborah L.", "Debbie", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "COHEN, Stephen", "Steve", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "INGLIS, Robert Durden", "Bob", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "ETHERIDGE, Bobby R.", "Bob", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BOUCHER, Frederick C.", "Rick", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "RYAN, Timothy J.", "Tim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "KAGEN, Steven", "Steve", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BURTON, Danny Lee", "Dan", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "LATHAM, Thomas", "Tom", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "COOPER, James Hayes Shofner", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "GORDON, Barton Jennings", "Bart", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DICKS, Norman DeValois", "Norm", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "HONDA, Mike", "Michael", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "JOHNSON, Ron", "Ronald", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "MEEHAN, Patrick", "Pat", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "SCHILLING, Bobby", "Robert", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "RIGELL, E. Scott", "Scott", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "WILLIAMS, Roger", "John", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "BERRY, Robert Marion", "Marion", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
  

  
  mutate(common_name = ifelse(bioname == "DENT, Charles W.", "Charlie", common_name)) %>% 
  # middle initials
  mutate(middle_initial = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BOND, Christopher Samuel (Kit)", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "PENCE, Mike", "R", middle_initial)) %>% 
  # first names
  mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>% 
  mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%
  mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name)) 

# make blank common names NA
members %<>%
   mutate(common_name = ifelse(members$common_name=="", NA,  members$common_name))

# select
members %<>% 
  select(first_name, common_name, middle_name, middle_initial, last_name, bioname, everything())

# NOTE: 
# Voteview is missing non-voting members:
# American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
# District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
# Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
# Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
# Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
# U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014


members$congresses <- NA # this list format throughs errors in merge




