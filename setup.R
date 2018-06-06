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
  mutate(common_name = ifelse(bioname == "LABRADOR, Raúl R.", "Raul", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "HOLLINGSWORTH, Joseph Albert III", "Trey", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "NOLAN, Richard Michael", "Rick", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "SAXTON, Hugh James", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "McNERNEY, Jerry", "Gerald", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "KHANNA, Rohit", "Ro", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "KRISHNAMOORTHI, S. Raja", "Raja", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "JOHNSON, Hank", "Henry", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "  DUNCAN, John J., Jr.", "Jim", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
  mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 

  # remove accent marks
  mutate(common_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", common_name)) %>% 
  mutate(last_name = ifelse(grepl("VEL.ZQUEZ, Nydia M.", bioname), "VELAZQUEZ", last_name)) %>% 
  mutate(last_name = ifelse(grepl("C.RDENAS, Tony", bioname), "CARDENAS", last_name)) %>% 
  mutate(last_name = ifelse(grepl("GUTI.RREZ, Luis V.", bioname), "GUTIERREZ", last_name)) %>% 
  mutate(first_name = ifelse(grepl("SERRANO, Jos. E.", bioname), "Jose", first_name)) %>% 
  mutate(first_name = ifelse(grepl("CARSON, Andr.", bioname), "Andre", first_name)) %>% 
  mutate(last_name = ifelse(grepl("LUJ.N, Ben Ray", bioname), "LUJAN", last_name)) %>% 
  mutate(last_name = ifelse(grepl("BARRAG.N, Nanette Diaz", bioname), "BARRAGAN", last_name)) %>% 
  mutate(last_name = ifelse(grepl("SÁNCHEZ, Linda T.", bioname), "SANCHEZ", last_name)) %>% 
  mutate(first_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", first_name)) %>% 
  mutate(first_name = ifelse(grepl("HINOJOSA, Rubén", bioname), "Ruben", first_name)) %>% 

  
  
 
  mutate(middle_name = ifelse(grepl("PLATTS, Todd", bioname), "Russell", middle_name)) %>% 
  

  
  mutate(common_name = ifelse(bioname == "DENT, Charles W.", "Charlie", common_name)) %>% 
  # middle initials
  mutate(middle_initial = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BOND, Christopher Samuel (Kit)", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "PENCE, Mike", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "LANGEVIN, James", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "WEST, Allen", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "RISCH, James", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "HIRONO, Mazie", "K", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "DJOU, Charles", "K", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "GILLIBRAND, Kirsten", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "KUCINICH, Dennis", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SMITH, Adrian", "M", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "THUNE, John", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "WALZ, Tim", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SARBANES, John", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SALAZAR, John", "T", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "UDALL, Mark", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "THOMPSON, Bennie", "G", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BENNETT, Robert", "F", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "PLATTS, Todd", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "DELAHUNT, Bill", "D", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "RICHARDSON, Laura", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "TIBERI, Patrick (Pat)", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BRALEY, Bruce", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "HAGAN, Kay", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "WARNER, Mark", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BEYER, Donald Sternoff Jr.", "E", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "RICHMOND, Cedric", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "ADERHOLT, Robert", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "McKINLEY, David", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SEWELL, Terri", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "HANNA, Richard", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "FLEISCHMANN, Chuck", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "MURPHY, Christopher", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "SCHIFF, Adam", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "HONDA, Mike", "M", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTWRIGHT, Matt", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "LOWENTHAL, Alan", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "ADAMS, Alma", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "JOYCE, David", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "WENSTRUP, Brad", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "RENACCI, Jim", "B", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "ROTHFUS, Keith", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "LIEU, Ted", "W", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "DUFFY, Sean", "P", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "RIBBLE, Reid", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BARLETTA, Lou", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "TONKO, Paul", "D", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  
  # first names
  mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>% 
  mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%
  mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name)) 

# make blank common names NA
members %<>%
   mutate(common_name = ifelse(members$common_name=="", NA,  members$common_name))

members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "JEWELL"; members$first_name[nrow(members)] <- "Sally"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "NORTON"; members$first_name[nrow(members)] <- "Eleanor"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "SABLAN"; members$first_name[nrow(members)] <- "Gregorio"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PLASKETT"; members$first_name[nrow(members)] <- "Stacey"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "RADEWAGEN"; members$first_name[nrow(members)] <- "Amata"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "CHRISTENSEN"; members$first_name[nrow(members)] <- "Donna"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PIERLUISI"; members$first_name[nrow(members)] <- "Pedro"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "WHITEHOUSE";
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "BORDALLO"; members$first_name[nrow(members)] <- "Madeleine"
members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "FALEOMAVAEGA"; members$first_name[nrow(members)] <- "Eni"


# select
members %<>% 
  select(first_name, first_initial ,common_name, middle_name, middle_initial, last_name, bioname, everything()) 

# NOTE: 
# Voteview is missing non-voting members:
# American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
# District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
# Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
# Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
# Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
# U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014


members$congresses <- NA # this list format throughs errors in merge





