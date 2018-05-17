options(stringsAsFactors = FALSE)
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
  
  
  mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>% 
  mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%
  mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name))




  

  
  
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


# Formats last_name to similiar format as members$last_name
# Capitalizes letters and fixes common errors 
formatLastName <- function(data){
  
  data %<>%
    mutate(last_name = str_to_upper(last_name)) %>% 
    # correct capitalization
    mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
    mutate(last_name = gsub("DEFAZIO", replacement = "DeFAZIO", last_name)) %>% 
    mutate(last_name = gsub("DELAURO", replacement = "DeLAURO", last_name)) %>% 
    mutate(last_name = gsub("DEMINT", replacement = "DeMINT", last_name)) %>% 
    mutate(last_name = gsub("LOBIONDO", replacement = "LoBIONDO", last_name)) %>% 
    mutate(last_name = gsub("LATOURETTE", replacement = "LaTOURETTE", last_name)) %>% 
    mutate(last_name = gsub("LAHOOD", replacement = "LaHOOD", last_name)) %>% 
    mutate(last_name = gsub("DEGETTE", replacement = "DeGETTE", last_name)) %>% 
    mutate(last_name = gsub("DELBENE", replacement = "DelBENE", last_name)) %>% 
    mutate(last_name = gsub("DESANTIS", replacement = "DeSANTIS", last_name)) %>% 
    mutate(last_name = gsub("MACARTHUR", replacement = "MacARTHUR", last_name)) %>% 
    mutate(last_name = gsub("LAMALFA", replacement = "LaMALFA", last_name)) %>% 
    # Spelling and specific corrections
    mutate(last_name = gsub("DUNCAN JOHN.*", replacement = "DUNCAN", last_name)) %>% 
    mutate(last_name = gsub("JOHNSON HENRY.*", replacement = "JOHNSON", last_name)) %>% 
    mutate(last_name = gsub("BONO MACK.*", replacement = "BONO", last_name)) %>% 
    mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% 
    mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Lujan, Ben.*", FROM),gsub("Lujan, Ben.*", replacement = "LUJÁN", FROM), last_name)) %>% 
    mutate(last_name = gsub("MOORE CAPITO.*", replacement = "CAPITO", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Milkulski, Barbara", FROM), "MIKULSKI", last_name)) %>% 
    mutate(last_name = ifelse(grepl("GRESHAM BARRETT", last_name), "BARRETT", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Shelley Moore", FROM), "CAPITO", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Cathy McMorris|McMorris, Cathy", FROM), "McMORRIS RODGERS", last_name)) %>% 
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Jackson",FROM)&grepl("She",FROM)&grepl("Lee",FROM), "JACKSON LEE", last_name)) %>% 
    mutate(last_name = gsub("GONZALES", replacement = "GONZALEZ", last_name)) 
    
  
  
  return(data)
  
}

# Formats first_name to similiar format as members$first_name
# Capitalizes letters appropriately and fixes common errors 
formatFirstName <- function(data){
  
  data %<>%
    mutate(first_name = stri_trans_totitle(first_name)) %>% 
    mutate(first_name = ifelse( grepl("Don",FROM)&grepl("Young",FROM), "Donald", first_name)) %>%
    mutate(first_name = ifelse( grepl("Andr",FROM)&grepl("Carson",FROM), "André", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM)&grepl("Thune",FROM), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM)&grepl("Rockefeller",FROM), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("Harold",FROM)&grepl("Rogers",FROM), "Harold", first_name)) %>%
    
    mutate(first_name = ifelse( grepl("James",FROM)&grepl("Sensenbrenner",FROM), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Richard",FROM)&grepl("Blumenthal",FROM), "Richard", first_name)) %>%
    mutate(first_name = ifelse( grepl("Bill",FROM)&grepl("Nelson",FROM), "Clarence", first_name)) %>%
    mutate(first_name = ifelse( grepl("Fred",FROM)&grepl("Upton",FROM), "Frederick", first_name)) %>%
    mutate(first_name = ifelse( grepl("Thad",FROM)&grepl("Cochran",FROM), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Kristen",FROM)&grepl("Gillibrand",FROM), "Kirsten", first_name)) %>%
    mutate(first_name = ifelse( grepl("C.A|C. A",FROM)&grepl("Ruppersberger",FROM), "Dutch", first_name)) %>%
    mutate(first_name = ifelse( grepl("Paul",FROM)&grepl("Gosar",FROM), "Paul", first_name)) %>%
    mutate(first_name = ifelse( grepl("Ros-Lehtinen",FROM), "Ileana", first_name)) %>%
    mutate(first_name = ifelse( grepl("Beutler",FROM)&grepl("Herrera",FROM), "Jaime", first_name)) %>%
    mutate(first_name = ifelse( grepl("Will|Bill",FROM)&grepl("Owens",FROM), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Butterfield",FROM)&grepl("G",FROM), "George", first_name)) %>%
    mutate(first_name = ifelse( grepl("Young",FROM)&grepl("C.W|C. W|CW",FROM), "Charles", first_name)) %>%
    mutate(first_name = ifelse( grepl("Jackson",FROM)&grepl("She",FROM)&grepl("Lee",FROM), "Sheila", first_name)) %>%
    mutate(first_name = ifelse( grepl("Gresham",FROM)&grepl("Barret",FROM), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Putnam",FROM)&grepl("Ad",FROM), "Adam", first_name)) %>%
    mutate(first_name = ifelse( grepl("Lind",FROM)&grepl("Graham",FROM), "Lindsey", first_name)) %>%
    
    mutate(first_name = gsub(pattern = "Christoher", replacement = "Christopher", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Hilllary|Hilary", replacement = "Hillary", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Babara", replacement = "Barbara", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Colin", replacement = "Collin", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Melisssa", replacement = "Melissa", first_name)) %>% 
    
    
    mutate(first_name = gsub("Duncan John.*", replacement = "John", first_name)) %>% 
    mutate(first_name = gsub("Johnson Henry.*", replacement = "Henry", first_name))
  
  
  return(data)
  
}




