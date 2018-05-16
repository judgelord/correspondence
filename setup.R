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
  mutate(first_name = gsub("^.*?, |, Jr.| Jr.|, III| III| IV", "", bioname)) %>%
  mutate(first_name = gsub(", II| II", "", first_name)) %>%
  mutate(common_name = stringr::str_extract(bioname, "\\(.*\\)")) %>%
  mutate(common_name = gsub("\\)|\\(", "", common_name)) %>%
  mutate(first_name = gsub("\\(.*\\)", "", first_name)) %>%
  mutate(middle_name = stringr::str_extract(first_name, " .*")) %>%
  mutate(middle_name = gsub(" ", "", middle_name)) %>%
  mutate(middle_initial = substr(middle_name, 1, 1)) %>%
  mutate(first_name = gsub(" .*", "", first_name)) %>%
  mutate(state = tolower(state)) %>%
  mutate(common_name = ifelse(is.na(common_name), "", common_name)) %>%
  mutate(common_name = ifelse(bioname == "BUNNING, James Paul David", "Jim", common_name)) %>%
  select(first_name, common_name, middle_name, middle_initial, last_name, bioname, everything())
# missing non-voting members:
# American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
# District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
# Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
# Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
# Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
# U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014


# Formats last_name to similiar format as members$last_namme
# Capitalizes letters and fixes common errors 
formatLastName <- function(data){
  
  data %<>%
    mutate(last_name = str_to_upper(last_name)) %>% 
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
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) 
  
  return(data)
  
}

members$congresses <- NA

source("stateFromLower.R") # format state names
source("clean.R") 

gs_ls() # log in to google

