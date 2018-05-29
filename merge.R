# This script combines clean log/letter files with other data sources.
# agency = the title of the R script for cleaning these data
# status = c("coded", "recoded", "not coded"), NA if not yet coded
# coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets
# clean.agency() # cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# load functions
source("setup.R")

# Departments and agencies are listed A-Z

# DHHS 
agency.list <- as.matrix(
c("DHHS_ACL",   "not coded", NA),
c"DHHS_CDC",    "not coded", NA),
c("DHHS_HRSA", "not coded", NA),



# DHS
"DHS"
"coded"
"Katie" # c("Katie", "Megha") # Megha's work is not there
DHS
DHS %<>%  #, by = c("congress", "chamber", "first_name", "last_name"))

"DHHS_ICE"
"not coded"
NA # c("Katie", "Megha") # Megha's work is not there
DHS
DHS_ICE %<>% 


# DOD 
"DOD_DLA_Aviation"
"not coded"
NA
DOD_DLA_Aviation
DOD_DLA_Aviation %<>%  #, by = c("congress", "chamber", "first_name", "last_name"))

"DOD_Navy"
"coded"
c("Delaney")
DOD_Navy 
DOD_Navy %<>%  #, by = c("congress", "chamber", "last_name"))

"DOE_FERC"
"not coded"
NA
DOE_FERC 
DOE_FERC %<>% select(-id)
DOE_FERC %<>%  

# DOL
"DOL_EBSA"
"not coded"
NA
DOL_EBSA 
DOL_EBSA %<>% 

"DOL_OCFO"
"coded"
"Devin"
DOL_OCFO 
DOL_OCFO %<>% 

"DOL_OFCCP"
"not coded"
NA
DOL_OFCCP 
DOL_OFCCP %<>% 

"DOL_VETS"
"not coded"
NA
DOL_VETS 
DOL_VETS %<>% 

# DOT 
"DOT_FAA"
"coded"
c("Sam")
DOT_FAA
DOT_FAA %<>% select(-middle_name) 
DOT_FAA %<>% 

#EPA
"EPA" # the title of the R script for cleaning these data
"coded" # c("coded", "recoded", "not coded") NA if not yet coded
c("Adam", "Avery") # coder names that preface the agency name in the title of their google sheet
EPA 
EPA %<>% select(-middle_name) 
EPA %<>%  #, by = c("last_name", "congress", "chamber", "state"))

#FCC
"FCC"
"coded"
"Devin"
FCC

#PRC
"PRC"
"not coded"
NA
PRC
PRC %<>%  #, by = c("congress", "chamber", "last_name", "state")) # matching on state may fail to match out-of-state advocacy, but false positives without it

# USDA 
"USDA"
"not coded"
NA
USDA
USDA %<>%  #, by = c("congress", "chamber", "last_name"))
# (still have a false positive problem with Johnson and Rogers, hard to match without state or chamber)

"USDA_ERS"
"not coded"
NA
USDA_ERS 
USDA_ERS %<>%  #, by = c("congress", "chamber", "last_name"))

"USDA_FS"
"not coded"
NA
USDA_FS
USDA_FS %<>%  #, by = c("congress", "first_name", "last_name"))

"USDA_NASS"
"coded"
c("Robert", "Henry")
USDA_NASS
USDA_NASS %<>%  #, by = c("congress", "first_name", "last_name"))

"USDA_NRCS"
"not coded"
NA
USDA_NRCS
USDA_NRCS %<>%  #, by = c("congress", "chamber", "last_name")) 

"USDA_RD"
"not coded"
NA
USDA_RD 
USDA_RD %<>%  #, by = c("congress", "first_name", "last_name"))
)

# merge data
data <- plyr::join_all(list(
  DHS,
  DOD_DLA_Aviation,
  DOD_Navy,
  DOE_FERC,
  DOL_EBSA,
  DOL_OCFO,
  DOL_OFCCP,
  DOL_VETS,
  DOT_FAA,
  EPA,
  FCC,
  PRC,
  USDA,
  USDA_ERS,
  USDA_FS,
  USDA_NASS,
  USDA_NRCS,
  USDA_RD
  ), type = 'full')

data$department <- gsub("_.*", "", data$agency)

data %<>% 
  mutate(bioname = ifelse(is.na(bioname), "", bioname)) %>% 
  mutate(party_name = ifelse(is.na(party_name), "", party_name)) %>% 
  mutate(chamber = ifelse(is.na(chamber), "", chamber)) %>% 
  filter(bioname != "PAYNE, Donald Milford" | DATE < as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
  filter(bioname != "PAYNE, Donald, Jr." | DATE > as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
  filter(bioname != "SPECTER, Arlen" | party_name != "Democratic Party" | DATE > as.Date("2009-04-28")) %>% # SPECTER, Arlen changed to DEM
  filter(bioname != "SPECTER, Arlen" | party_name != "Republican Party" | DATE < as.Date("2009-04-28")) %>% 
  filter(bioname != "GRIFFITH, Parker" | party_name != "Republican Party" | DATE > as.Date("2009-12-22")) %>% # GRIFFITH, Parker changed to GOP
  filter(bioname != "GRIFFITH, Parker" | party_name != "Democratic Party" | DATE < as.Date("2009-12-22")) %>%
  filter(bioname != "GILLIBRAND, Kirsten" | chamber != "House" | DATE < as.Date("2009-01-26")) %>% # GILLIBRAND APPOINTED TO SENATE FROM HOUSE January 26, 2009
  filter(bioname != "GILLIBRAND, Kirsten" | chamber != "Senate" | DATE > as.Date("2009-01-26")) %>%
  filter(bioname != "MARKEY, Edward John" | chamber != "House" | DATE < as.Date("2013-06-25")) %>% # # Rep Ed Markey elected to Senate in special election June 25, 2013
  filter(bioname != "MARKEY, Edward John" | chamber != "Senate" | DATE > as.Date("2013-06-25")) 

problems <- data %>% group_by(agency, ID, FROM, first_name, last_name) %>% tally() %>% filter(n>1)


###################
# summay analysis # TO BE MOVED TO ANOTHER FILE 
###################

# identify top members

mocs <- data %>% filter(!is.na(bioname), !is.na(chamber), bioname != "", chamber %in% c("House", "Senate")) %>%
  group_by(bioname, congress, chamber, department) %>%
  tally() %>% ungroup() 

mocs %<>% group_by(department) %>% mutate(percent = dplyr::ntile(n,100)) %>% ungroup()

mocs$name <- gsub(",.*", "", mocs$bioname)


# plot by nominate and dept
mocs %>%
  ggplot() +
  geom_text(
    aes(x = congress, y = chamber, label = paste0(name, "(", n,")"), size = percent, alpha = percent, color = nominate.dim1),
    position=position_jitter(width=0,height=.4)
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  scale_x_continuous(breaks = seq(110, 115, 1), limits = c(110,115)) + 
  facet_grid(department ~ .)  +
  labs(y = "", 
       title = paste("")) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #legend.text = element_blank(),
    panel.grid = element_blank()
  ) 



# plot by nominate and dept
mocsTYPE <- data %>% filter(!is.na(bioname), !is.na(chamber), bioname != "", chamber %in% c("House", "Senate")) %>%
  group_by(bioname, nominate.dim1, chamber, department, TYPE) %>%
  tally() %>% ungroup() 

mocsTYPE %<>% group_by(department) %>% mutate(percent = dplyr::cume_dist(n)) %>% ungroup()

mocsTYPE$name <- gsub(",.*", "", mocsTYPE$bioname)

mocsTYPE %>% filter(!is.na(TYPE)) %>% filter(agency != "DHS") %>%
  ggplot() +
  geom_text(
    aes(x = TYPE, y = chamber, label = name, size = n, alpha = percent, color = nominate.dim1),
    position=position_jitter(width=0,height=.4)
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  #scale_x_continuous(breaks = seq(110, 115, 1), limits = c(110,115)) + 
  facet_grid(department ~ .)  +
  labs(y = "", 
       title = paste("")) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #legend.text = element_blank(),
    panel.grid = element_blank()
  ) 









#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
