# This script combines clean log/letter files with other data sources.
# agency = the title of the R script for cleaning these data
# status = c("coded", "recoded", "not coded"), NA if not yet coded
# coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets
# clean.agency() # cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# load functions
source("setup.R")

# Departments and agencies are listed A-Z

# DHS
agency <- "DHS"
status <- "coded"
coders <- "Katie" # c("Katie", "Megha") # Megha's work is not there
DHS <- clean.agency()
DHS %<>% left_join(members) #, by = c("congress", "chamber", "first_name", "last_name"))

# DOD 
agency <- "DOD_DLA_Aviation"
status <- "not coded"
coders <- NA
DOD_DLA_Aviation <- clean.agency()
DOD_DLA_Aviation %<>% left_join(members) #, by = c("congress", "chamber", "first_name", "last_name"))

agency <- "DOD_Navy"
status <- "coded"
coders <- c("Delaney")
DOD_Navy <- clean.agency() 
DOD_Navy %<>% left_join(members) #, by = c("congress", "chamber", "last_name"))

agency <- "DOE_FERC"
status <- "not coded"
coders <- NA
DOE_FERC <- clean.agency() 
DOE_FERC %<>% left_join(members) 

# DOL
agency <- "DOL_EBSA"
status <- "not coded"
coders <- NA
DOL_EBSA <- clean.agency() 
DOL_EBSA %<>% left_join(members)

agency <- "DOL_OCFO"
status <- "not coded"
coders <- NA
DOL_OCFO <- clean.agency() 
DOL_OCFO %<>% left_join(members)

agency <- "DOL_OFCCP"
status <- "not coded"
coders <- NA
DOL_OFCCP <- clean.agency() 
DOL_OFCCP %<>% left_join(members)

agency <- "DOL_VETS"
status <- "not coded"
coders <- NA
DOL_VETS <- clean.agency() 
DOL_VETS %<>% left_join(members)

# DOT 
agency <- "DOT_FAA"
status <- "coded"
coders <- c("Sam")
DOT_FAA <- clean.agency()

#EPA
agency <- "EPA" # the title of the R script for cleaning these data
status <- "coded" # c("coded", "recoded", "not coded") NA if not yet coded
coders <- c("Adam", "Avery") # coder names that preface the agency name in the title of their google sheet
EPA <- clean.agency() 
EPA %<>% select(-middle_name) 
EPA %<>% left_join(members) #, by = c("last_name", "congress", "chamber", "state"))

#FCC
agency <- "FCC"
status <- "coded"
coders <- "Devin"
FCC <- clean.agency()

#PRC
agency <- "PRC"
status <- "not coded"
coders <- NA
PRC <- clean.agency()
PRC %<>% left_join(members) #, by = c("congress", "chamber", "last_name", "state")) # matching on state may fail to match out-of-state advocacy, but false positives without it

# USDA 
agency <- "USDA"
status <- "not coded"
coders <- NA
USDA <- clean.agency()
USDA %<>% left_join(members) #, by = c("congress", "chamber", "last_name"))
# (still have a false positive problem with Johnson and Rogers, hard to match without state or chamber)

agency <- "USDA_ERS"
status <- "not coded"
coders <- NA
USDA_ERS <- clean.agency() 
USDA_ERS %<>% left_join(members) #, by = c("congress", "chamber", "last_name"))

agency <- "USDA_FS"
status <- "not coded"
coders <- NA
USDA_FS <- clean.agency()
USDA_FS %<>% left_join(members) #, by = c("congress", "first_name", "last_name"))

agency <- "USDA_NASS"
status <- "coded"
coders <- c("Robert", "Henry")
USDA_NASS <- clean.agency()
USDA_NASS %<>% left_join(members) #, by = c("congress", "first_name", "last_name"))

agency <- "USDA_NRCS"
status <- "not coded"
coders <- NA
USDA_NRCS <- clean.agency()
USDA_NRCS %<>% left_join(members) #, by = c("congress", "chamber", "last_name")) 

agency <- "USDA_RD"
status <- "not coded"
coders <- NA
USDA_RD <- clean.agency() 
USDA_RD %<>% left_join(members) #, by = c("congress", "first_name", "last_name"))


# merge data
data <- plyr::join_all(list(
  DHS,
  # DOD_DLA_Aviation,
  DOD_Navy,
  EPA,
  # PRC
  USDA
  # USDA_ERS,
  #USDA_FS,
  #USDA_NASS,
  # USDA_NRCS
  # USDA_RD
  ), type = 'full')



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

mocs <- data %>% filter(!is.na(last_name), !is.na(chamber), last_name != "", chamber %in% c("House", "Senate")) %>%
  group_by(last_name, congress, nominate.dim1, chamber, agency) %>%
  tally() %>% ungroup() 


# plot by nominate and agency
mocs %>%
  ggplot() +
  geom_text(
    aes(x = congress, y = agency, label = last_name, size = n, alpha = n, color = nominate.dim1),
    position=position_jitter(width=0,height=.4)
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  scale_x_continuous(breaks = seq(110, 115, 1), limits = c(110,115)) + 
  facet_grid(. ~ chamber)  +
  labs(y = "", 
       title = paste("")) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #legend.text = element_blank(),
    panel.background = element_blank()
  ) 




# plot by nominate and TYPE
agenciesToPlot <- c("EPA", "DOT_FAA", "DOD_Navy", "DHS")
data %>% group_by(last_name, congress, nominate.dim1, chamber, TYPE, agency) %>%
  tally() %>% ungroup() %>%
  filter(agency %in% agenciesToPlot & TYPE != 0 & TYPE != 6 & !is.na(chamber)) %>%
  ggplot() +
  geom_jitter(aes(x = congress, y = agency,  color = nominate.dim1, size = n),
              alpha = .5) +
  scale_colour_gradient2(low = "red", mid = "grey", high = "blue") +
  geom_text(
    data = data %>% group_by(last_name, congress, nominate.dim1, chamber, TYPE, agency) %>%
      tally() %>% ungroup() %>%
      filter(n > 10 & agency %in% agenciesToPlot & TYPE != 0 & TYPE != 6 & !is.na(chamber)),
    aes(x = congress, y = agency, label = last_name, size = n/4 ),
    position=position_jitter(width=0,height=.4)
  ) +
  facet_grid(TYPE ~ chamber)  +
  labs(y = "", 
       title = paste("Letters to ", agenciesToPlot)) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) 


#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
