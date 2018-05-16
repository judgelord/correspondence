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
coders <- "Katie" # c("Katie", "Megha")
DHS <- clean.agency()
DHS %<>% left_join(members, by = c("congress", "chamber", "last_name"))

# DOD 
agency <- "DOD_DLA_Aviation"
status <- "not coded"
coders <- NA
DOD_DLA_Aviation <- clean.agency()

agency <- "DOD_Navy"
status <- "coded"
coders <- c("Delaney")
DOD_Navy <- clean.agency() 
DOD_Navy %<>% left_join(members, by = c("last_name", "congress", "chamber"))

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
EPA %<>% left_join(members, by = c("last_name", "congress", "chamber", "state"))

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

# USDA 
agency <- "USDA"
status <- "not coded"
coders <- NA
USDA <- clean.agency()

agency <- "USDA_ERS"
status <- "not coded"
coders <- NA
USDA_ERS <- clean.agency() 
USDA_ERS %<>% left_join(members, by = c("congress", "chamber", "last_name"))

agency <- "USDA_FS"
status <- "not coded"
coders <- NA
USDA_FS <- clean.agency()
USDA_FS %<>% left_join(members, by = c("congress", "first_name", "last_name"))

agency <- "USDA_NASS"
status <- "coded"
coders <- c("Robert", "Henry")
USDA_NASS <- clean.agency()
USDA_NASS %<>% left_join(members, by = c("congress", "first_name", "last_name"))

agency <- "USDA_NRCS"
status <- "not coded"
coders <- NA
USDA_NRCS <- clean.agency()
USDA_NRCS %<>% left_join(members, by = c("congress", "first_name", "last_name")) # change to chamber when USDA_NRCS.R is updated

agency <- "USDA_RD"
status <- "not coded"
coders <- NA
USDA_RD <- clean.agency() 
USDA_NASS %<>% left_join(members, by = c("congress", "first_name", "last_name"))


# merge data
data <- plyr::join_all(list(
  DHS,
  DOD_DLA_Aviation,
  DOD_Navy,
  EPA,
  PRC, # incomplete, need to add sheets
  USDA,
  USDA_ERS,
  USDA_FS,
  USDA_NASS,
  USDA_NRCS,
  USDA_RD
  ), type = 'full')



###################
# summay analysis # TO BE MOVED TO ANOTHER FILE 
###################

# identify top members
mocs <- data %>% filter(!is.na(last_name), !is.na(chamber)) %>%
  group_by(last_name, chamber, agency) %>% tally() %>% ungroup() %>%
  group_by(agency, chamber) %>% top_n(2, n) %>% ungroup()

# plot by agency 
ggplot(data %>% filter(last_name %in% mocs$last_name, !is.na(year), !is.na(chamber)), 
       aes(x = factor(year), fill = last_name)) +
  geom_bar() +
  facet_grid(agency ~ chamber) + 
  labs(x = "", y = "", 
       title = paste("Letters from top 2 members of each chamber to each agency")) +
  theme(
    legend.title = element_blank(),
    panel.background = element_blank()
  ) 


# plot by nominate and agency
data %>% group_by(last_name, congress, nominate.dim1, chamber, agency) %>%
  tally() %>% ungroup() %>%
  filter(!is.na(chamber), !is.na(congress)) %>%
  ggplot() +
  geom_jitter(aes(x = congress, y = agency,  color = nominate.dim1, size = n),
              alpha = .3) +
  scale_colour_gradient2(low = "red", mid = "grey", high = "blue") +
  geom_text(
    data = data %>% group_by(last_name, congress, nominate.dim1, chamber, TYPE, agency) %>%
      tally() %>% ungroup() %>%
      filter(n > 50 & !is.na(chamber)),
    aes(x = congress, y = agency, label = last_name, size = n/4 ),
    position=position_jitter(width=0,height=.4)
  ) +
  scale_x_continuous(breaks = seq(107, 115, 1)) + 
  facet_grid(. ~ chamber)  +
  labs(y = "", 
       title = paste("Letters")) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank()
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
