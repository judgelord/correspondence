# This script combines clean log/letter files with other data sources.
# agency = the title of the R script for cleaning these data
# status = c("coded", "recoded", "not coded"), NA if not yet coded
# coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets
# clean.agency() # cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# load functions
source("setup.R")


# the title of the R script for cleaning these data
# c("coded", "recoded", "not coded") NA if not yet coded
# coders c(NA, "one coder", or multiple coders c("Adam", "Avery")

# Departments and agencies are listed A-Z


data_list <- matrix(c(
  # Agency      # coded     # coders 
"DHHS_ACL", "not coded", NA,
"DHHS_CDC",    "not coded", NA,
"DHHS_HRSA", "not coded", NA,
# DHS
"DHS", "coded", "Katie", # "Katie", "Megha") # Megha's work is not there
"DHS_ICE", "not coded", NA,
# DOD
"DOD_DLA_Aviation", "not coded", NA,
"DOD_Navy", "coded", "Delaney",
"DOE_FERC", "not coded", NA,
"DOL_EBSA", "not coded", NA,
"DOL_OCFO", "coded", "Devin",
"DOL_OFCCP", "not coded", NA,
"DOL_VETS", "not coded", NA,
"DOT_FAA", "coded", "Sam",
#EPA
"EPA", "coded", "Adam", # c("Adam", "Avery"),
#FCC
"FCC", "coded", "Devin",
#PRC
"PRC", "not coded", NA,
"USDA", "not coded", NA,
"USDA_ERS", "not coded", NA,
"USDA_FS", "not coded", NA,
"USDA_NASS", "coded", "Robert", # c("Robert", "Henry"),
"USDA_NRCS", "not coded", NA,
"USDA_RD", "not coded", NA
), ncol = 3, nrow = 33, byrow = T)

# merge data
agency <- data_list[1,1]
status <- data_list[1,2]
coders <- data_list[1,3]
data <- clean.agency()

for (i in 2:nrow(data.list)) {
  agency <- data_list[i,1]
  status <- data_list[i,2]
  coders <- data_list[i,3]
  data %<>% full_join(clean.agency())
}

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
