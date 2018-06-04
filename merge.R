# This script combines clean log/letter files with other data sources.
# load functions
source("setup.R")
# clean.agency() # cleans data and adds a sheet of unresolved intercoder discrepencies to google drive

# Departments and agencies are listed A-Z
# Columns:
# 1 agency = the title of the R script for cleaning these data
# 2 status = c("coded", "recoded", "not coded"), NA if not yet coded
# 3 coders = coder names that proceed the agency name in the title of their google sheet, e.g. c("Adam", "Avery") for "EPA Adam" and "EPA Avery" sheets

data_list <- as.data.frame(matrix(c(
# Agency    # coded     # coders 
"DHHS_ACL", "not coded", NA,
"DHHS_CDC", "not coded", NA,
"DHHS_HRSA", "not coded", NA,
# DHS
"DHS", "coded", "Katie", # "Katie", "Megha") # but Megha's work is not there
"DHS_ICE", "not coded", NA,
# DOC
"DOC_IOS", "not coded", NA,
"DOC_SBA", "not coded", NA,
## "DOC_MBDA", "not coded", NA,
# DOD
"DOD_DeCA", "coded", "Devin",
"DOD_DFAS", "not coded", NA,
"DOD_DLA_Aviation", "not coded", NA,
"DOD_Navy", "coded", "Delaney",
# DOE
"DOE_FERC", "not coded", NA,
# DOI 
"DOI_BOEM", "not coded", NA,
"DOI_BSEE", "not coded", NA,
"DOI_NPS", "not coded", NA,
"DOI_USGS", "not coded", NA,
# DOJ 
"DOJ_CIV", "not coded", NA,
# DOL 
"DOL_EBSA", "not coded", NA,
"DOL_OCFO", "coded", "Devin",
"DOL_OFCCP", "not coded", NA,
"DOL_VETS", "not coded", NA,
# DOT 
"DOT_FAA", "coded", "Sam",
"DOT_FHWA", "not coded", NA,
"DOT_SLDC", "not coded", NA,
# Education
"ED", "not coded", NA,
# EPA
"EPA", "coded", "Adam", # c("Adam", "Avery"),
# FCC
"FCC", "coded", "Devin",
# PRC
"NASA", "not coded", NA,
"PRC", "not coded", NA,
# Treasury
"Treasury_OCC", "not coded", NA,
# USDA 
"USDA", "not coded", NA,
"USDA_ERS", "not coded", NA,
"USDA_FS", "not coded", NA,
"USDA_NASS", "coded", "Robert", # c("Robert", "Henry"),
"USDA_NRCS", "not coded", NA,
"USDA_RD", "not coded", NA,
"USDA_RMA", "not coded", NA,
# USPS
"USPS", "not coded", NA
), ncol = 3, byrow = T), col.names = c("agency", "status", "coders"))

names(data_list) <- c("agency", "status", "coders")

# merge data
i = 1
d <- clean.agency(agency = data_list[i, 1],
                     status = data_list[i, 2],
                     coders = data_list[i, 3])
d %<>% left_join(members)
d$ID %<>% as.character()

errors <- "Failed to merge:"

for (i in 2:nrow(data_list)) {
  print(data_list[i, 1])
  tryCatch({
    dt <- clean.agency(
      agency = data_list[i, 1],
      status = data_list[i, 2],
      coders = data_list[i, 3]) %>% 
    left_join(members)
    dt$ID %<>% as.character()
  d %<>% full_join(dt)
  length(unique(d$agency)) == i
  }, error = function(e) {errors <- paste(errors, data_list[i, 1], e)})
}
errors

d$department <- gsub("_.*", "", d$agency)

d %<>% 
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

problems <- d %>% group_by(agency, ID, FROM, first_name, last_name) %>% tally() %>% filter(n>1)


###################
# summay analysis # TO BE MOVED TO ANOTHER FILE 
###################

# identify top members
mocs <- d %>% 
  filter(!is.na(bioname), !is.na(chamber), bioname != "", chamber %in% c("House", "Senate"))  %>% 
  group_by(ID, agency, bioname) %>% mutate(n = n()) %>% filter(n == 1) %>% ungroup() %>%
  select(ID, bioname, TYPE, congress, year, chamber, agency, department, nominate.dim1)



# bin into percentiles of letter writers per agency and per dept
mocs %<>% 
  group_by(department, bioname, chamber) %>% 
  mutate(perDept = n()) %>% group_by(department, bioname, chamber, congress) %>%
  mutate(perDeptperCongress = n()) %>% group_by(department, bioname, chamber, year) %>%
  mutate(perDeptperYear = n()) %>% ungroup() %>%
  mutate(DeptPercentile = dplyr::ntile(perDept,100)) %>% 
  group_by(agency, bioname, chamber) %>%
  mutate(perAgency = n()) %>% group_by(agency, bioname, chamber, congress) %>%
  mutate(perAgencyperCongress = n()) %>% group_by(agency, bioname, chamber, year) %>%
  mutate(perAgencyperYear = n()) %>% ungroup() %>%
  mutate(AgencyPercentile = dplyr::ntile(perAgency,100)) 
  
mocs$name <- gsub(",.*", "", mocs$bioname)


##########################################################################################################################################################################################################################
# plot by nominate and dept
mocs %>%  group_by(congress, chamber, department, bioname, name, nominate.dim1) %>% tally() %>% ungroup() %>% 
  group_by(department) %>% mutate(percent = ntile(n, 10000)) %>%
  ggplot() +
  geom_text(
    aes(x = congress, 
        y = chamber, 
        label = paste0(name, "(", n,")"), 
        size = percent, 
        alpha = percent, 
        color = nominate.dim1),
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



# Name jitter plot by nominate and dept
mocs %>% 
  filter(!is.na(TYPE)) %>% 
  group_by(bioname, nominate.dim1, chamber, TYPE, name) %>% tally() %>% ungroup()  %>% 
  group_by(chamber, TYPE) %>% mutate(percentile = ntile(n, 100)) %>%
  ggplot() +
  geom_text(
    aes(x = TYPE, y = chamber, label = name, size = n, alpha = percentile, color = nominate.dim1),
    position=position_jitter()#width=0,height=.4)
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  #scale_x_continuous(breaks = seq(110, 115, 1), limits = c(110,115)) + 
  #facet_grid(department ~ .)  +
  labs(y = "", 
       title = paste("")) +
  theme(
    #axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    #legend.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) 




mocs %<>%
  group_by(chamber, agency) %>% 
  mutate(mean.agency = mean(n()), var.agency = var(n()), sd.agency = sd(n())) %>% ungroup() %>% 
  group_by(chamber, year) %>% 
  mutate(n.year = n()) %>%
  mutate(mean.year = mean( n() ) ) %>% 
  mutate(var.year = var(n) ) %>%  
  mutate(sd.year = sd(n) ) %>% ungroup() 









# member by year by agency 
chamb <- "Senate" # "Senate"
members.year.agency <- mocs %>% group_by(bioname, chamber, year, agency) %>% tally() %>%
  filter(chamber == chamb) %>%
  ggplot() +
  geom_point(
    aes(x = year, 
        y = bioname, #reorder(bioname, nominate.dim1), 
        #label = agency, 
        alpha = n,  
        color = agency), 
    position=position_jitter(width=.4,height=0)#,
    #alpha = .3
  ) +
  scale_x_continuous(breaks = seq(2007, 2018, 1), limits = c(2007,2018)) + 
  labs(title = paste(chamb),
       y = "Members by NOMINATE D1", 
       x = "" ) +
  theme(
    #axis.ticks = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45)
  ) 
members.year.agency

# mocs$TYPE[is.na(mocs$TYPE)] <- "to be coded"

members.year.agency.TYPE  <- mocs %>% group_by(bioname, chamber, year, agency, TYPE) %>% tally() %>%
  filter(chamber == chamb, TYPE != "0", TYPE != "6") %>%
  ggplot() +
  geom_point(
    aes(x = year, 
        y = bioname, #reorder(bioname, nominate.dim1), 
        label = agency, 
        alpha = n,  
        color = agency), 
    position=position_jitter(width=.4,height=0)
  ) +
  scale_x_continuous(breaks = seq(2007, 2018, 2), limits = c(2007,2018)) + 
  labs(title = paste(chamb),
       y = "Members by NOMINATE D1", 
       x = "" ) +
  theme(
    #axis.ticks = element_blank(),
    #legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45)
  ) + facet_grid(. ~ TYPE) 

members.year.agency.TYPE




# boxplots
mocs %>% group_by(bioname, year) %>% tally() %>% ungroup() %>%
  ggplot() + 
  geom_boxplot(
    aes(x = factor(year), y = n)) + 
  coord_flip() +
  labs(title = paste(chamb),  y = "Varience across agencies", 
                                                      x = "" ) +

mocs %>% group_by(bioname, agency) %>% tally() %>% ungroup() %>%
  ggplot() + 
  geom_boxplot(
    aes(x = factor(agency), y = n)) + coord_flip()






#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
