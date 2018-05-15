# This script combines clean log/letter files with other data sources.
source("setup.R")

# INDEPENDENT AGENCIES 
agency <- "EPA" # the title of the R script for cleaning these data
status <- "coded" # c("coded", "recoded", "NA")
coders <- c("Adam", "Avery") # coder names that preface the agency name in the title of their google sheet
epa <- clean.agency() # adds a sheet of unresolved coder discrepencies to google drive

agency <- "PRC"
status <- "NA"
coders <- NA
prc <- clean.agency()

# DOD 
agency <- "DOD_Navy"
status <- "NA"
coders <- NA
dod.navy <- clean.agency()

agency <- "DOD_Aviation"
status <- "NA"
coders <- NA
dod.navy <- clean.agency()

# DOT 
agency <- "DOT_FAA"
status <- "NA"
coders <- NA
USDA <- clean.agency()

# DOT 
agency <- "FAA"
status <- "NA"
coders <- NA
USDA <- clean.agency()

# USDA 
agency <- "USDA"
status <- "NA"
coders <- NA
USDA <- clean.agency()

agency <- "USDA_Forest Service"
status <- "NA"
coders <- NA
USDA_ForestService <- clean.agency()

agency <- "USDA_RD"
status <- "NA"
coders <- NA
USDA <- clean.agency()

agency <- "USDA_ERS"
status <- "NA"
coders <- NA
USDA <- clean.agency()

agency <- "USDA_NASS"
status <- "NA"
coders <- NA
USDA <- clean.agency()

agency <- "USDA_NRCS"
status <- "NA"
coders <- NA
USDA <- clean.agency()

# merge data
data <- plyr::join_all(list(
                  epa,
                  #prc, # script works, but not all PRC data are in google sheet
                  dod.navy
                  ), type = 'full')

# merge with voteview etc. ... TO BE IMPROVED
for(i in length(members)){
  data %<>% mutate(first_name = 
                     ifelse(
                       is.na(first_name) & last_name==members$last_name[i] & state == members$state[i],
                       members$first_name,
                       first_name))
}

data %<>% left_join(members, by = "last_name")






###################
# summay analysis # TO BE MOVED TO ANOTHER FILE 
###################

# identify top members
mocs <- data %>% filter(!is.na(last_name), !is.na(title)) %>%
  group_by(last_name, title, agency) %>% tally() %>% ungroup() %>%
  group_by(agency, title) %>% top_n(2, n) %>% ungroup()

# plot by agency 
ggplot(data %>% filter(last_name %in% mocs$last_name, !is.na(year)), aes(x = factor(year), fill = last_name)) +
  geom_bar() +
  facet_grid(agency ~ chamber) + 
  labs(x = "", y = "", 
       title = paste("Letters from top 2 members of each chamber to the", 
                     paste(unique(data$agency), collapse = ", "))) +
  theme(
    legend.title = element_blank(),
    panel.background = element_blank()
  ) 



# plot by nominate and TYPE
data %>% group_by(last_name, congress.x, nominate.dim1, chamber, TYPE, agency) %>%
  tally() %>% ungroup() %>%
  filter(agency == "EPA" & TYPE != 0 & TYPE != 6 & !is.na(chamber)) %>%
  ggplot() +
  geom_jitter(aes(x = congress.x, y = agency,  color = nominate.dim1, size = n),
              alpha = .5) +
  scale_colour_gradient2(low = "red", mid = "grey", high = "blue") +
  geom_text(
    data = data %>% group_by(last_name, congress.x, nominate.dim1, chamber, TYPE, agency) %>%
      tally() %>% ungroup() %>%
      filter(n > 50 & agency == "EPA" & TYPE != 0 & TYPE != 6 & !is.na(chamber)),
    aes(x = congress.x, y = agency, label = last_name, size = n/4 ),
    position=position_jitter(width=0,height=.4)
  ) +
  facet_grid(TYPE ~ chamber)  


#####################################
# clean up workspace before commit #
#####################################
rm(list = ls(all = TRUE))
