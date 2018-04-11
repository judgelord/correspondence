# This script combines clean data files with other data sources.
source("setup.R")

agency <- "EPA" # the title of the R script for cleaning these data
status <- "coded" # c("coded", "recoded", "NA")
coders <- c("Adam", "Avery") # coder names that preface the agency name in the title of their google sheet
epa <-
  clean.agency() # adds a sheet of unresolved coder discrepencies to drive

agency <- "DOD_Navy"
status <- "NA"
coders <- NA
dod.navy <- clean.agency()

agency <- "PRC"
status <- "NA"
coders <- NA
prc <- clean.agency()

# merge data
data <- plyr::join_all(list(epa,
                  dod.navy,
                  prc
                  ), type = 'full')

# now merge with voteview etc. ...


###################
# summay analysis #
###################

# identify top members
mocs <- data %>% filter(!is.na(last_name)) %>%
  group_by(last_name, agency) %>% tally() %>% ungroup() %>%
  group_by(agency) %>% top_n(5, n) %>% ungroup()

ggplot(data %>% filter(last_name %in% mocs$last_name, !is.na(year)), aes(x = year, fill = last_name)) +
  geom_bar() +
  facet_grid(.~ agency) + 
  labs(x = "", y = "", 
       title = paste("Letters from top 5 Members of Congress to the", 
                     paste(unique(data$agency), collapse = ", "))) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    panel.background = element_blank()
  ) 



#####################################
# clean up workspace before committ #
#####################################
rm(list = ls(all = TRUE))
