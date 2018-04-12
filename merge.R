# This script combines clean log/letter files with other data sources.
source("setup.R")

agency <- "EPA" # the title of the R script for cleaning these data
status <- "coded" # c("coded", "recoded", "NA")
coders <- c("Adam", "Avery") # coder names that preface the agency name in the title of their google sheet
epa <- clean.agency() # adds a sheet of unresolved coder discrepencies to google drive

agency <- "DOD_Navy"
status <- "NA"
coders <- NA
dod.navy <- clean.agency()

agency <- "PRC"
status <- "NA"
coders <- NA
prc <- clean.agency()

# merge data
data <- plyr::join_all(list(
                  epa,
                  #prc, # script works, but not all data are in google sheet
                  dod.navy
                  ), type = 'full')

# now merge with voteview etc. ...


###################
# summay analysis # TO BE MOVED TO ANOTHER FILE 
###################

# identify top members
mocs <- data %>% filter(!is.na(last_name), !is.na(title)) %>%
  group_by(last_name, title, agency) %>% tally() %>% ungroup() %>%
  group_by(agency, title) %>% top_n(2, n) %>% ungroup()

ggplot(data %>% filter(last_name %in% mocs$last_name, !is.na(year)), aes(x = factor(year), fill = last_name)) +
  geom_bar() +
  facet_grid(title ~ agency) + 
  labs(x = "", y = "", 
       title = paste("Letters from top 2 members of each chamber to the", 
                     paste(unique(data$agency), collapse = ", "))) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    panel.background = element_blank()
  ) 



#####################################
# clean up workspace before commit #
#####################################
rm(list = ls(all = TRUE))
