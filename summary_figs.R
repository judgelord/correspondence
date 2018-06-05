
# get the latest FOIA data 
source(merge.R) # this may take a while as it loads and cleans each sheet, incorperating any new coding


# select unique observations matched in voteview
mocs <- d %>% 
  filter(!is.na(bioname), bioname != "", chamber %in% c("House", "Senate"))  %>% 
  group_by(ID, agency, bioname) %>% mutate(n = n()) %>% filter(n == 1) %>% ungroup() 


# bin percentiles of letter writers per agency and per dept
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
  mutate(AgencyPercentile = dplyr::ntile(perAgency,100)) %>%
  group_by(chamber, agency)


##########################################################################################################################################################################################################################
# plot by nominate and dept
mocs %>%  group_by(congress, chamber, department, bioname, last_name, nominate.dim1) %>% tally() %>% ungroup() %>% 
  group_by(department) %>% mutate(percent = ntile(n, 100)) %>%
  ggplot() +
  geom_text(
    aes(x = congress, 
        y = chamber, 
        label = paste0(last_name, "(", n,")"), 
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
  group_by(bioname, nominate.dim1, chamber, TYPE, last_name) %>% tally() %>% ungroup()  %>% 
  group_by(chamber, TYPE) %>% mutate(percentile = ntile(n, 100)) %>%
  ggplot() +
  geom_text(
    aes(x = TYPE, y = chamber, label = last_name, size = n, alpha = percentile, color = nominate.dim1),
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







# member by year by agency 
chamb <- "House" # "Senate"
members.year.agency <- mocs %>% # group_by(bioname, chamber, year, agency) %>% tally() %>%
  filter(chamber == chamb) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = bioname, #reorder(bioname, nominate.dim1), 
        #label = agency, 
        #alpha = n,  
        color = agency), 
    #position=position_jitter(width=.4,height=0),
    alpha = .2
  ) +
  #scale_x_continuous(breaks = seq(2007, 2018, 1), limits = c(2007,2018)) + 
  labs(title = paste(chamb),
       y = "Members by NOMINATE D1", 
       x = "" ) +
  theme(
    #axis.ticks = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45, color = "blue")#scale_colour_gradient2(low = "blue", mid = "grey", high = "red"))
  ) 
members.year.agency

mocs$TYPE[is.na(mocs$TYPE)] <- "to be coded"

members.year.agency.TYPE  <- mocs %>% # group_by(bioname, chamber, year, agency, TYPE) %>% tally() %>%
  filter(chamber == chamb, TYPE != "0", TYPE != "6") %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(bioname, nominate.dim1), 
        label = agency, 
        #alpha = n,  
        color = agency), 
    alpha = .2
    #position=position_jitter(width=.4,height=0)
  ) +
  #scale_x_continuous(breaks = seq(2007, 2018, 2), limits = c(2007,2018)) + 
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



chamb <- "Senate"
# boxplots
mocs %>% group_by(bioname, year, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(bioname, n), y = n, color = nominate.dim1)) + 
  #abline(mean) +
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per year,", chamb)) + 
  theme(axis.text.y = element_text(size=5))

chamb <- "Senate"
# boxplots
mocs %>% 
  group_by(agency) %>% filter(n() > 1000) %>%
  group_by(bioname, agency, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(bioname, n), y = n, color = nominate.dim1)) + 
  #abline(mean) +
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per agency,", chamb)) + 
  theme(axis.text.y = element_text(size=5))

# DENSITY
# distribution over agencies ranked
# bar plot of all members
mocs %>% 
  group_by(bioname, agency, chamber) %>% tally () %>% ungroup() %>% 
  group_by(bioname, chamber) %>% 
  mutate(agency.rank = dense_rank(-n)) %>% ungroup() %>% 
  mutate(member.quartile = ntile(n, 4)) %>% 
  ggplot() +
  geom_density(aes(x= agency.rank, fill = factor(member.quartile)), color = NA, alpha = .3) + 
  facet_grid(. ~ chamber) 

# line plot of members 
mocs %>% 
  group_by(bioname, department, chamber) %>% tally () %>% ungroup() %>% group_by(bioname) %>%
  mutate(agency.rank = dense_rank(-n)) %>%
  ggplot() +
  geom_line(aes(x= agency.rank, y= n, group = bioname), alpha = .2) + 
  geom_point(aes(x= agency.rank, y= n, color = department)) + 
  facet_grid(. ~ chamber)

# bar plot of complete agnecies over time 
mocs %>% 
  #filter(!(agency %in% c("USPS", "DHS", "DHS_ICE", "DHHS_CDC", "DOI_NPS", "DOI_BOEM", "DOD_DFAS", "DOD_OSDJS", "DOD_Navy"))) %>%
  filter(agency %in% c("DOL_EBSA", "DOD_DeCA", "DOL_OFCCP", "DOL_VETS", "EPA", "ED", " PRC", "USDA")) %>%
  #filter(TYPE != "6", TYPE != "0") %>%
  filter(TYPE %in% c(2,4)) %>%
  ggplot() +
  geom_bar(aes(x = year, fill = agency)) + facet_grid(TYPE ~ .)





#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
