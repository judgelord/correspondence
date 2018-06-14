# get the latest FOIA data 
if( !exists("d") ) { source("merge.R") } # this may take a while as it loads and cleans each sheet, incorperating any new coding


# select unique observations matched in voteview
mocs <- d %>% 
  filter(!is.na(bioname), bioname != "", chamber %in% c("House", "Senate"))  %>% 
  group_by(ID, agency, bioname) %>%  filter(n() == 1) %>% ungroup() 


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


# Name jitter plot by congress and dept
mocs %>%  filter(complete == T) %>% 
  group_by(congress, chamber, department, bioname, last_name, nominate.dim1) %>% tally() %>% ungroup() %>% 
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
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

ggsave("namesbydept.pdf", width = 8.5, height = 11)


# Name jitter plot by TYPE 
mocs %>% 
  filter(!is.na(TYPE), TYPE != "0", TYPE != "6") %>% 
  group_by(bioname, nominate.dim1, chamber, TYPE, last_name) %>% tally() %>% ungroup()  %>% 
  group_by(chamber, TYPE) %>% mutate(percentile = ntile(n, 100)) %>%
  ggplot() +
  geom_text(
    aes(x = TYPE, y = chamber, label = last_name, size = n, alpha = percentile, color = nominate.dim1),
    position=position_jitter()#width=0,height=.4)
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(y = "", 
       title = paste("")) +
  theme(
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) 
ggsave("namesbytype.pdf", width = 8.5, height = 11)





chamb <- "Senate" # "Senate"

# member by year by agency 

mocs %>% # group_by(bioname, chamber, year, agency) %>% tally() %>%
  filter(chamber == chamb, complete == T) %>%
  group_by(bioname) %>%
  mutate(n = n()) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(bioname, n), # sort by total number of lewters writen
        color = agency), 
    alpha = .3
  ) +
  labs(title = paste(chamb),
       y = paste("Members by", "n"), 
       x = "" ) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45)
  ) 
ggsave(paste("members_by_year_agency", chamb, "n", ".pdf"), width = 8.5, height = 11)


mocs$TYPE[is.na(mocs$TYPE)] <- "to be coded"

members.year.agency.TYPE  <- mocs %>% # group_by(bioname, chamber, year, agency, TYPE) %>% tally() %>%
  filter(chamber == chamb, TYPE != "0", TYPE != "6")  %>%
  group_by(bioname) %>%
  mutate(n = n()) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(bioname, n), 
        label = agency, 
        color = agency), 
    alpha = .2
  ) +
  labs(title = paste(chamb),
       y = paste("Members by", "n"), 
       x = "" ) +
  theme(
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45)
  ) + facet_grid(. ~ TYPE) 

ggsave(paste("members_by_year_agency_type", "n", chamb,".pdf"), width = 8.5, height = 11)



# boxplots by year 
mocs %>% group_by(bioname, year, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(bioname, n), y = n, color = nominate.dim1), outlier.shape = NA) + 
  #abline(mean) +
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per Year,", chamb)) + 
  theme(axis.text.y = element_text(size=5))
ggsave(paste("boxplot_by_year", chamb,".pdf"), width = 8.5, height = 11)

# boxplots by agency 
mocs %>% 
  group_by(agency) %>% filter(n() > 1000) %>%
  group_by(bioname, agency, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(bioname, n), y = n, color = nominate.dim1), outlier.shape = NA) + 
  #abline(mean) +
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per Agency,", chamb)) + 
  theme(axis.text.y = element_text(size=5))

ggsave(paste("boxplot_by_agency", chamb,".pdf"), width = 8.5, height = 11)



# DENSITY
# distribution over agencies ranked

# density over all members
mocs %>% 
  group_by(bioname, agency, chamber) %>% 
  mutate(permemberperagency = n()) %>% ungroup() %>% 
  group_by(bioname, chamber) %>% 
  mutate(agency.rank = dense_rank(-permemberperagency)) %>% 
  mutate(permember = sum(permemberperagency)) %>%  ungroup() %>% group_by(chamber) %>% 
  mutate(member.rank = min_rank(permember)) %>%
  mutate(member.quartile = ntile(member.rank, 4)) %>% 
  ggplot() +
  geom_bar(aes(x= agency.rank)) +# , fill = factor(member.quartile)))+ 
  facet_grid(. ~ chamber) 

ggsave(paste("density_by_agencyrank.pdf"), width = 8.5, height = 11)

# line plot of density over agencies by member
mocs %>% 
  group_by(bioname, department, chamber) %>% tally () %>% ungroup() %>% group_by(bioname) %>%
  mutate(agency.rank = dense_rank(-n)) %>%
  ggplot() +
  geom_line(aes(x= agency.rank, y= n, group = bioname), alpha = .2) + 
  labs(title = "Letters per agency ranked") +
  #geom_point(aes(x= agency.rank, y= n, color = department)) + 
  facet_grid(. ~ chamber)
ggsave(paste("density_by_agencyrank_member.pdf"), width = 8.5, height = 11)


# histogram of complete agnecies over time 
mocs %>% 
  filter(complete == T) %>% 
  filter(TYPE %in% c(1,2,3, 4,5, "to be coded")) %>%
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_histogram(aes(x = DATE, fill = agency), alpha = 1, bins = 120)  +
  facet_grid(TYPE ~ .) 
ggsave(paste("letters_per_month_by_type_agency.pdf"), height = 8.5, width = 11)


mocs %<>% mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n())

mocs %>% 
  filter(complete == T) %>% 
  filter(TYPE %in% c(1,2,3, 4,5, "to be coded")) %>%
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_line(aes(x = month, y = permonth, group = bioname, color = chamber), alpha = .2)  +
  facet_grid(TYPE ~ .)
ggsave(paste("letters_per_month_by_type_member.pdf"), height = 8.5, width = 11)

mocs %<>% mutate(cal.month = substr(DATE, 6,7))


# histogram of complete agnecies over time 
mocs %>% 
  filter(TYPE %in% c(1,2,3, 4,5, "to be coded")) %>%
  ggplot() +
  labs(title = "Letters per month") +
  geom_bar(aes(x = cal.month, fill = agency), alpha = 1)  +
  facet_grid(TYPE ~ .) 
ggsave(paste("letters_per_calmonth_by_type_agency.pdf"), height = 8.5, width = 11)



#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
