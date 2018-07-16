# get the latest FOIA data 
if( !exists("d") ) { source("merge.R") } # this may take a while as it loads and cleans each sheet, incorperating any new coding
if( !exists("members") ) { source("setup.R") }
# load("correspondence20180710.RData")


d$TYPE[is.na(d$TYPE)] <- "To be coded"
d$TYPE[d$TYPE == 0] <- "To be coded"
d$TYPE[d$TYPE == 1] <- "Indiv. Constituent"
d$TYPE[d$TYPE == 2] <- "Corp. Constituent"
d$TYPE[d$TYPE == 3] <- "501c3 or Local Gov."
d$TYPE[d$TYPE == 4] <- "Corp. Policy"
d$TYPE[d$TYPE == 5] <- "Policy"
d$TYPE[d$TYPE == 6] <- "To be coded"


dcommittees <- d %>% left_join(committees)
dcommittees$assigneddate %<>% as.Date()
dcommittees$terminationdate %<>% as.Date()

dcommittees$party[dcommittees$party == 100] <- "(D)"
dcommittees$party[dcommittees$party == 200] <- "(R)"
dcommittees$party[dcommittees$party == 328] <- "(I)"



dcommittees %<>% 
  mutate(position = ifelse(10 < seniorstatus & seniorstatus < 17, "Chair", NA)) %>% 
  mutate(position = ifelse(20 < seniorstatus & seniorstatus < 24, "Ranking Minority", position))  %>% 
  mutate(position = ifelse(seniorstatus == 0 | seniorstatus > 24, NA, position)) %>%
  mutate(alpha = ifelse(TYPE %in% c("To be coded", "Indiv. Constituent"), .3, .7))

dcommittees %<>% 
  mutate(committeename = toupper(committeename))

# select unique observations matched in voteview
mocs <- d %>% 
  filter(!is.na(bioname), bioname != "", chamber %in% c("House", "Senate"))  %>% 
  group_by(ID, agency, bioname) %>%  
  #filter(n() == 1) %>% 
  ungroup() 

mocs %<>% mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n())

mocs %<>% mutate(cal.month = format(DATE, "%m(%b)"))

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
# plots #
#########

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

ggsave("namesbydept.pdf", width = 8.5, height = 11,  path = "~/correspondence/figs")


# Name jitter plot by TYPE 
mocs %>% 
  filter(TYPE != "To be coded") %>% 
  group_by(bioname, nominate.dim1, chamber, TYPE, last_name) %>% tally() %>% ungroup()  %>% 
  group_by(chamber, TYPE) %>% mutate(percentile = ntile(n, 100)) %>%
  ggplot() +
  geom_text(
    aes(x = TYPE, y = chamber, label = last_name, size = n, alpha = percentile, color = nominate.dim1),
    position=position_jitter()
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(y = "", 
       title = paste("")) +
  theme(
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) 
ggsave("namesbytype.pdf", width = 8.5, height = 11,  path = "~/correspondence/figs")






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
ggsave(paste("density_by_agencyrank_member.pdf"), width = 8.5, height = 11, path = "~/correspondence/figs")


# histogram of complete agnecies over time 
mocs %>% 
  filter(complete == T) %>% 
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_histogram(aes(x = DATE, fill = agency), alpha = 1, bins = 120)  +
  facet_grid(TYPE ~ .) 
ggsave(paste("letters_per_month_by_type_agency.pdf"), height = 8.5, width = 11,  path = "~/correspondence/figs")




mocs %>% 
  filter(complete == T) %>% 
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_line(aes(x = month, y = permonth, group = bioname, color = chamber), alpha = .2)  +
  facet_grid(TYPE ~ .)
ggsave(paste("letters_per_month_by_type_member.pdf"), height = 8.5, width = 11, path = "~/correspondence/figs")




# histogram of complete agnecies over time 
mocs %>% 
  ggplot() +
  labs(title = "Letters per month") +
  geom_bar(aes(x = cal.month, fill = agency), alpha = 1)  +
  facet_grid(TYPE ~ .) 
ggsave(paste("letters_per_calmonth_by_type_agency.pdf"), height = 11, width = 8.5, path = "~/correspondence/figs")












##############################################################
# plots by chamber # 
####################

chamb <- "Senate" # "House" # 
mocs$yaxis <- mocs$committeename
# member by year by agency 

mocs %>% # group_by(yaxis, chamber, year, agency) %>% tally() %>%
  filter(chamber == chamb, complete == T) %>%
  group_by(yaxis) %>%
  mutate(n = n()) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(yaxis, n), # sort by total number of lewters writen
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
ggsave(paste("members_by_year_agency", chamb, "n", ".pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")



mocs %>% # group_by(yaxis, chamber, year, agency, TYPE) %>% tally() %>%
  filter(chamber == chamb,  complete == T)  %>%
  group_by(yaxis) %>%
  mutate(n = n()) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(yaxis, n), 
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

ggsave(paste("members_by_year_agency_type", "n", chamb,".pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")


# not by year
mocs %>% # group_by(yaxis, chamber, year, agency, TYPE) %>% tally() %>%
  filter(chamber == chamb, seniorstatus >0)  %>%
  group_by(yaxis, agency) %>% tally() %>%
  #mutate(n = n()) %>% arrange(-n) %>% select(committeename, agency, n)
  ggplot() +
  geom_col(
    aes(x= yaxis, 
        y= n, 
        fill = agency)  ) +
  labs(title = paste(chamb), x = "") +
  theme(axis.text.x = element_text(size = 5),
        axis.ticks.x = element_blank()  ) + 
  coord_flip()
ggsave(paste("committeeseniors_by_agency", chamb,".pdf"), width = 11, height = 8.5,  path = "~/correspondence/figs")

# not by year, but by type
mocs %>% 
  filter(chamber == chamb,  TYPE != "To be coded", seniorstatus > 0)  %>%
  group_by(yaxis, agency, TYPE) %>% tally() %>%
  ggplot() +
  geom_col(
    aes(x= yaxis, 
        y= n, 
        fill = agency)  ) +
  labs(title = paste(chamb), x = "") +
  facet_grid(. ~ TYPE) +   
  theme(axis.text.x = element_text(size = 5),
    axis.ticks.x = element_blank()  ) + 
  coord_flip()
ggsave(paste("committeeseniors_by_agency_type", chamb,".pdf"), width = 11, height = 8.5,  path = "~/correspondence/figs")



# boxplots by year 
mocs %>% group_by(yaxis, year, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(yaxis, n), y = n, color = nominate.dim1), outlier.shape = NA) + 
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per Year,", chamb)) + 
  theme(axis.text.y = element_text(size=5))

ggsave(paste("boxplot_by_year", chamb,".pdf"), width = 8.5, height = 11, path = "~/correspondence/figs")

# boxplots by agency 
mocs %>% 
  group_by(agency) %>% filter(n() > 1000) %>%
  group_by(yaxis, agency, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == chamb) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(yaxis, n), y = n, color = nominate.dim1), outlier.shape = NA) + 
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per Agency,", chamb)) + 
  theme(axis.text.y = element_text(size=5))

ggsave(paste("boxplot_by_agency", chamb,".pdf"), width = 8.5, height = 11, path = "~/correspondence/figs")




# Comittee Chairs

chairs <- filter(dcommittees, !is.na(bioname), bioname != "", chamber %in% c("House", "Senate"))
chairs %<>% mutate(member_committee = paste(bioname, committeename)) 
# chairs %<>% filter(member_committee %in% c(unique(chairs$member_committee[which(chairs$position == "Chair")]))) 
chairs %<>% mutate(assignedyear = ifelse(position == "Chair", as.numeric(substring(assigneddate, 1, 4)), 9999))
chairs %<>% group_by(member_committee) %<>% mutate(firstassigned = min(assignedyear, na.rm = TRUE)) %>% ungroup()
chairs %<>% mutate(chair = paste(firstassigned,  last_name, party))
chairs %<>% mutate(member_party = paste(congress, last_name, party)) %>%
  mutate(member_term = paste(congress, bioname)) %>%
  mutate(committee = gsub(" AND .*|, .*|\\(.*", "", committeename))

chairs %>% 
  filter(chamber == chamb, complete == T, agency != "Amtrak", agency != "PRC", DATE < as.Date("2017-01-01")) %>%
  group_by(chair) %>%
  mutate(n = n()) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = chair, 
        color = department),
    shape = 73,
    size=2
  ) +
  geom_segment(aes(y = chair, yend = chair, 
                   x = assigneddate, xend = terminationdate, 
                   linetype = factor(position)),
               position = position_nudge(y = -0.3)) +
  labs(title = paste(chamb, "Committee Chairs"),
       y = "", 
       x = "" ) +
  scale_y_discrete(position = "right") +
  theme(
    strip.text.y = element_text(angle = 180, size = 5),
    legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 0)
  ) + 
  facet_grid(committee ~ ., scales = "free_y", space = "free_y", switch = "both") 

ggsave(paste("committeechairs_by_year_agency", chamb, ".pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")

install.packages("lemon")
library(lemon)

# not by year 
chairs %>% 
  filter(chamber == chamb, agency != "Amtrak", agency != "PRC", DATE < as.Date("2017-01-01"))  %>%
  group_by(department, committee, member_term, position) %>% tally() %>%
  ggplot() +
  geom_boxplot(
    aes(y = n, 
        # fill = position,
        x = department)) +
  geom_point(aes(y = n, 
                     color = factor(position),
                     x = department), na.rm = TRUE
             ) + 
  scale_color_discrete(na.translate = FALSE) +
  labs(title = paste("Letters per term from Each", chamb, "Committee Member"),
       x = "", 
       y = "Number of Contacts per Congress from Each Committee Member 2008-2016" ) +
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5)) + 
  facet_rep_grid(committee ~ ., scales = "free_y", space = "free_y", repeat.tick.labels = 'bottom') 

ggsave(paste("committees_by_agency", chamb,".pdf"), width = 8.5, height = 22,  path = "~/correspondence/figs")


# Variance across all members 
all <- d %>% group_by(agency, bioname) %>% tally() 
var(all$n)
# Variance across all chairs within committee
all <- d %>% group_by(agency, bioname) %>% tally() 
var(all$n)
# Variance across committees

# Within-member variance between chairs and non-chair

#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
