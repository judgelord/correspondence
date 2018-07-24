options(stringsAsFactors = FALSE)

requires <- c("dplyr", "ggplot2", "magrittr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
library(dplyr) 
library(ggplot2)
library(magrittr)

# get latest data 
if( !exists("d") ) { source("merge.R") } # this may take a while as it loads and cleans each sheet, incorperating any new coding
if( !exists("committees") ) { source("setup.R") }
# or load an archived data file:
# load("correspondence 2018-07-24 .RData")

df <- filter(d, !is.na(icpsr)) # select only voteview-matched observations

# numeric to text 
df$TYPE[is.na(df$TYPE)] <- "To be coded"
df$TYPE[df$TYPE == 0] <- "To be coded"
df$TYPE[df$TYPE == 1] <- "Indiv. Constituent"
df$TYPE[df$TYPE == 2] <- "Corp. Constituent"
df$TYPE[df$TYPE == 3] <- "501c3 or Local Gov."
df$TYPE[df$TYPE == 4] <- "Corp. Policy"
df$TYPE[df$TYPE == 5] <- "Policy"
df$TYPE[df$TYPE == 6] <- "To be coded"

df %<>% 
  mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n()) %>% ungroup() %>% 
  mutate(cal.month = format(DATE, "%m(%b)"))

# bin percentiles of letter writers per agency and per dept
df %<>% 
  group_by(department, bioname, chamber) %>% 
  mutate(perDept = n()) %>% group_by(department, bioname, chamber, congress) %>%
  mutate(perDeptperCongress = n()) %>% group_by(department, bioname, chamber, year) %>%
  mutate(perDeptperYear = n()) %>% ungroup() %>%
  mutate(DeptPercentile = dplyr::ntile(perDept,100)) %>% 
  group_by(agency, bioname, chamber) %>%
  mutate(perAgency = n()) %>% group_by(agency, bioname, chamber, congress) %>%
  mutate(perAgencyperCongress = n()) %>% group_by(agency, bioname, chamber, year) %>%
  mutate(perAgencyperYear = n()) %>% ungroup() %>%
  mutate(AgencyPercentile = dplyr::ntile(perAgency, 100)) 


# re-merge committee data to one obs per letter per committee
dcommittees <- df %>% left_join(committees)
dcommittees$assigneddate %<>% as.Date()
dcommittees$terminationdate %<>% as.Date()

# to text
dcommittees$party[dcommittees$party_code == 100] <- "(D)"
dcommittees$party[dcommittees$party_code == 200] <- "(R)"
dcommittees$party[dcommittees$party_code == 328] <- "(I)"

dcommittees %<>% 
  mutate(position = ifelse(10 < seniorstatus & seniorstatus < 17, "Chair", NA)) %>% 
  mutate(position = ifelse(20 < seniorstatus & seniorstatus < 24, "Ranking Minority", position))  %>% 
  mutate(position = ifelse(seniorstatus == 0 | seniorstatus > 24, NA, position)) %>%
  mutate(alpha = ifelse(TYPE %in% c("To be coded", "Indiv. Constituent"), .3, .7))

dcommittees %<>% 
  mutate(committeename = toupper(committeename)) # combine upper and lower case stewart committee names

# short committee name
dcommittees %<>% mutate(committee = gsub(" AND .*|, .*|\\(.*", "", committeename))
# year first assigned to a committee
dcommittees %<>% mutate(member_committee = paste(bioname, committee)) 
dcommittees %<>% group_by(member_committee) %<>% 
  mutate(firstassigneddate = min(assigneddate, na.rm = TRUE)) %>% ungroup()
dcommittees %<>% mutate(firstassigned = as.numeric(substring(firstassigneddate, 1, 4)))
# assigned chair
dcommittees %<>% mutate(assignedchairdate = as.Date(assigneddate))
dcommittees$assignedchairdate[dcommittees$position != "Chair"] <- NA
dcommittees$assignedchairdate[is.na(dcommittees$position)] <- NA
dcommittees %<>% group_by(member_committee) %>% 
  mutate(firstassignedchairdate = min(assignedchairdate, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(firstassignedchair = as.numeric(substring(firstassignedchairdate, 1, 4)))
dcommittees %<>% 
  mutate(chair = paste(firstassignedchair,  bioname, party)) %>%
  mutate(member_party = paste(bioname, party)) 



# Select only Comittee Chairs
chairs <- filter(dcommittees, member_committee %in% c(unique(dcommittees$member_committee[which(dcommittees$position == "Chair")]))) 









##########################################################################################################################################################################################################################
# plots #
#########

# inspect data completeness coding
df %>% 
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>%
  mutate(coded = ifelse(TYPE == "To be coded", F, T)) %>% 
  ggplot() + 
  geom_point(aes(x = DATE, y = paste(n, agency), color = complete, alpha = coded))
# CDC is rolling release 
# SBA has no records before 2010
# DOJ_CIV is a rolling release - 2009-2011 recieved in July 2018
# CBP waiting since april 2017
# USDA RMA has no logs prior to 2010
# 


df %>% 
  mutate(coded = ifelse(TYPE == "To be coded", F, T)) %>% 
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>%
  ggplot() + 
  geom_point(aes(x = DATE, y = reorder(agency, n), color = TYPE, alpha = coded))




# Name jitter plot by congress and dept
df %>%  filter(complete == T) %>% 
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
df %>% 
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
df %>% 
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
df %>% 
  group_by(bioname, department, chamber) %>% tally () %>% ungroup() %>% group_by(bioname) %>%
  mutate(agency.rank = dense_rank(-n)) %>%
  ggplot() +
  geom_line(aes(x= agency.rank, y= n, group = bioname), alpha = .2) + 
  labs(title = "Letters per agency ranked") +
  #geom_point(aes(x= agency.rank, y= n, color = department)) + 
  facet_grid(. ~ chamber)
ggsave(paste("density_by_agencyrank_member.pdf"), width = 8.5, height = 11, path = "~/correspondence/figs")


# histogram of complete agnecies over time 
df %>% 
  filter(complete == T) %>% 
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_histogram(aes(x = DATE, fill = agency), alpha = 1, bins = 120)  +
  facet_grid(TYPE ~ ., scales = "free_y") 
ggsave(paste("letters_per_month_by_type_agency.pdf"), height = 8.5, width = 11,  path = "~/correspondence/figs")




df %>% 
  filter(complete == T) %>% 
  ggplot() +
  labs(title = "Letters per month for agencies with complete data") +
  geom_line(aes(x = month, y = permonth, group = bioname, color = chamber), alpha = .2)  +
  facet_grid(TYPE ~ ., scales = "free_y", space = "free_y")
ggsave(paste("letters_per_month_by_type_member.pdf"), height = 8.5, width = 11, path = "~/correspondence/figs")




# histogram of complete agnecies over time 
df %>% 
  ggplot() +
  labs(title = "Letters per month") +
  geom_bar(aes(x = cal.month, fill = agency), alpha = 1)  +
  facet_grid(TYPE ~ .) 
ggsave(paste("letters_per_calmonth_by_type_agency.pdf"), height = 11, width = 8.5, path = "~/correspondence/figs")












##############################################################
# plots by chamber # 
####################

chamb <- "Senate" # "House" # 

# define y axis
df$yaxis <- df$bioname

# member by year by agency 
df %>% # group_by(yaxis, chamber, year, agency) %>% tally() %>%
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



df %>% # group_by(yaxis, chamber, year, agency, TYPE) %>% tally() %>%
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

ggsave(paste("members_by_year_agency_type", chamb,".pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")






# boxplots by year 
df %>% group_by(yaxis, year, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
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
df %>% 
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





chairs %>% 
  filter(chamber == chamb, complete == T, agency != "Amtrak", agency != "PRC", DATE < as.Date("2017-01-01")) %>%
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

# working on faceting
# install.packages("lemon")
# library(lemon)

# not by year 
chairs %>% 
  filter(chamber == chamb, agency != "Amtrak", agency != "PRC", DATE < as.Date("2017-01-01"))  %>%
  group_by(department, committee, member_party, position) %>% tally() %>%
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
  facet_grid(committee ~ ., scales = "free_y", space = "free_y")#, repeat.tick.labels = 'bottom') 

ggsave(paste("committees_by_agency", chamb,".pdf"), width = 8.5, height = 22,  path = "~/correspondence/figs")


# define matchable committees and depts
depts <- c("EPA", "USDA", "DOT", "ED")
comms <- c("ENVIRONMENT", "SCIENCE", "AGRICULTURE", "TRANSPORTATION", "EDUCATION", "HEALTH")
# committee effect for select dwepts 
chairs %>% 
  filter(department %in% depts & committee %in% comms & chamber == chamb) %>%
  group_by(chamber, member_committee, department, committee, position) %>%  tally() %>% ungroup() %>%
  ggplot() + 
  geom_text(aes(x = position, y = n, label = gsub(",.*", "", member_committee), alpha = n), size = 2) + 
  geom_boxplot(aes(x = position, y = n), outlier.shape = NA)  + 
  facet_grid(committee ~ department,  scales = "free_y") + 
  labs(title = chamb) + 
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("committee effect for chairs", chamb, ".pdf", collapse = ""), width = 11, height = 8.5,  path = "~/correspondence/figs")


depts <- c("DHS")
comms <- c("HOMELAND SECURITY")
# committee effect for select dwepts 
chairs %>% 
  filter(department %in% depts & committee %in% comms & chamber == chamb) %>%
  group_by(chamber, member_committee, department, committee, position) %>%  tally() %>% ungroup() %>%
  ggplot() + 
  geom_text(aes(x = position, y = n, label = gsub(",.*", "", member_committee), alpha = n), size = 2) + 
  geom_boxplot(aes(x = position, y = n), outlier.shape = NA)  + 
  facet_grid(committee ~ department,  scales = "free_y") + 
  labs(title = chamb) + 
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("committee effect for chairs", chamb, "DHS.pdf", collapse = ""), width = 11, height = 8.5,  path = "~/correspondence/figs")


tenure <- chairs %>%
  mutate(tenure = ifelse(year == firstassignedchair, "Year After", NA)) %>% 
  mutate(tenure = ifelse(year == firstassignedchair-1, "Year  Before", tenure)) %>% 
  group_by(member_committee, tenure, chamber) %>% tally() %>% ungroup() %>% 
  filter(!is.na(tenure)) 

mean(tenure$n[tenure$tenure == "Year  Before"])
mean(tenure$n[tenure$tenure == "Year After"])

tenure %>% 
  ggplot() + 
  geom_text(aes(x = tenure, y = n, label = member_committee), size  = 2) + 
  geom_boxplot(aes(x = tenure, y = n)) + 
  facet_grid(chamber ~ .,  scales = "free_y")

ggsave(paste("chair effect all.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")

# define matchable committees and depts
depts <- c("DHS", "EPA", "USDA", "DOT", "ED")
comms <- c("HOMELAND SECURITY", "SCIENCE", "ENVIRONMENT", "AGRICULTURE", "TRANSPORTATION", "EDUCATION")

tenure <- chairs %>%
  mutate(tenure = ifelse(year == firstassignedchair, "Year After", NA)) %>% 
  mutate(tenure = ifelse(year == firstassignedchair-1, "Year  Before", tenure)) %>% 
  group_by(member_committee, tenure, year ,committee, department) %>% 
  tally() %>% ungroup() %>% 
  filter(!is.na(tenure) & department %in% depts & committee %in% comms)


tenure %>% 
  ggplot() + 
  geom_text(aes(x = tenure, y = n, label = gsub(",.*", "", member_committee)), size = 2) + 
  geom_boxplot(aes(x = tenure, y = n))  + 
  facet_grid(committee ~ department,  scales = "free_y") + 
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("chair effect selected pairs.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")




# years before and after 
tenure <- chairs %>%
  mutate(tenure = ifelse(year >= firstassignedchair, "Years After", NA)) %>% 
  mutate(tenure = ifelse(year < firstassignedchair, "Years  Before", tenure)) %>% 
  group_by(member_committee, tenure, year) %>% 
  tally() %>% ungroup() %>% 
  filter(!is.na(tenure))

mean(tenure$n[tenure$tenure == "Years  Before"])
mean(tenure$n[tenure$tenure == "Years After"])

# define matchable committees and depts
depts <- c("DHS", "EPA", "USDA", "DOT", "ED")
comms <- c("HOMELAND SECURITY", "SCIENCE", "ENVIRONMENT", "AGRICULTURE", "TRANSPORTATION", "EDUCATION")

tenure <- chairs %>%
  mutate(tenure = ifelse(year >= firstassignedchair, "Years After", NA)) %>% 
  mutate(tenure = ifelse(year < firstassignedchair, "Years  Before", tenure)) %>% 
  group_by(member_committee, tenure, year ,committee, department) %>% 
  tally() %>% ungroup() %>% 
  filter(!is.na(tenure) & department %in% depts & committee %in% comms)


tenure %>% 
  ggplot() + 
  geom_text(aes(x = tenure, y = n, label = gsub(",.*", "", member_committee)), size = 2) + 
  geom_boxplot(aes(x = tenure, y = n))  + 
  facet_grid(committee ~ department) + 
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))
  
ggsave(paste("chair effect selected pairs.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")


tenure <- chairs %>%
  mutate(tenure = ifelse(year >= firstassignedchair, "Years After", NA)) %>% 
  mutate(tenure = ifelse(year < firstassignedchair, "Years  Before", tenure)) %>% 
  group_by(member_committee, tenure, year ,committee, department, TYPE) %>% 
  tally() %>% ungroup() %>% 
  filter(!is.na(tenure) & department %in% depts & committee %in% comms, TYPE == "Policy")

mean(tenure$n[tenure$tenure == "Years  Before"])
mean(tenure$n[tenure$tenure == "Years After"])

tenure %>% 
  ggplot() + 
  geom_text(aes(x = tenure, y = n, label = gsub(",.*", "", member_committee)), size = 2) + 
  geom_boxplot(aes(x = tenure, y = n))  + 
  facet_grid(committee ~ department) + 
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("chair effect selected pairs", chamb," (policy only).pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")





# years before and after 
tenure <- dcommittees %>%
  mutate(TYPE = ifelse(TYPE == "To be coded", NA, TYPE)) %>%
  mutate(TYPE = ifelse(TYPE == "Corp. Policy", "Policy", TYPE)) %>%
  mutate(TYPE = ifelse(TYPE %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", TYPE)) %>%
  mutate(member_committee_TYPE = paste(member_committee, TYPE)) 

tenure %<>%
  mutate(tenure = year - firstassignedchair) %>% # firstassigned or firstassignedchair (firstassigned is mostly the first year of the data)
  group_by(member_committee, tenure, TYPE) %>% 
  mutate(n = n()) %>% ungroup() %>% 
  filter(!is.na(tenure))

mean(tenure$year[tenure$tenure == 0])

mean(tenure$n[tenure$tenure == -2])
mean(tenure$n[tenure$tenure == -1])
mean(tenure$n[tenure$tenure == 0])
mean(tenure$n[tenure$tenure == 1])
mean(tenure$n[tenure$tenure == 2])

# define matchable committees and depts
depts <- c("DHS", "EPA", "USDA", "DOT", "ED")
comms <- c("HOMELAND SECURITY", 
           "ENVIRONMENT", "AGRICULTURE", 
           "TRANSPORTATION", "OVERSIGHT", 
           "RULES", "BUDGET", "WAYS", "COMMERCE", 
           "APPROPRIATIONS", "Energy", "Health")



tenure %>% 
  filter(!is.na(TYPE)) %>%
  #filter(!is.na(tenure) & department %in% depts & committee %in% comms) %>%
  ggplot() + 
  # geom_text(aes(x = ifelse(tenure == 0, tenure, NA), y = ifelse(n>100, n, NA), label = member_committee, color = TYPE), size = 2, check_overlap = T) + 
  # geom_line(aes(x = tenure, y = n, color = TYPE, group = member_committee_TYPE), alpha = .2) +
  geom_vline(aes(xintercept = 0), color = "grey") + 
  geom_smooth(aes(x = tenure, y = n)) + #, color = TYPE))+#, color = "black")  + 
  facet_grid(TYPE ~ ., scales = "free_y") +
  #facet_grid(committee ~ department, scales = "free_y") +
  # facet_wrap(~committee, scales = "free_y") +
  scale_x_continuous(limits = c(-8,8), breaks = seq(-8,8,by =1)) + 
  labs(title = paste("Correspondence Before and After Appointment to Committee Chair"),
       x = "Years Before and After Appointment to Committee Chair",
       y = "Number of Letters")
  theme(legend.title = element_blank(),
        strip.text.y = element_text(angle = 0, size = 5),
        axis.text.x = element_text(angle = 0, size = 5),
        axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("chair effect selected pairs.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")



chairs %<>% 
  mutate(daysAsChair = subtract(DATE, firstassignedchairdate) ) %>%
  mutate(yearsAsChair = daysAsChair/365) %>%
  group_by(year) %>% mutate(n = n()) %>% ungroup() %>%
  mutate(committee_member = paste(committee, "-", last_name, firstassignedchair))

chairs %>%
  filter(committee %in% comms) %>%
  filter(chamber == chamb) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(TYPE = ifelse(TYPE == "To be coded", NA, TYPE)) %>%
  mutate(TYPE = ifelse(TYPE == "Corp. Policy", "Policy", TYPE)) %>%
  mutate(TYPE = ifelse(TYPE %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", TYPE)) %>%
  filter(!is.na(TYPE)) %>% 
  ggplot() + 
  labs(title = paste(chamb, "Committee Chairs Before and After Appointment (subset appointed 2009-2016)"),
       x = "Days Before and Affter Appointment") + 
  geom_density(aes(x = daysAsChair, fill = committee_member), alpha = .3)+#, color = position))  + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  #scale_color_grey() +
  scale_x_continuous(breaks = seq(-720,720,90), limits = c(-720,720)) + 
  facet_grid(committee ~ TYPE, scales = "free_y")  
 

  ggsave(paste(chamb, "chair pre post density.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")
  
  
chairs %>% 
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  ggplot() + 
  # geom_text(aes(x = ifelse(tenure == 0, tenure, NA), y = ifelse(n>100, n, NA), label = member_committee, color = TYPE), size = 2, check_overlap = T) + 
  # geom_line(aes(x = tenure, y = n, color = TYPE, group = member_committee_TYPE), alpha = .2) +
  geom_vline(aes(xintercept = 0), color = "black") + 
  geom_smooth(aes(x = yearsAsChair, y = n)) + #, color = TYPE))+#, color = "black")  + 
  facet_grid(. ~ chamber, scales = "free_y") +
  #facet_grid(committee ~ department, scales = "free_y") +
  # facet_wrap(~committee, scales = "free_y") +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,by =1)) + 
  labs(title = paste("Before and After Appointment to Committee Chair"),
       x = "Years Before and After Appointment to Committee Chair",
       y = "Number of Letters")

chairs %>% 
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(TYPE = ifelse(TYPE == "To be coded", NA, TYPE)) %>%
  mutate(TYPE = ifelse(TYPE == "Corp. Policy", "Policy", TYPE)) %>%
  mutate(TYPE = ifelse(TYPE %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", TYPE)) %>%
  filter(!is.na(TYPE)) %>% 
  ggplot() + 
  # geom_text(aes(x = ifelse(tenure == 0, tenure, NA), y = ifelse(n>100, n, NA), label = member_committee, color = TYPE), size = 2, check_overlap = T) + 
  # geom_line(aes(x = tenure, y = n, color = TYPE, group = member_committee_TYPE), alpha = .2) +
  geom_vline(aes(xintercept = 0), color = "black") + 
  geom_smooth(aes(x = yearsAsChair, y = n)) + #, color = TYPE))+#, color = "black")  + 
  facet_grid(TYPE ~ chamber, scales = "free_y") +
  #facet_grid(committee ~ department, scales = "free_y") +
  # facet_wrap(~committee, scales = "free_y") +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,by =1)) + 
  labs(title = paste("Before and After Appointment to Committee Chair"),
       x = "Years Before and After Appointment to Committee Chair",
       y = "Number of Letters")
theme(legend.title = element_blank(),
      strip.text.y = element_text(angle = 0, size = 5),
      axis.text.x = element_text(angle = 0, size = 5),
      axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("chair effect.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")



#####################################
# clean up workspace before commit #
#####################################
# rm(list = ls(all = TRUE))
