# this file is a sketchpad for developing figures. See summary.html for selected versions 

options(stringsAsFactors = FALSE)
requires <- c("dplyr", "ggplot2", "magrittr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
library(dplyr) 
library(ggplot2)
library(magrittr)

# Refresh data? Or load archived data file from https://drive.google.com/drive/u/0/folders/1DSGGZP_v2zwdfxg9Do3Ii4Y8UdXultVg
ifelse( F ,  source("merge.R"), load("correspondence.RData") )

# inspect data completeness and coding
df %>% 
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>%
  # mutate(coded = ifelse(TYPE == "To be coded", "To be coded", "Coded")) %>% 
  ggplot() + 
  labs(title = "Data Sources Reasonably Complete?",
       y = paste("Total observations matched with ICPSR =", nrow(df) )) +
  geom_point(aes(x = DATE, y = reorder(paste(agency, n), n), color = coded), alpha = .2) +
   facet_grid(complete ~., scales = "free_y", space = "free_y")
ggsave(paste("coding status.pdf"), width = 11, height = 8.5, path = "~/correspondence/figs")


# CDC is rolling release, 2010-2011 expected Nov 2018
# SBA has no records before 2010
# DOJ_CIV is a rolling release - 2009-2011 recieved July 2018
# CBP waiting since april 2017
# USDA RMA has no logs prior to 2010
# PRC no responsive records for FY 2007 or FY 2008. Tracking did not start until FY 2009


df %>% 
  mutate(coded = ifelse(TYPE == "To be coded", "To be coded", "Coded")) %>% 
  filter(TYPE != "To be coded") %>% 
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>%
  ggplot() + 
  geom_point(aes(x = DATE, y = reorder(paste(agency, n), n), color = TYPE), alpha = .2) + 
  labs(title = "Missing and Complete Data") +
  facet_grid(complete ~ ., scales = "free_y", space = "free_y")





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
df$TYPE %<>% as.numeric()
df %>% 
  filter(Type != "To be coded") %>% 
  group_by(bioname, nominate.dim1, chamber, Type, TYPE, last_name) %>% tally() %>% ungroup()  %>% 
  group_by(chamber, Type) %>% mutate(percentile = ntile(n, 100)) %>%
  ggplot() +
  geom_tile(aes(x= chamber, y = Type), fill = "white", color = "grey") +
  geom_text(
    aes(x = chamber, y = reorder(Type, TYPE), label = last_name, size = n, alpha = percentile, color = nominate.dim1),
    position=position_jitter()
  ) +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(y = "Type", 
       x= "Chamber",
       title = paste("Types of Correspondence")) +
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

Chamber <- "Senate" # "House" # 

# define y axis
df$yaxis <- df$bioname

# member by year by agency 
df %>% 
  filter(chamber == Chamber, complete == T) %>%
  group_by(yaxis) %>% mutate(n = n()) %>% ungroup() %>% 
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(yaxis, n), # sort by total number of lewters writen
        color = agency), 
    alpha = .3,
    shape = 73,
    size=2
  ) +
  labs(title = paste(Chamber),
       y = paste("Members by", "n"), 
       x = "Date of Correspondence" ) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 45)
  ) 
ggsave(paste("members_by_year_agency", Chamber, "n", ".pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")



df %>% 
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





Chamber = "Senate"
# boxplots by year 
df %>% group_by(yaxis, year, chamber, nominate.dim1) %>% tally() %>% ungroup() %>% 
  mutate(mean = mean(n)) %>%
  filter(chamber == Chamber) %>%
  ggplot() + 
  geom_boxplot(
    aes(x = reorder(yaxis, n), y = n, color = nominate.dim1), outlier.shape = NA) + 
  coord_flip() +
  scale_colour_gradient2(low = "blue", mid = "grey", high = "red") +
  labs(title = paste("Letters per Year from Members of the U.S.", Chamber)) + 
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




Chamber = "Senate"
chairs %>% 
  filter(chamber == Chamber, complete == T, agency != "PRC", DATE < as.Date("2017-01-01")) %>%
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
  labs(title = paste(Chamber, "Committee Chairs"),
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
  filter(chamber == chamb, agency != "PRC", DATE < as.Date("2017-01-01"))  %>%
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

mean(tenure$nn[tenure$tenure == "Year  Before"])
mean(tenure$nn[tenure$tenure == "Year After"])

tenure %>% 
  ggplot() + 
  geom_text(aes(x = tenure, y = nn, label = member_committee), size  = 2) + 
  geom_boxplot(aes(x = tenure, y = nn)) + 
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
year.before.chair <- mean(tenure$n[tenure$tenure == -1])
year.after.chair <- mean(tenure$n[tenure$tenure == 0])
mean(tenure$n[tenure$tenure == 1])
mean(tenure$n[tenure$tenure == 2])

# define matchable committees and depts
depts <- c("DHS", "EPA", "USDA", "DOT", "ED")
comms <- c("HOMELAND SECURITY", "AGRICULTURE", 
           "TRANSPORTATION", "OVERSIGHT", 
           "RULES", "BUDGET", "WAYS", "COMMERCE", 
           "APPROPRIATIONS", "ENERGY")



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





chairs %>%
  filter(committee %in% comms, last_name != "STARK") %>%
  #filter(chamber == chamb) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(TYPE = ifelse(TYPE == "To be coded", NA, TYPE)) %>%
  mutate(TYPE = ifelse(TYPE == "Corp. Policy", "Policy", TYPE)) %>%
  mutate(TYPE = ifelse(TYPE %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", TYPE)) %>%
  filter(!is.na(TYPE)) %>% 
  ggplot() + 
  labs(title = paste("Committee Chairs Before and After Appointment (subset appointed 2009-2015)"),
       x = "Days Before and Affter Appointment") + 
  geom_density(aes(x = daysAsChair, fill = paste(chamber, committee_member)), alpha = .3)+#, color = position))  + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  #scale_color_grey() +
  scale_x_continuous(breaks = seq(-720,720,720), limits = c(-720,720)) + 
  facet_grid(TYPE ~ committee, scales = "free_x")  + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 5)
        )
 

  ggsave(paste("chair pre post density by type.pdf"), width = 11, height =8.5,  path = "~/correspondence/figs")
  
  
  comms <- c("HOMELAND SECURITY", "AGRICULTURE", 
             "TRANSPORTATION", "OVERSIGHT", 
             "RULES", "BUDGET", "WAYS", "COMMERCE", 
             "APPROPRIATIONS", "ENERGY")
chairs %>%
    filter(committee %in% comms, complete == T, department %in% c("DHHS", "DHS", "DOC", "DOD", "DOE", "DOI", "DOL", "DOT", "EPA", "USDA")) %>%
    filter(chamber == Chamber) %>%
    filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
    ggplot() + 
    labs(title = paste("Committee Chairs Before and After Appointment (subset appointed 2009-2015)"),
         x = "Days Before and Affter Appointment") + 
    geom_density(aes(x = daysAsChair, fill = paste(chamber, committee_member)), alpha = .3)+#, color = position))  + 
    geom_vline(aes(xintercept = 0), color = "black") + 
    #scale_color_grey() +
    scale_x_continuous(breaks = seq(-720,720,720), limits = c(-720,720)) + 
    facet_grid(committee ~ ., scales = "free_y")  + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 5))
  
  ggsave(paste("chair pre post density by dept.pdf"), width = 11, height =8.5,  path = "~/correspondence/figs")
  
  comms <- c("HOMELAND SECURITY", "AGRICULTURE", 
             "TRANSPORTATION", "OVERSIGHT", 
             "RULES", "BUDGET", "WAYS", "COMMERCE", 
             "APPROPRIATIONS", "ENERGY")
chairs %>% 
  filter(complete == T, committee %in% comms, last_name != "STARK") %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  ggplot() + 
  # geom_text(aes(x = ifelse(tenure == 0, tenure, NA), y = ifelse(n>100, n, NA), label = member_committee, color = TYPE), size = 2, check_overlap = T) + 
  # geom_line(aes(x = tenure, y = n, color = TYPE, group = member_committee_TYPE), alpha = .2) +
  geom_vline(aes(xintercept = 0), color = "black") + 
  geom_density(aes(x = monthsAsChair, fill = committee, color = committee), alpha = .1) + 
  #geom_smooth(aes(x = monthsAsChair, y = n)) + #, color = TYPE))+#, color = "black")  + 
  facet_grid(. ~ chamber, scales = "free_y") +
  #facet_grid(committee ~ department, scales = "free_y") +
  # facet_wrap(~committee, scales = "free_y") +
  scale_x_continuous(limits = c(-24,24), breaks = seq(-24,24,by =6)) + 
  labs(title = paste("Before and After Appointment to Committee Chair"),
       x = "Months Before and After Appointment to Committee Chair",
       y = "Density")


comms <- c("HOMELAND SECURITY", "AGRICULTURE", 
           "TRANSPORTATION", "OVERSIGHT", 
           "RULES", "BUDGET", "WAYS", "COMMERCE", 
           "APPROPRIATIONS", "ENERGY")
chairs %>% 
  #filter(committee %in% comms) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(TYPE = ifelse(TYPE == "To be coded", NA, TYPE)) %>%
  mutate(TYPE = ifelse(TYPE == "Corp. Policy", "Policy", TYPE)) %>%
  mutate(TYPE = ifelse(TYPE %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", TYPE)) %>%
  filter(!is.na(TYPE)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  geom_smooth(aes(x = monthsAsChair, y = permonth_permember))+#, color = Type))+#, color = "black")  + 
  facet_grid(chamber ~ ., scales = "free_y") +
  scale_x_continuous(limits = c(-24,24), breaks = seq(-24,24,by =1)) + 
  labs(title = paste("Before and After Appointment to Committee Chair"),
       x = "Months Before and After Appointment to Committee Chair",
       y = "Number of Letters per Month")
theme(legend.title = element_blank(),
      strip.text.y = element_text(angle = 0, size = 5),
      axis.text.x = element_text(angle = 0, size = 5),
      axis.text.y = element_text(angle = 0, size = 5))

ggsave(paste("chair effect.pdf"), width = 8.5, height = 11,  path = "~/correspondence/figs")



 


#  tile 
Chamber = "House"

# zeros
zeros <- data_frame(
  name_state = rep(unique(df$name_state), n_distinct(df$department)), 
  department =  rep(unique(df$department), n_distinct(df$name_state)),
  perMember = 0) %>%
  mutate(name_dept = paste(name_state, department)) %>% 
  filter(!name_dept %in% df$name_dept)

data <- df %>% 
  filter(chamber == Chamber) %>%
  group_by(department, name_state, year) %>% mutate(n = n()) %>% ungroup() %>%
  group_by(department, name_state) %>% mutate(perMember = mean(n)) %>% ungroup() %>% 
  full_join(zeros) %>% # add zeros
  group_by(department) %>% mutate(mean = mean(perMember), sd = sd(perMember), sd.above.mean = (perMember - mean)/sd, perAgency = n()) %>% ungroup() %>%
  filter(perAgency > 1000) %>% # select agencies with significant data
  group_by(name_state) %>% filter(sum(perMember) > 10000) %>% ungroup() %>% # select prolific members
  group_by(department, name_state, sd.above.mean, perMember, sd) %>% tally() %>% ungroup()

ggplot(data) + # plot
  geom_tile(aes(x = department, y = name_state, fill = sd.above.mean))  + 
  geom_text(aes(x = department, y = name_state, label = round(perMember, 0 ))) + 
  labs(title = paste("Average Letters per Year per Department or Agency
(Showing members averaging more than 1000 and agencies receiving more than 1,000)
Ovarall mean =", round(mean(data$perMember), 0), ", Mean Standard Deviation = ", round(mean(data$sd), 0)),
       x = "",
       y = Chamber)
###


# member-TYPE distance from the mean (cross-tab/tile)
Chamber = "Senate"

data <- df %>% 
  filter(chamber == Chamber, Type != "To be coded") %>%
  group_by(Type, name_state, year) %>% mutate(n = n()) %>% ungroup() %>%
  group_by(Type, name_state) %>% mutate(perMember = mean(n)) %>% ungroup() %>% 
  group_by(Type) %>% mutate(mean = mean(perMember), sd = sd(perMember), sd.above.mean = (perMember - mean)/sd) %>% ungroup() %>%
  group_by(name_state) %>% filter(sum(perMember) > 5000) %>% ungroup() %>% # select prolific members
  group_by(Type, name_state, sd.above.mean, perMember, sd) %>% tally() %>% ungroup()

ggplot(data) + # plot
  geom_tile(aes(x = Type, y = name_state, fill = sd.above.mean))  + 
  geom_text(aes(x = Type, y = name_state, label = round(perMember, 0 ))) + 
  labs(title = paste("Average Letters per Year per Type or Agency
(Showing members averaging more than 1000 and agencies receiving more than 1,000)
Ovarall mean =", round(mean(data$perMember), 0), ", Mean Standard Deviation = ", round(mean(data$sd), 0)),
       x = "",
       y = Chamber)

comms <- c( "OVERSIGHT", "RULES", "BUDGET", "WAYS", "COMMERCE", 
            "APPROPRIATIONS", "ARMED SERVICES", "FINANCE", "FOREIGN RELATIONS") 

chairs %>% 
  filter(complete == T) %>% #, committee %in% comms) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(prestige = ifelse(committee %in% c( "OVERSIGHT", "RULES", "BUDGET", "WAYS", "COMMERCE", 
     "APPROPRIATIONS", "ARMED SERVICES", "FINANCE", "FOREIGN RELATIONS"), "Prestige", "Not prestige") ) %>% 
  mutate(Type = ifelse(Type == "To be coded", NA, Type)) %>%
  mutate(Type = ifelse(Type == "Corp. Policy", "Policy", Type)) %>%
  mutate(Type = ifelse(Type %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", Type)) %>%
  filter(!is.na(Type)) %>% 
  filter(!last_name %in% c("STARK", "ROGERS", "LEVIN", "CONAWAY")) %>% 
  ggplot() + 
  geom_density(aes(x = monthsAsChair, fill = prestige, color = prestige), alpha = .2, bw = 2) + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  facet_grid(Type ~ chamber, scales = "free_y") +
  scale_x_continuous(limits = c(-24,24), breaks = seq(-24,24,by =6)) + 
  guides(fill=guide_legend(ncol=1))+
  labs(title = paste("Correspondence Before and After Appointment to Committee Chair"),
       x = "Months Before and After First Appointment to Committee Chair",
       y = "Correspondence per Month")


# prestige committee members
# density 
chairs %>% 
  filter(complete == T, committee %in% comms) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(Type = ifelse(Type == "To be coded", NA, Type)) %>%
  mutate(Type = ifelse(Type == "Corp. Policy", "Policy", Type)) %>%
  mutate(Type = ifelse(Type %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", Type)) %>%
  filter(!is.na(Type)) %>% 
  filter(!last_name %in% c("STARK", "ROGERS", "LEVIN", "CONAWAY")) %>% 
  ggplot() + 
  geom_density(aes(
    x = monthsAsChair, 
    fill = paste(chamber, committee_member), 
    color = paste(chamber, committee_member)), alpha = .1, bw = 2, position = "stack") + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  facet_grid(Type ~ chamber, scales = "free_y") +
  scale_x_continuous(limits = c(-24,24), breaks = seq(-24,24,by =6)) + 
  guides(fill=guide_legend(ncol=1))+
  labs(title = paste("Correspondence Before and After Appointment to Prestige Committee Chair"),
       x = "Months Before and After Appointment to Committee Chair",
       y = "Correspondence per Month")


# count
chairs %>% 
  filter(complete == T, committee %in% comms) %>%
  filter(firstassignedchair < 2016, firstassignedchair > 2008) %>%
  mutate(Type = ifelse(Type == "To be coded", NA, Type)) %>%
  mutate(Type = ifelse(Type == "Corp. Policy", "Policy", Type)) %>%
  mutate(Type = ifelse(Type %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", Type)) %>%
  filter(!is.na(Type)) %>% 
  filter(!last_name %in% c("STARK", "ROGERS", "LEVIN", "CONAWAY")) %>% 
  ggplot() + 
  geom_density(aes(
    x = monthsAsChair, 
    y = ..count.., 
    fill = paste(chamber, committee_member), 
    color = paste(chamber, committee_member)), alpha = .1, bw = 2) + 
  geom_vline(aes(xintercept = 0), color = "black") + 
  facet_grid(Type ~ chamber, scales = "free_y") +
  scale_x_continuous(limits = c(-24,24), breaks = seq(-24,24,by =6)) + 
  guides(fill=guide_legend(ncol=1))+
  labs(title = paste("Correspondence Before and After Appointment to Prestige Committee Chair"),
       x = "Months Before and After Appointment to Committee Chair",
       y = "Correspondence per Month")



# COMMITTEES 
Chamber <- "House"
# zeros
zeros <- data_frame(
  committee = rep(unique(dcommittees$committee[dcommittees$chamber == Chamber]), n_distinct(dcommittees$department) ), 
  department =  rep(unique(dcommittees$department), n_distinct(dcommittees$committee[dcommittees$chamber == Chamber]) ),
  n = 0) %>%
  mutate(committee_dept = paste(committee, department)) %>% 
  filter(!committee_dept %in% dcommittees$committee_dept)

data <- dcommittees %>% 
  filter(chamber == Chamber) %>% 
  filter(!department %in% c("PRC", "NASA", "FCA", "Amtrak", "RRB")) %>% 
  filter(!is.na(committee), !committee %in% c("PRINTING", "MINORITY WHIP", "MINORITY LEADER", "MAJORITY WHIP", "MAJORITY LEADER", "LIBRARY", "ETHICS", "SPEAKER", "INVESTIGATE THE VOTING IRREGULARITIES OF AUGUST 2", "HOUSE ADMINISTRATION", "EVENTS SURROUNDING THE 2012 TERRORIST ATTACK ON BENGHAZI", "ASSISTANT MINORITY LEADER"))%>% 
  group_by(committee, department, chamber, year) %>% tally() %>% ungroup() %>% 
  group_by(committee, department, chamber) %>% summarise(n = mean(n)) %>% ungroup() %>%
  full_join(zeros) %>% 
  filter(!department %in% c("PRC", "NASA", "FCA", "Amtrak", "RRB")) %>% 
  filter(!is.na(committee), !committee %in% c("PRINTING", "MINORITY WHIP", "MINORITY LEADER", "MAJORITY WHIP", "MAJORITY LEADER", "LIBRARY", "ETHICS", "SPEAKER", "INVESTIGATE THE VOTING IRREGULARITIES OF AUGUST 2", "HOUSE ADMINISTRATION", "EVENTS SURROUNDING THE 2012 TERRORIST ATTACK ON BENGHAZI", "ASSISTANT MINORITY LEADER", "ENERGY INDEPENDENCE", "DEFICIT REDUCTION", "STANDARDS OF OFFICIAL CONDUCT"))%>% 
  group_by(department) %>% mutate(mean = mean(n), sd = sd(n)) %>% 
  mutate(above = n - mean) %>% 
  mutate(sd.above.mean = above/sd) %>% ungroup %>%
  mutate(prestige = ifelse(committee %in% c( "RULES", "BUDGET", "WAYS", "COMMERCE", 
                                             "APPROPRIATIONS", "ARMED SERVICES", "FINANCE", "FOREIGN RELATIONS"), "Prestige", "Not prestige") ) 
  
  ggplot(data) + # plot
  geom_tile(aes(x = department, y = committee, fill = sd.above.mean))  + 
  geom_text(aes(x = department, y = committee, label = round(n, 0 ))) +
  facet_grid(prestige ~ ., scales = "free_y", space = "free_y") +
  labs(title = paste(Chamber, "Committee Correspondence per Year per Department or Agency 2007-2017"),
       x = "Department",
       y = "")
  
  
  
  
  
  
  # party

  
  
  df %>% 
    filter(party != "(I)", partystatus %in% c("Majority", "Minority")) %>% 
    group_by(party, partystatus, chamber, member_state, year) %>% tally() %>% 
    group_by(chamber, party, partystatus) %>% mutate(perParty = mean(n))%>% ungroup() %>% 
    group_by(chamber) %>% mutate(sd = sd(perParty), mean = mean(perParty)) %>%
    mutate(sd.above.mean = (perParty - mean)/sd, 0) %>% ungroup() %>%
    group_by(party, sd.above.mean, perParty, partystatus, chamber) %>% tally() %>% 
    ggplot() + 
    geom_tile(aes(x = party, y = partystatus, fill = sd.above.mean))  +
    geom_text(aes(x = party, y = partystatus, label = round(perParty, 0)))  +
    facet_grid(. ~ chamber) + 
    labs(title = "Average Correspondence per Year by Party and Majority Status")
  
  
  df %>% 
    filter(party != "(I)", partystatus %in% c("Majority", "Minority"), Type != "To be coded") %>% 
    group_by(Type, party, partystatus, chamber, member_state, year) %>% tally() %>% 
    group_by(Type, chamber, party, partystatus) %>% mutate(perParty = mean(n))%>% ungroup() %>% 
    group_by(Type, chamber) %>% mutate(sd = sd(perParty), mean = mean(perParty)) %>%
    mutate(sd.above.mean = (perParty - mean)/sd, 0) %>% ungroup() %>%
    group_by(Type, party, sd.above.mean, perParty, partystatus, chamber) %>% tally() %>% 
    ggplot() + 
    geom_tile(aes(x = party, y = partystatus, fill = sd.above.mean))  +
    geom_text(aes(x = party, y = partystatus, label = round(perParty, 1)))  +
    facet_grid(Type ~ chamber) + 
    labs(title = "Average Correspondence per Year by Party and Type")
  
  df %>% 
    filter(complete == T, party != "(I)") %>%
    ggplot() +
    geom_density(aes(x = DATE, fill = party, color = party), alpha = .1) + facet_grid(chamber ~.)
  
  
# grouping by type 
df %>% 
  filter(Type != "To be coded") %>%
    group_by(Type, member_state, chamber, year) %>% tally() %>%
    group_by(Type, member_state, chamber) %>% mutate(mean = mean(n)) %>% ungroup() %>% 
    group_by(Type, chamber) %>% mutate(Percentile = ntile(mean, 100)) %>% 
  arrange(-Percentile) %>% 
    group_by(Type, Percentile, chamber, mean) %>% mutate(n = sum(n)) %>% ungroup() %>% 
  select(member_state, Type, Percentile, mean, chamber) %>% distinct() %>% 
    ggplot() + 
    geom_line(aes(x = Percentile, y = mean, color = Type)) + 
  geom_point(aes(x = Percentile, y = mean, color = Type)) + 
  geom_text(aes(x = Percentile, y = mean, label = ifelse(Percentile > 94, member_state, "")), check_overlap = T, size = 2, hjust = "inward") + 
    facet_grid(Type ~ chamber , scales = "free_y") + 
  labs(title = "Average Correspondence per Year by Percential and Type
(labels = 95th percentile)",
       x = "Percentile (Calculated within types)")

# not grouping 
df %>% 
  filter(Type != "To be coded") %>%
  group_by(Type, member_state, chamber, year) %>% tally() %>% ungroup() %>% 
  group_by(Type, member_state, chamber) %>% mutate(mean = mean(n)) %>% ungroup() %>% 
  group_by(member_state, chamber, year) %>% mutate(np = sum(n)) %>%
  group_by(member_state, chamber) %>% mutate(meanp = mean(np)) %>% ungroup() %>% 
  group_by(chamber) %>% mutate(Percentile = ntile(meanp, 100)) %>% 
  arrange(-Percentile) %>% 
  select(member_state, Type, Percentile, mean, chamber) %>% distinct() %>% 
  ggplot() + 
  #geom_line(aes(x = Percentile, y = mean, color = Type)) + 
  geom_point(aes(x = Percentile, y = mean, color = Type)) + 
  geom_text(aes(x = Percentile, y = mean, label = ifelse(Percentile > 94, member_state, "")), check_overlap = T, size = 2, hjust = "inward") + 
  facet_grid(Type ~ chamber , scales = "free_y") + 
  labs(title = "Average Correspondence per Year by Overall Percentile
(labels = 95th percentile)",
       x = "Overall Percentile (Calculated across types)")


# member-year distance from the mean (cross-tab/tile)
Chamber = "House"

data <- df %>% 
  filter(chamber == Chamber, !is.na(year), complete == T) %>%
  group_by(year, name_state) %>% tally() %>% ungroup() %>%
  group_by(year) %>% mutate(mean = mean(n), sd = sd(n), sd.above.mean = (n - mean)/sd) %>% ungroup() %>% 
  group_by(name_state) %>% filter(length(unique(year)) > 8) %>% ungroup()

ggplot(data) + # plot
  geom_tile(aes(x = year, y = name_state, fill = sd.above.mean))  + 
  # geom_text(aes(x = year, y = name_state, label = round(n, 0 ))) + 
  labs(title = paste("Letters per Year"),
       x = "",
       y = Chamber)
  