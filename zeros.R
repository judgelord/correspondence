# this sheets makes a data frame of all possible combinations of member-year-type-agency and adds n = 0
# for adding 0s


load("gh-pages/correspondence.RData") # load data (df is d + covariates + dropping obs not matching an ICPSR)
d1 <- df # back up df because we are using it

requires <- c( "dplyr", "magrittr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(magrittr)
library(dplyr)

# add year to member file
zeros <- rbind(
  members %>% mutate(year = ((congress - 115)*2 + 2017)), # year 1 of term
  members %>% mutate(year = ((congress - 115)*2 + 2018)) ) # year 2

zeros %<>% mutate(icpsr_year = paste(icpsr, year))

# member year 
zeros %<>% full_join(
  data_frame(
    icpsr_year = rep(unique(zeros$icpsr_year), n_distinct(df$Type)),
    Type =  rep(unique(df$Type), n_distinct(zeros$icpsr_year)) ) ) %>%
  mutate(icpsr_year_type = paste(icpsr_year, Type))

# member year agency
zeros %<>% full_join(
  data_frame(
    icpsr_year_type = rep(unique(zeros$icpsr_year_type), n_distinct(df$agency)),
    agency =  rep(unique(df$agency), n_distinct(zeros$icpsr_year_type)) ) 
) %>%
  mutate(icpsr_year_type_agency = paste(icpsr_year_type, agency)) 


###########################################################

###########################################################

# Coppied from merge.R ####################################

zeros$icpsr %<>% as.numeric()

zeros %<>% filter(year < 2018 & year > 2006)


zeros %<>% ungroup()
df <- filter(zeros, !is.na(icpsr), !is.na(year), chamber %in% c("House", "Senate")) # select only voteview-matched observations
committees %<>% select(-party) # drop Stewart committee data party codes 



# TIMESERIES COMPLETENESS 
# identify timeframe and completeness for each agency
df %<>% group_by(agency) %>% mutate(timeframe = paste(sort(unique(year)), collapse = ":")) %>%
  mutate(timeframe = paste(agency, timeframe)) %>%
  mutate(complete = ifelse(grepl("2007", timeframe) &
                             grepl("2008", timeframe) & 
                             grepl("2009", timeframe) &
                             grepl("2010", timeframe) &
                             grepl("2011", timeframe) &
                             grepl("2012", timeframe) &
                             grepl("2013", timeframe) &
                             grepl("2014", timeframe) &
                             grepl("2015", timeframe) &
                             grepl("2016", timeframe)  #& grepl("2017", timeframe)
                           , T, F)) %>% ungroup()
# Timeframe:
unique(cbind(df$complete ,df$timeframe))
# Problems: 
data_list %>% filter(!(agency %in% df$agency)) 

####################################################################################
# yearly totals for core APSA2018 model 
df %<>% group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() %>%
  mutate(bioname_year = paste(bioname, year))
df$year %<>% as.numeric()
# members with zero letters in a year
zeros <- data_frame(
  year = as.numeric(rep(unique(df$year), n_distinct(members$bioname))), 
  bioname =  rep(unique(members$bioname), n_distinct(df$year)),
  permemberyear = 0) %>%
  mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) %>% 
  left_join(members) %>% 
  filter(!is.na(icpsr), chamber %in% c("House", "Senate")) %>% 
  mutate(bioname_year = paste(bioname, year)) %>% 
  filter(!bioname_year %in% df$bioname_year)

df %<>% full_join(zeros)

############
# New vars #
############
df$department <- gsub("_.*", "", df$agency) # name dept
df %<>% mutate(id = paste(agency, ID)) # unique ID

# numeric to text 
df$Type <- NA
df$Type[is.na(df$TYPE)] <- "To be coded"
df$Type[df$TYPE == 0] <- "To be coded"
df$Type[df$TYPE == 1] <- "Indiv. Constituent"
df$Type[df$TYPE == 2] <- "Corp. Constituent"
df$Type[df$TYPE == 3] <- "501c3 or Local Gov."
df$Type[df$TYPE == 4] <- "Corp. Policy"
df$Type[df$TYPE == 5] <- "Policy"
df$Type[df$TYPE == 6] <- "To be coded"


df$Type %<>% as.factor()
df$Type <- factor(df$Type, c("Indiv. Constituent", "Corp. Constituent", "501c3 or Local Gov.", 
                             "Corp. Policy", "Policy", "To be coded"))   


df %<>% 
  mutate(Type2 = ifelse(Type %in% c("Policy", "Corp. Policy"), "Policy", NA)) %>%
  mutate(Type2 = ifelse(Type %in% c("501c3 or Local Gov.", "Corp. Constituent", "Indiv. Constituent"), "Constituent Service", Type2)) 

df$party <- NA 
df$party[df$party_code == 100] <- "(D)"
df$party[df$party_code == 200] <- "(R)"
df$party[df$party_code == 328] <- "(I)"

df %<>%
  mutate(member_party = paste(bioname, party)) %>%  
  mutate(member_state = paste(bioname, party, state_abbrev))  %>% 
  mutate(member_state = gsub(",.*\\("," \\(", member_state))


# transformation vars 
df %<>% 
  mutate(month = format(DATE, "%Y-%m")) %>% 
  group_by(bioname, month) %>% mutate(permonth = n()) %>% ungroup() %>% 
  mutate(cal.month = format(DATE, "%m(%b)")) %>% 
  mutate(name_state = as.factor(paste(bioname, party, "-", state_abbrev))) %>% 
  mutate(name_state = factor(name_state, levels=rev(levels(name_state)))) %>% 
  mutate(name_agency = paste(name_state, agency)) %>%
  mutate(name_dept = paste(name_state, department))

#################
# District vars #
#################
df %<>% left_join(read.csv("districts/states.csv") )
df %<>% mutate(state_pop2010_millions = pop2010/1000000)








###############
######################################################





######################################################
# member vars #
###############
# shorten party name
df$party_name <- gsub(" Party", "", df$party_name)

# president's party
df %<>% 
  mutate(presidents_party = ifelse(year > 2000 & year < 2009 & party == "(R)", 1, 0)) %>% 
  mutate(presidents_party = ifelse(year > 2008 & year < 2017 & party == "(D)", 1, presidents_party)) %>% 
  mutate(presidents_party = ifelse(year > 2016 & year < 2021 & party == "(R)", 1, presidents_party)) 

# election cycle 
df %<>% 
  mutate(election_year = ifelse(chamber == "Senate" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 6, yearelected+12, yearelected+18, yearelected+24, yearelected+30), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) %>%
  mutate(election_year = ifelse(chamber == "House" & 
                                  !is.na(yearelected) &
                                  year %in% c(yearelected, yearelected + 2, yearelected+4, yearelected+6, yearelected+8, yearelected+10, yearelected+12, yearelected+14, yearelected+16, yearelected+18, yearelected+20), #c(seq(yearelected, yearelected + 60, 6)),
                                1, 0)) 

# gender for those where we have the data from LEP # WE HAVE BETTER DATA, NEEDS TO BE MERGED IN
df$icpsr %<>% as.numeric()

df %<>% left_join(
  read.csv("members/LEP111to113.csv") %>% select(icpsr, female) %>% distinct() %>% filter(icpsr %in% df$icpsr) %>% mutate(icpsr = as.numeric(icpsr))
)

# TOTALS
df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df %<>% fix.member.date.coding()
df %<>% filter(!(icpsr == 94910 & year == 2009)) # remove Arlen Specter as GOP
df %<>% filter(!(icpsr == 90901 & year == 2009)) # remove Grifith Parker as GOP








#############################################################################
# create dcommittees #
######################
# merge committee data to one obs per letter per committee
committees %<>% select(-partystatus)
committees %<>% filter(!is.na(icpsr))
committees %<>%  filter(!is.na(congress)) 
dcommittees <- df %>% full_join(committees) %>% filter(!is.na(DATE)) # select committee data matching obs

dcommittees %<>% mutate(committee_dept = paste(committee, department)) 
# Compare DATE and assigned date
dcommittees %<>% group_by(member_committee) %>% 
  mutate(firstassignedchairdate = min(assignedchairdate, na.rm = TRUE)) %>% ungroup() %>% 
  mutate(firstassignedchair = as.numeric(substring(firstassignedchairdate, 1, 4)))  %>%
  mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
  mutate(yearsAsChair = daysAsChair/365) %>%
  mutate(monthsAsChair = daysAsChair/30) %>%
  mutate(chair = ifelse(chair_since_2007 == T, paste(firstassignedchair,  bioname, party), NA) ) # note this overwrites 0/1 chair variable


#####################
###########################################################################










#####################
# df Committee Vars #
#####################
# add committee chair data to df (still one observation per letter, unlike dcommittees)
# run after creating dcommittees because below df vars are across committees, e.g. chair = if chair of ANY committee in that congress

# FIXME
# missing Critz in the 111th?


# leadership positions
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = chair) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, ranking_minority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = ranking_minority) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_leader) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_leader) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_whip) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_whip) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, speaker) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = speaker) %>% distinct()
) %>% filter(!is.na(bioname))


# FIXME 
# ADD BELOW TO MemberNameDateCorrections.R fix.member.dates function:
#  mutate(party = ifelse(name == "Specter, Arlen" & assigneddate < as.Date("2009-04-28"), 200, party)) %>% # THIS IS INSUFICIENT
#  mutate(icpsr = ifelse(name == "Specter, Arlen" & assigneddate > as.Date("2009-04-28"), 94110, icpsr)) %>%  # NEED TO CORRECT MEMBERSHIP ETC
# need to add Kennedy Joe, Jr and III to MemberNameDateCorrections.R
# /FIXME


# chair variable to text
df %<>% 
  mutate(position = ifelse(chair ==1, "Chair", NA)) %>%
  mutate(position = ifelse(ranking_minority == 1, "Ranking Minority", position)) 

bad.committees.2 <- filter(df, is.na(chair)) %>% group_by(icpsr, bioname, congress) %>% summarise(n = n()) %>% arrange(-n)

# partystatus
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, majority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = majority) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% mutate(partystatus = ifelse(majority == 1, "Majority", "All Others"))

# prestige committees
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige) %>% distinct()
) %>% filter(!is.na(bioname))

df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige_chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige_chair) %>% distinct()
) %>% filter(!is.na(bioname))

# all committee names, sep = "|"
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, committees) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = committees) %>% distinct()
) %>% filter(!is.na(bioname))

# chairs committee names
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chair_of) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = chair_of) %>% distinct() %>% filter(!is.na(chair_of))
) %>% filter(!is.na(bioname))

# year elected 
df %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, yearelected) %>% distinct() 
) %>% filter(!is.na(bioname))

# Those who served as Chairs at some point
df %<>% mutate(chair_since_2007 = ifelse(bioname %in% c(unique(df$bioname[which(df$position == "Chair")])), T, F) )
# mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
# mutate(yearsAsChair = daysAsChair/365) %>%
# mutate(monthsAsChair = daysAsChair/30) 

df %<>% 
  group_by(bioname, year) %>% mutate(permemberyear = n()) %>% ungroup() 

# clean up problems with party switchers etc. that may have come in with merge 
df %<>% fix.member.date.coding() #  should have dealt with party switchers (Arlen Specter)


#####################

########################################################################
df %<>% filter(!is.na(agency)) # drop any NAs resulting from other merges before merging oversight data 

# Add agency names by acronym from the FOIA List google sheet
df %<>% left_join(
  read.csv("agencies/FOIA List.csv")
  )

df %<>% left_join(
  # From Lewis and Seldin AJPS
  data <- read.csv("committees/ACUS.csv") %>% select(Agency, Reporting.Committees, Number.of.Committees, Committeesconfirmingapps, Employees, Independent.Funding, Rulemaking) %>% filter(!is.na(Number.of.Committees)) %>% rename(Department = Agency)
)

# match to committee list 
df$oversight_committee <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$chair_of[i]) & 
     !is.na(df$Reporting.Committees[i]) & 
     grepl(df$committees[i], df$Reporting.Committees[i], ignore.case = T) ) {
    df$oversight_committee[i] <- 1
  } } 
sum(df$oversight_committee)

# match to committee chair (excludes library and printing)
df$oversight_committee_chair <- 0

for(i in 1:nrow(df)){
  if(!is.na(df$chair_of[i]) & 
     !is.na(df$Reporting.Committees[i]) & 
     grepl(df$chair_of[i], df$Reporting.Committees[i], ignore.case = T) ) {
    df$oversight_committee_chair[i] <- 1
  } } 
sum(df$oversight_committee_chair)
########################################################################################################



zeros$n <- 0



write.csv(df, "gh-pages/zeros.csv")

df <- d1
