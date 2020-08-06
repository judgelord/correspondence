# augement 


#################
# District vars #
#################
nrow(members)
members %<>% left_join(read.csv("districts/states.csv") )
members %<>% mutate(state_pop2010_millions = pop2010/1000000)
nrow(members)



###############
# member vars #
###############
members$party <- NA 
members$party[members$party_code == 100] <- "(D)"
members$party[members$party_code == 200] <- "(R)"
members$party[members$party_code == 328] <- "(I)"

members %<>%
  mutate(member_party = paste(bioname, party)) %>%  
  mutate(member_state = paste(bioname, party, state_abbrev))  %>% 
  mutate(member_state = gsub(",.*\\("," \\(", member_state)) %>% 
  mutate(name_state = as.factor(paste(bioname, party, "-", state_abbrev))) %>% 
  mutate(name_state = factor(name_state, levels=rev(levels(name_state)))) 



members$presidents_party <- NA
# president's party
members %<>% 
  # CLINTON elected with 103th
  mutate(presidents_party = ifelse(congress >102 & congress < 107 & party == "(D)", 1, presidents_party)) %>% 
  # Bush elected with 107th
  mutate(presidents_party = ifelse(congress >106 & congress < 111 & party == "(R)", 1, 0)) %>% 
  # OBAMA elected with 111th
  mutate(presidents_party = ifelse(congress >110 & congress < 115 & party == "(D)", 1, presidents_party)) %>% 
  # TRUMP elected with 115th
  mutate(presidents_party = ifelse(congress > 114 & congress <117 & party == "(R)", 1, presidents_party)) 


# MEMBER DEMOGRAPHICS 

# gender for those where we have the data from LEP # WE HAVE BETTER DATA, NEEDS TO BE MERGED IN 
members %<>% 
  # merge LEP data into members 
  left_join(
    # read in the LEP data 
    read_csv("members/LEP111to113.csv") %>% 
      # just grabbing female variable for now
      select(icpsr, female) %>% 
      # distinct icpsr-gender combinations
      distinct() %>% 
      #make ICPSR numbers numeric to merge with members
      mutate(icpsr = as.numeric(icpsr))
  )
nrow(members)

#####################
# members Committee Vars #
#####################
# add committee chair data to members (still one observation per letter, unlike dcommittees)
# run after creating dcommittees because below members vars are across committees, e.g. chair = if chair of ANY committee in that congress


# FIXME
# JUST UNTIL WE FIX THESE IN COMMITTEE DATA via committees.R
# members$chair[members$icpsr==94910] # fixed in committees.R
# missing Critz in the 111th
# 
###########################################

# leadership positions
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = chair) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, ranking_minority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = ranking_minority) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_leader) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_leader) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, party_whip) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = party_whip) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, speaker) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = speaker) %>% distinct()
) %>% filter(!is.na(bioname))
nrow(members)

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, position) %>% 
    group_by(icpsr, congress, chamber) %>% 
    top_n(1, wt = position) %>% 
    distinct()
  ) %>% filter(!is.na(bioname))
nrow(members)

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, position) %>% 
    group_by(icpsr, congress, chamber) %>% 
    top_n(1, wt = position) %>% 
    distinct()
) %>% filter(!is.na(bioname))
nrow(members)

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, majority) %>% 
    group_by(icpsr, congress, chamber) %>% 
    top_n(1, wt = majority) %>% 
    distinct()
) %>% filter(!is.na(bioname))
nrow(members)

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, majority) %>% 
    group_by(icpsr, congress, chamber) %>% 
    top_n(1, wt = majority) %>% 
    distinct()
) %>% filter(!is.na(bioname))
nrow(members)


# FIXME 
# ADD BELOW TO MemberNameDateCorrections.R fix.member.dates function:
#  mutate(party = ifelse(name == "Specter, Arlen" & assigneddate < as.Date("2009-04-28"), 200, party)) %>% # THIS IS INSUFICIENT
#  mutate(icpsr = ifelse(name == "Specter, Arlen" & assigneddate > as.Date("2009-04-28"), 94110, icpsr)) %>%  # NEED TO CORRECT MEMBERSHIP ETC
# need to add Kennedy Joe, Jr and III to MemberNameDateCorrections.R
# /FIXME


# chair variable to text
members %<>% 
  mutate(position = ifelse(chair ==1, "Chair", NA)) %>%
  mutate(position = ifelse(ranking_minority == 1, "Ranking Minority", position)) 

bad.committees.2 <- filter(members, is.na(chair)) %>% group_by(icpsr, bioname, congress) %>% summarise(n = n()) %>% arrange(-n)

# partystatus
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, majority) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = majority) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% mutate(partystatus = ifelse(majority == 1, "Majority", "All Others"))

# prestige committees
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige) %>% distinct()
) %>% filter(!is.na(bioname))

members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, prestige_chair) %>% 
    group_by(icpsr, congress) %>% top_n(1, wt = prestige_chair) %>% distinct()
) %>% filter(!is.na(bioname))

# all committee names, sep = "|"
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, committees, chamber) %>% 
    group_by(icpsr, congress, chamber) %>% top_n(1, wt = committees) %>% distinct()
) %>% filter(!is.na(bioname))

# chairs committee names
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, chair_of) %>% 
    group_by(icpsr, congress, chamber) %>% top_n(1, wt = chair_of) %>% distinct() %>% filter(!is.na(chair_of))
) %>% filter(!is.na(bioname))

# year elected 
members %<>% full_join(
  committees %>% dplyr::select(icpsr,congress, chamber, yearelected) %>% distinct() 
) %>% filter(!is.na(bioname))

# Those who served as Chairs at some point
members %<>% mutate(chair_since_2007 = ifelse(bioname %in% c(unique(members$bioname[which(members$position == "Chair")])), T, F) )
# mutate(daysAsChair = ifelse(chair_since_2007 == T, subtract(DATE, firstassignedchairdate), NA) ) %>%
# mutate(yearsAsChair = daysAsChair/365) %>%
# mutate(monthsAsChair = daysAsChair/30) 


members %>% select(-contains(c("common", "comma", "middle", "initial", "_last","maiden")))




# FIXME
vars <- c("congress", "icpsr", "chamber", "chair", "ranking_minority", "party_leader", "party_whip", "speaker", "position", 
          "majority", "prestige", "prestige_chair", "committees", "chair_of", "yearelected", "chair_since_2007")

Unfoundnames <- members %>% 
  select(vars, bioname) %>% 
  filter(is.na(chair), 
         chamber != "President", 
         !congress %in% c(105, 116)) %>% 
  count(congress, bioname, chamber, icpsr) %>% select(-n) 

Unfoundnames%>% kable()

