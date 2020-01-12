
source("setup.R")


# metadata here: https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20Candidates%20Data.htm

# check for files
list.files("CRP Data/pmoney")


# initialize
load(here(str_c("CRP Data/pmoney/pmoney", 0, ".RData")))
d <- pmoney

for(file in list.files("CRP Data/pmoney")){
  load(here(str_c("CRP Data/pmoney/", file)))
  
  d %<>% full_join(pmoney)
  
}

pac_money <- d %<>% as_tibble() 
save(pac_money, file = here("CRP Data/pac_money.RData"))

d

# drop losing candidates
d %<>% filter(!str_detect(RecipCode, "L"))
nrow(d)

# filter to house and senate only
d %>% filter(str_detect(FECCandID, "^H|^S"))
nrow(d)

# select needed variables
d %<>%  select(FECCandID, CID, Cycle, FirstLastP, CycleCand) %>% distinct()
d

# get congress from cycle
d %<>% mutate(congress = (Cycle - 2000)/2 + 107)

# congresses per member
d %<>%   
  group_by(FirstLastP) %>% 
  mutate(congresses = str_c(unique(congress), collapse = ";") ) %>% 
  ungroup() 

# inspect the number of cands per cycle 
d %>% count(Cycle, congress) # note: lots of duplication post 2012, esp 2016
d %>% select(Cycle, congress, FirstLastP) %>% distinct() %>% count(Cycle, congress) 

# many more names than voteview has for the same period
d %>% distinct(FirstLastP)
# unique voteview names 
members %>% filter(congress %in% d$congress) %>% distinct(bioname)

# make sure other susan davis (who lost) was dropped. Only Susan A. Davis should be here.
d %>% filter(str_detect(FirstLastP, "Susan.*Davis"))

##########################
# Match names with ICPSR #
members %<>% filter(chamber %in% c("House", "Senate"))
d %<>% extractMemberName(members = members, col_name = "FirstLastP")


members_crp <- d
save(members_crp, file = "data/members_crp.Rdata")

