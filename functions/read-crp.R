
source("setup.R")


# metadata here: https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20Candidates%20Data.htm

# check for files
list.files("CRP Data/pmoney")


# initialize
load(here(str_c("CRP Data/pmoney/pmoney", 0, ".RData")))

d <- pmoney %>% mutate(cycle = "2000")

for(file in list.files("CRP Data/pmoney")){
  load(here(str_c("CRP Data/pmoney/", file, ".RData")))
  
  d %<>% full_join(pmoney)
  
}

pac_money <- d
save(pac_money, file = here("CRP Data/pac_money.RData"))

nrow(d)

unique(d$Type)

# subset to corp pacs 
# d %<>% filter(type = "c")

head(d) 

# drop losing candidates
d %<>% filter(!str_detect(RecipCode, "L"))

# filter to house and senate only
d %>% filter(str_detect(FECCandID, "^H|^S"))

d %<>% as_tibble() %>% select(FECCandID, CID, Cycle, FirstLastP, CycleCand) %>% distinct()

# get congress from cycle
d %<>% mutate(congress = (Cycle - 2000)/2 + 107)

# inspect the number of cands per cycle 
d %>% count(Cycle, congress)
d %>% select(Cycle, congress, FirstLastP) %>% distinct() %>% count(Cycle, congress)
d %>% distinct(FirstLastP)

# make sure other susan davis was dropped
d %>% filter(str_detect(FirstLastP, "Susan.*Davis"))

# Match names with ICPSR 
d %<>% extractMemberName(members = members, col_name = "FirstLastP")

d %<>%   
  group_by(FirstLastP) %>% 
  mutate(congresses = str_c(unique(congress), collapse = ";") ) %>% 
  ungroup() 

# Names not matched
missed <- d %>% 
  filter(pattern == "404error") %>% 
  count(FirstLastP, string, congresses, sort = T) 

# top
missed  %>% top_n(50) %>% kable() 

missed <- d %>% 
  filter(pattern == "404error",
         str_detect(string, str_c(tolower(members$last_name), collapse = "|") ) ) %>% 
  count(FirstLastP, string, congresses, sort = T) 

# last names 
missed  %>% kable() 

missed <- d %>% 
  filter(pattern == "404error",
         str_detect(string, str_c(tolower(members$first_name), collapse = "|") )) %>% 
  count(FirstLastP, string, congresses, sort = T) 

# first names 
missed  %>% filter(n>1) %>% kable() 


crosswalk <- d %>% select(CID, FirstLastP, FECCandID, pattern, congress, congresses, Cycle) %>% 
  filter(pattern != "404error") %>% 
  distinct() %>% left_join(members %>% select(pattern, icpsr, bioname, congress) %>% distinct()) %>% 
  select(-pattern) %>% 
  distinct()



# FIXME in issue #1 or #62
# corrections
crosswalk %<>% 
  mutate(
    icpsr = case_when(
      CID == "N00033091" ~ 21319, # murphy
      # CID == "N00026160" ~ 20505, # price 
      # CID == "N00033832" ~ 21359, # rice 
      TRUE ~ as.double(icpsr))
    ) %>% 
  # drop overmatched
  filter(!(icpsr == 20505 & CID != "N00026160"), # price
         !(icpsr == 21359 & CID != "N00033832"), # rice
         !(icpsr == 20301 & CID != "N00024759"),# rogers
         !(icpsr == 20120 & CID != "N00009668")) # rogers 

# remaining problems
crosswalk %>% arrange(icpsr) %>% 
  add_count(icpsr, congress, sort = T) %>% 
  select(-congress) %>%
  distinct() %>% 
  filter(n>1) %>%
  # select(CID, FirstLastP, Party, Office, icpsr, bioname) %>%
  kable()

# missing CRP members 
missed_CRP <- d %>% filter(!FirstLastP %in% crosswalk$FirstLastP) %>% count(CID, FirstLastP, sort = T) 
missed_CRP

# missing VOTEVIEW members 
missed <- members %>% filter(!icpsr %in% crosswalk$icpsr, 
                             chamber %in% c("House", "Senate"),
                             #congress > 112, # looks like CRP 2012 file is super incomplete, does not cover the 112th or before
                             congress < 116) %>%
  select(icpsr, bioname, last_name, congresses) %>% distinct()
missed 

# Joe Lieberman, who served until the 112th, so the error for the 113th is correct
d %>% filter(str_detect(FirstLastP, "Lieberman")) %>% select(congress, string, pattern, CID, FirstLastP)

# duplicates 
crosswalk %>% 
  add_count(CID, congress, sort = T) %>% 
  select(-congress) %>%
  distinct() %>% 
  arrange(FirstLastP) %>% 
  filter(n>1) %>%
  kable()

# Inspect for false positives
crosswalk %>% 
  filter(str_sub(str_to_upper(bioname), 1,4) != str_sub(str_to_upper(FirstLastP), 1, 4)) %>% 
  select(-congress) %>% distinct() %>% 
  kable()

missed_CRP <- d %>% 
  filter(!CID %in% crosswalk$CID,
         !FirstLastP %in% missed$FirstLastP,
         congress < 113) %>% 
  select(FirstLastP, Party, Office) %>% 
  distinct()
missed_CRP
