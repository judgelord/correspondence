
source("setup.R")

# CRP Recipients
# metadata here: https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20Candidates%20Data.htm
load("data/members_crp.Rdata")
d <- members_crp %>% select(congress, string, pattern, FECCandID, CID, Cycle, FirstLastP) %>% distinct()

members %<>% filter(chamber %in% c("House", "Senate"))

d %<>% left_join(members %>% select(pattern, congress, bioname, icpsr) %>% distinct()) %>% distinct()

# should be no NAs where there is a pattern
d %>% filter(is.na(icpsr) & pattern != "404error")

# corrections (where extracMemberNames matched the wrong person)
# FIXME in issue #1 or #62
d %<>% 
  # mutate(
  #   icpsr = case_when(
  #     CID == "N00033091" ~ 21319, # murphy
  #     # CID == "N00026160" ~ 20505, # price 
  #     # CID == "N00033832" ~ 21359, # rice 
  #     TRUE ~ as.double(icpsr))
  # ) %>% 
  # drop overmatched false positivees
  filter(!(icpsr == 20505 & CID != "N00026160"), # price
         !(icpsr == 21359 & CID != "N00033832"), # rice
         !(icpsr == 20301 & CID != "N00024759"),# mike D rogers
         !(icpsr == 20120 & CID != "N00009668"), # mike rogers 
         !(icpsr == 29585 & CID != "N00012457"), # Jessee Jackson Jr, not sr
         !(icpsr == 20147 & CID != "N00012460"), # William L Clay Jr
         !(icpsr == 29364 & CID != "N00004113"), # Nick Smith 
         !(icpsr == 20963 & CID != "N00029258"), # Duncan D. Hunter (R)
         !(icpsr == 21335 & CID != "N00034044"), # Joe Kennedy III (D)
         !(icpsr == 39304 & CID != "N00005870"), # GREEN, Raymond Eugene (Gene) TX not LA 
         !(icpsr == 20342 & CID != "N00025175"), # Michael R Turner (R) OH not OK
         !(icpsr == 15455 & CID != "N00003209"), # John J Duncan Jr (R) TN not NC 
         !(icpsr == 29714 & CID != "N00002926"), # Jim Davis (D) FL, not Thomas James Davis (D) WA
         !(icpsr == 15063 & CID != "N00007799"), # SMITH, Robert Freeman OR, not HN or PA
         !(icpsr == 14066 & CID != "N00007999"), # YOUNG, Donald Edwin AK, not CA 
         !(icpsr == 29373 & CID != "N00000699") # MENENDEZ, Robert
  )
         
         
# chamber switchers
members %>% add_count(icpsr, bioname, congress, sort = T) %>% 
  filter(n>1) %>% 
  select(icpsr, bioname, congress, chamber)

# add chamber variable
d %<>% mutate(chamber = ifelse(str_detect(FECCandID, "^S"), "Senate", "House"))


#################################
# Crosswalk CRP IDs with ICPSRs # 
crosswalk <- d %>% select(CID, FirstLastP, FECCandID, pattern, congress, Cycle, chamber) %>% 
  filter(pattern != "404error") %>% 
  distinct() %>% 
  left_join(members %>% select(pattern, icpsr, bioname, congress, chamber) %>% distinct()) %>% 
  select(-pattern) %>% 
  distinct() 

# FIXME Why are there NAs
crosswalk %<>% filter(!is.na(icpsr))

# Helper function
str_distinct <- . %>% 
  unique() %>% 
  str_c(sep = ";", collapse = ";")

crosswalk %<>% group_by(icpsr, congress) %>% 
  mutate_all(str_distinct) %>% 
  ungroup() %>% 
  distinct()

# Problems 
crosswalk %>% 
  filter(str_detect(FirstLastP, ";"),
         str_detect(CID, ";"),
         !(str_detect(FirstLastP, "\\(R") & str_detect(FirstLastP, "\\(D")) ) %>% 
  select(CID, FirstLastP, FECCandID, bioname, icpsr) %>%
  distinct() %>% 
  kable()

# problems with overmatched false positives
crosswalk %>% arrange(icpsr) %>% 
  add_count(icpsr, congress, chamber, sort = T) %>% 
  filter(n >1) %>% 
  select(-congress, -Cycle) %>%
  distinct() # %>% 
  # top_n(30) %>%
  # select(CID, FirstLastP, Party, Office, icpsr, bioname) %>%
  #kable()


# missing VOTEVIEW members 
missed <- members %>% filter(!icpsr %in% crosswalk$icpsr, 
                             #congress > 112, # looks like CRP 2012 file is super incomplete, does not cover the 112th or before
                             congress > 106) %>%
  count(icpsr, bioname, last_name,pattern, sort = T)

missed

missed %>% top_n(10) %>%  select(-pattern) %>% kable()

# Joe Lieberman, who served until the 112th, so the error for the 113th is correct
d %>% filter(str_detect(FirstLastP, "Lieberman")) %>% select(congress, string, pattern, CID, FirstLastP)


# last names 
missed_crp <- d %>% 
  filter(pattern == "404error",
         str_detect(string, str_c(tolower(missed$last_name), collapse = "|") ) ) %>% 
  count(FirstLastP, string, congresses, sort = T) 

missed_crp

# to fix 
brian higgans needs middle initial
WHITE, Richard Alan (Rick)?
  Douglas L Lamborn
)

# first names 
missed <- d %>% 
  filter(pattern == "404error",
         str_detect(string, str_c(tolower(members$first_name), collapse = "|") )) %>% 
  count(FirstLastP, string, congresses, sort = T) 

missed  %>% filter(n>1) %>% kable() 




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
