
source("setup.R")

# CRP Recipients
# metadata here: https://www.opensecrets.org/resources/datadictionary/Data%20Dictionary%20Candidates%20Data.htm
load("data/members_crp.Rdata")
d <- members_crp %>% select(congress, pattern, FECCandID, CID, Cycle, FirstLastP) %>% distinct()

load("data/members_crp_xls.Rdata")
d %<>% full_join(members_crp_xls %>% 
                   select(congress, pattern, FECCandID, CID, CRPName) %>% 
                   distinct() )
d

members %<>% filter(chamber %in% c("House", "Senate"))

d %<>% left_join(members %>% select(pattern, congress, bioname, icpsr) %>% distinct()) %>% distinct()

# should be no NAs where there is a pattern
d %>% filter(is.na(icpsr) & pattern != "404error")

# corrections (where extracMemberNames matched the wrong person)
# much of this is redundant after matching on party and state, but worth fixing overmatching in extractMemberNames
# FIXME in issue #1 or #62
d %<>% 
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
         !(icpsr == 15116 & CID != "N00008957"), # SMITH, Robert C.
         !(icpsr == 14066 & CID != "N00007999"), # YOUNG, Donald Edwin AK, not CA 
         !(icpsr == 29373 & CID != "N00000699"), # MENENDEZ, Robert
         !(icpsr == 15431 & CID != "N00002577"), # John R Lewis (R) 
         !(icpsr == 15116 & CID != "N00008957"), # SMITH, Robert C. (R)
         !(icpsr == 21364 & CID != "N00030602"), # WILLIAMS, Roger
         !(icpsr == 20346 & CID != "N00024992"), # MURPHY, Timothy
         !(icpsr == 21720 & CID != "N00039330"), # GALLAGHER, Michael  
         !(icpsr == 20147 & CID != "N00012460"), # CLAY, William Lacy, Jr.
         !(icpsr == 31103 & CID != "N00034639"), # 31103 PAYNE, Donald, Jr.
         !(icpsr == 21335 & CID != "N00034044"), # 21335 KENNEDY, Joseph P. III
         !(icpsr == 29756 & CID != "N00003218"), # 29756 FORD, Harold, Jr.
         !(CID == "N00000716" & icpsr != 15619) # PAYNE, Donald Milford, not Jr
  )
         

members %>% add_count(icpsr, bioname, congress, sort = T) %>% 
  filter(n>1) %>% 
  select(icpsr, bioname, congress, chamber)

# add chamber from FEC ID
d %<>% mutate(chamber = ifelse(str_detect(FECCandID, "^S"), "Senate", "House"))

# add state from FEC ID
d %<>% mutate(state_abbrev = substr(FECCandID, 3,4))

# add party from CRP 
d %<>% mutate(party = str_sub(FirstLastP,-2,-2))

members %<>% mutate(party = str_sub(party_name,1,1)) 

#################################
# Crosswalk CRP IDs with ICPSRs # 
crosswalk <- d %>% 
  select(CID, FirstLastP, CRPName, FECCandID, pattern, congress, Cycle, chamber, state_abbrev, party) %>% 
  filter(pattern != "404error") %>% 
  distinct() %>% 
  left_join(members %>% 
              select(pattern, icpsr, bioname, congress, chamber, state_abbrev, first_name, last_name, party) %>% 
              distinct() 
            ) %>% 
  filter(!(CID == "N00000716" & icpsr != 15619) ) %>% # PAYNE, Donald Milford, not Jr
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

# Save 
icpsr_crosswalk_crp <- crosswalk
save(icpsr_crosswalk_crp, file = "data/icpsr_crosswalk_crp.Rdata")

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




# Joe Lieberman served until the 112th, so the error for the 113th is correct
d %>% filter(str_detect(FirstLastP, "Lieberman")) %>% select(congress, CID, FirstLastP, bioname)

# duplicates? 
crosswalk %>% 
  add_count(CID, congress, chamber, sort = T) %>% 
  select(-congress) %>%
  distinct() %>% 
  arrange(FirstLastP) %>% 
  filter(n>1) %>%
  kable()

# Inspect for false positives - these all look correct
crosswalk %>% 
  filter(str_sub(str_to_upper(first_name), 1,3) != str_sub(str_to_upper(FirstLastP), 1, 3)) %>% 
  select(-congress, -Cycle, -FECCandID) %>% distinct() %>% 
  select(first_name, last_name, FirstLastP, bioname, party) %>% 
  #top_n(30) %>% 
  kable()


# Remaining duplicate CIDs? (merging on party and state fixed this)
crosswalk %>% 
  filter(str_detect(CID, ";")) %>% 
  select(CID, FirstLastP, FECCandID, state_abbrev, CRPName, chamber, icpsr, bioname) %>% 
  distinct()



# FIXME 
# corrected in nameCongres.R file Allen, McCotter, Hastert, Sutton, GutKnecht, Ford, Douglas L Lamborn, brian M higgans middle initial, 
# corrected in MemberNameTypos.R: "Kit" Bond, (Chip) Pickering, (Butch) Otter, 

# missing from CRP: 
# SEKULA GIBBS, Shelley for the 109th where she served only a few weeks. She won the special election to fill the 22nd Congressional seat on November 7, 2006, for the remaining weeks of the 109th United States Congress. On the same day, she also lost in the general election for that seat in the 110th United States Congress.[3] 

# Donald J. Cazayoux (D) did not serve in the 112th 
# MURPHY, Scott only served in the 11th
# DJOU, Charles is not in the 116th
# RENZI did not serve in the 111, 112, 113th
# Doug Ose is not in the 109th or 115th

# VOTEVIEW members still missing from crosswalk
missed <- members %>% filter(!icpsr %in% crosswalk$icpsr, 
                             !bioname %in% crosswalk$bioname,
                             #congress > 112, # looks like CRP 2012 file is super incomplete, does not cover the 112th or before
                             congress > 106) %>%
  count(icpsr, bioname, last_name, pattern, sort = T)

missed %>% select(-pattern) %>% kable()

