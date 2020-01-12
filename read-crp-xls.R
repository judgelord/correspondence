




###################################
# CRP has sheets for the 13th, 14th, and 15th congresses, and then one called "Candidate IDs - 2012" for prior congresses.
# The first 5 rows of each sheet are header
# The 6th row is names


# 115th 
c115th <- readxl::read_xls(here("CRP Data/CRP_IDs.xls"), sheet = "Members 115th")

names(c115th) <- c115th[5,]

c115th %<>% mutate(congress = 115) %>% select("CID", "CRPName", "Party", "Office", "FECCandID","congress")


# 114th 
c114th <- readxl::read_xls(here("CRP Data/CRP_IDs.xls"), sheet = "Members 114th")

names(c114th) <- c114th[5,]

c114th %<>% mutate(congress = 114) %>% select("CID", "CRPName", "Party", "Office", "FECCandID","congress")


# 113th 
c113th <- readxl::read_xls(here("CRP Data/CRP_IDs.xls"), sheet = "Members 113th")

names(c113th) <- c113th[5,]

c113th %<>% mutate(congress = 113) %>% select("CID", "CRPName", "Party", "Office", "FECCandID","congress")

# prior 
c2012 <- readxl::read_xls(here("CRP Data/CRP_IDs.xls"), sheet = "Candidate IDs - 2012")

names(c2012) <- c2012[5,]

c2012 %<>% rename(Office = DistIDRunFor) %>% select("CID", "CRPName", "Party", "Office", "FECCandID")

c112th <- c2012 %>% mutate(congress = 112) 

c111th <- c2012 %>% mutate(congress = 111)

c110th <- c2012 %>% mutate(congress = 110)

c109th <- c2012 %>% mutate(congress = 109)

c108th <- c2012 %>% mutate(congress = 108)

c107th <- c2012 %>% mutate(congress = 107)

c106th <- c2012 %>% mutate(congress = 106)

c105th <- c2012 %>% mutate(congress = 105)

congresses <- c(c107th, c108th, c109th)

d <- rbind(c105th,
           c106th,
           c107th, 
           c108th, 
           c109th, 
           c110th, 
           c111th, 
           c112th, 
           c113th, 
           c114th, 
           c115th)

crpOriginal <- d


# nonmembers with the same names as members
nonmembers <- c("Davis, Susan")
d %<>% filter(!CRPName %in% nonmembers)




# Match names with ICPSR 
d %<>% extractMemberName(members = members, col_name = "FirstLastP")

missed <- d %>% filter(congress > 112, pattern == "404error", !is.na(CRPName), CRPName != "CRPName") %>% 
  select(CRPName, string) %>% distinct() 

missed %>% kable()


crosswalk <- d %>% select(CID, CRPName, Party, Office, FECCandID, pattern, congress) %>% 
  filter(pattern != "404error") %>% 
  distinct() %>% left_join(members %>% select(pattern, icpsr, bioname, congress) %>% distinct()) %>% 
  select(-pattern) %>% 
  distinct()

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
  # select(CID, CRPName, Party, Office, icpsr, bioname) %>%
  kable()

# missing CRP members 
missed_CRP <- d %>% filter(!CRPName %in% crosswalk$CRPName) %>% select(CID, CRPName, Party, Office) %>% distinct()
missed_CRP$CRPName

# missing VOTEVIEW members 
missed <- members %>% filter(!icpsr %in% crosswalk$icpsr, 
                             chamber %in% c("House", "Senate"),
                             congress > 112, # looks like CRP 2012 file is super incomplete, does not cover the 112th or before
                             congress < 116) %>%
  select(icpsr, bioname, last_name) %>% distinct()
missed$bioname

# For example, there is no Joe Lieberman, who served until the 112th
d %>% filter(str_detect(CRPName, "Lieberman")) %>% select(congress, string, pattern, CID, CRPName)

missed_CRP %>% filter(str_detect(toupper(CRPName), missed$last_name))

crosswalk %>% 
  add_count(CID, congress, sort = T) %>% 
  select(-congress) %>%
  distinct() %>% 
  filter(n>1) %>%
  kable()

# Inspect for false positives
crosswalk %>% 
  filter(str_sub(str_to_upper(bioname), 1,4) != str_sub(str_to_upper(CRPName), 1, 4)) %>% 
  select(-congress) %>% distinct() %>% 
  kable()

missed_CRP <- d %>% 
  filter(!CID %in% crosswalk$CID,
         !CRPName %in% missed$CRPName,
         congress < 113) %>% 
  select(CRPName, Party, Office) %>% 
  distinct()
missed_CRP

