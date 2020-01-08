
source("setup.R")


# CRP has sheets for the 13th, 14th, and 15th congresses, and then one called "Candidate IDs - 2012" for prior congresses.
# The first 5 rows of each sheet are header
# The 6th row is names


# 113th 
c115th <- readxl::read_xls(here("CRP Data/CRP_IDs.xls"), sheet = "Members 115th")

names(c115th) <- c115th[5,]

c115th %<>% mutate(congress = 115) %>% select("CID", "CRPName", "Party", "Office", "FECCandID","congress")


# 113th 
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

d %<>% extractMemberName(members = members, col_name = "CRPName")

missed <- d %>% filter(congress > 112, pattern == "404error", !is.na(CRPName), CRPName != "CRPName") %>% 
  select(CRPName, string) %>% distinct() 

missed %>% kable()


crosswalk <- d %>% select(CID, CRPName, Party, Office, FECCandID, pattern, congress) %>% 
  filter(pattern != "404error") %>% 
  distinct() %>% full_join(members %>% select(pattern, icpsr, bioname, congress) %>% distinct()) %>% 
  select(-pattern) %>% 
  distinct()

crosswalk %>% arrange(icpsr) %>% add_count(icpsr, sort = T) #%>% select(CID, CRPName, bioname)

members %>% filter(!icpsr %in% crosswalk$icpsr) 

missed_CRP <- d %>% 
  filter(!CID %in% crosswalk$CID,
         !CRPName %in% missed$CRPName,
         congress < 113) %>% 
  select(CRPName, Party, Office) %>% 
  distinct()
missed_CRP
