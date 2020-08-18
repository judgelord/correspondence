source(here::here("setup.R"))

load(here::here("data/dcounts_min.Rdata"))
names(dcounts_min)
load(here::here("data/dcounts.Rdata"))
names(dcounts)


template <- dcounts_min %>% select(agency, icpsr, year, chamber) %>% distinct()

# load members data for count data frames 
load(here::here("data/members.Rdata"))

load(here::here("data/all_contacts.Rdata"))



#FIXME update constituent type and class codes 
# source("functions/constituent_types.R")

# inspect 
constituent_coding <- all_contacts %>% 
  ungroup() %>% 
  filter(!is.na(CONSTITUENT_TYPE)) %>% 
  select(agency, SUBJECT, #TYPE, 
         CONSTITUENT_TYPE, NOTES, ERROR) %>% 
  mutate(CONSTITUENT_TYPE = CONSTITUENT_TYPE %>% 
           str_split(";|,")) %>% #fails to drop duplicates, eg "veteran, veteran, veteran" 
  unnest(CONSTITUENT_TYPE)  %>% 
  mutate(CONSTITUENT_TYPE = str_squish(CONSTITUENT_TYPE) %>% 
           str_to_lower()) 
  
constituent_coding$CONSTITUENT_TYPE %>% head() 
# codes
constituent_coding %>% count(CONSTITUENT_TYPE, sort = T)  %>% kable()

constituent_coding %<>% mutate()

# should not have duplicate ids
all_contacts %>% count(ID, agency) %>% filter(n>1) %>% count(agency)
# if there are, they should be here too
all_contacts %>% count(LetterID, ID, DATE, agency, SUBJECT, icpsr, chamber)%>% filter(n>1) %>% count(agency)
all_contacts %>% count(LetterID, ID, DATE, agency, SUBJECT, icpsr, chamber, bioname)%>% filter(n>1) %>% count(bioname)
