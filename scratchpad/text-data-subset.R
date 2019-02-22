# This script subsets to agencies with ample subject texts
source(here::here("setup.R"))

load(here("data/correspondence.Rdata"))

d <- df %>% select(ID, DATE, year, congress, bioname, state, party_name, department, Department, agency, SUBJECT, TYPE, POLICY_EVENT, complete)

d %<>% filter(complete == T) %>% 
  filter(!is.na(SUBJECT)) %>%
  mutate(nchar = nchar(SUBJECT)) %>% 
  group_by(agency) %>%
  mutate(n = n()) %>% 
  filter(n > 1000) %>% 
  mutate(mean = mean(nchar, na.rm = T)) %>% 
  filter(mean > 50)  %>% 
  ungroup()

d %<>% select(-complete, -nchar, -n, -mean)

# inspect
unique(d$agency)
unique(d$Department)
head(d$SUBJECT[which(d$agency == "DOT_FTA")])

d$Department %<>% str_replace("Department of ", "")
d %<>% mutate(Department = ifelse(department=="DHS","Homeland Security", Department))

correspondenceTexts <- d

save(correspondenceTexts, file =here("data/correspondenceTexts.Rdata"))
