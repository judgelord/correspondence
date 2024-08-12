# This script creates counts of letters per year "dcounts" (including zero-counts)


# do this if using previously saved all_contacts.Rdata
if(F){
## load functions 
source(here::here("setup.R"))

## load data 
load(here::here("data", "all_contacts.Rdata"))
df <- all_contacts
names(df)
}

# otherwise, this script continues with df from the end of merge2.R

# remove any old counts 
df %<>% select(-starts_with("per_"))
nrow(df)
# add letter counts per name and icpsr (party switchers have a new icpsr after they switch)
df %<>% mutate(icpsryear = str_c(icpsr, chamber, year, sep = "-")) %>% 
  mutate(TYPE = replace_na(TYPE, "NA") %>% str_remove(";;;.*")) # FIXME REMOVE DOUBLE CODING 

# count members in the data, including 0s for agencies, years, TYPES where no letter exists
dfac <- df %>% 
  # mutate(across(select(agency, icpsr, TYPE, year, as.factor)) %>% 
  mutate_at( c("agency", "icpsryear", "TYPE"), as.factor)

dcounts <- dfac %>% count(agency, icpsryear, TYPE, .drop = F, name = "per_icpsr_chamber_year_agency_type") 

# inspect
dcounts %>% filter(per_icpsr_chamber_year_agency_type >0)

# unique member year obs
membersyear1 <- members %>% select(icpsr, congress, chamber) %>%
  mutate(year = (congress -100)*2 + 1987) %>% distinct() 
membersyear1 %>% count(congress, year)

membersyear2 <- members %>% select(icpsr, congress, chamber) %>%
  mutate(year = (congress -100)*2 + 1988) %>% distinct() 
membersyear2 %>% count(congress, year)

membersyear <- full_join(membersyear1, membersyear2) %>% distinct() %>% 
  mutate(icpsryear = str_c(icpsr, chamber, year, sep = "-"))

# template grid 
grid <- expand_grid(agency = unique(df$agency), 
                    TYPE = unique(df$TYPE), 
                    icpsryear = unique(membersyear$icpsryear) )

# add base counts 
dcounts %<>% 
  full_join(grid) %>% 
  distinct() 


# helper function
replace_na_zero <- . %>% replace_na(0)

# replace NAs with zeros 
dcounts %<>% mutate(across(starts_with("per_"), replace_na_zero)) #%>% select(starts_with("per")) %>% head() %>% kable()


# should be null
dcounts %>% count(icpsryear, TYPE, agency, sort = T) %>% filter(n != 1)

# target N 
nrow(count(df, agency))*nrow(count(df, TYPE))*nrow(membersyear %>% select(icpsryear) %>% distinct())
# N
nrow(dcounts)

# should be the same as nrow df 
sum(dcounts$per_icpsr_chamber_year_agency_type )
nrow(df)

# add in basic vars 
dcounts %<>% 
  left_join(membersyear) %>% # add in congress
  # split out icpsr
  mutate(icpsr = icpsr %>% as.character() %>% as.numeric(),
         congress = congress %>% as.character() %>% as.numeric()) 

nrow(dcounts)
# now drop years where we have no observations from an agency 
# FIXME use agency-year
dcounts %<>% 
  group_by(year, agency) %>% 
  mutate(per_agency_year = sum(per_icpsr_chamber_year_agency_type )) %>% 
  filter(per_agency_year > 0)
nrow(dcounts)


########################################################################################
# add more counts
df$CONSTITUENT_TYPE %<>% str_to_lower()

nrow(df)

df %>% filter(agency == "SSA") %>% 
  count(CONSTITUENT_TYPE, agency) %>% kablebox()


df %>% drop_na(CONSTITUENT_TYPE) %>%  count(agency)


vet <- df %>% 
  filter(str_detect(CONSTITUENT_TYPE, "veteran") | agency == "VA" & TYPE == "1") %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_vet")

vet %>% group_by(TYPE) %>% tally(per_icpsr_chamber_year_agency_vet)

military <- df %>% 
  filter(str_detect(CONSTITUENT_TYPE, "veteran|military") | agency == "VA" & TYPE == "1") %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_military")

senior <- all_contacts %>% 
  filter(str_detect(CONSTITUENT_TYPE, "senior|medicare|social") | str_detect(SUBJECT, "medicare")) %>% 
  filter(TYPE == 1) %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_senior")

count(senior, agency)
# filter(all_contacts, agency == "DHHS_CMS") %>% distinct(CONSTITUENT_TYPE)

lowincome <- df %>% 
  filter(CONSTITUENT_CLASS == 1 | str_detect(CONSTITUENT_TYPE, "medicaid")) %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_lowincome")

hardship <- df %>% 
  filter(str_detect(CONSTITUENT_TYPE, "foreclosure|hardship|debtor|delinquency")) %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_hardship")

immigrant <- df %>% 
  filter(str_detect(CONSTITUENT_TYPE, "immigra")) %>% 
  count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_immigrant")


# add constituent counts to base counts
dcounts %<>% 
  left_join(vet) %>% 
  left_join(military) %>% 
  left_join(senior) %>% 
  left_join(lowincome) %>% 
  left_join(hardship) %>% 
  left_join(immigrant)
nrow(dcounts)

dcounts %>% filter(agency == "VA") %>% tally(per_icpsr_chamber_year_agency_vet)

senior %>% tally(per_icpsr_chamber_year_agency_senior)

dcounts %>% filter(agency == "DHHS_CMS") %>% tally(per_icpsr_chamber_year_agency_senior)

# helper function
replace_na_zero <- . %>% replace_na(0)

# replace NAs with zeros 
dcounts %<>% mutate(across(starts_with("per_"), replace_na_zero)) #%>% select(starts_with("per")) %>% head() %>% kable()

dcounts %>% filter(agency == "VA") %>% count(per_icpsr_chamber_year_agency_vet)


# should be the same as nrow df 
sum(dcounts$per_icpsr_chamber_year_agency_type )
nrow(df)


# FIXME add other counts here
# # rolled up counts 
# dcounts %<>% 
# group_by(icpsr, year, TYPE, agency) %>% 
# mutate(per_icpsr_year_agency_type = sum(per_icpsr_chamber_year_agency_type) ) %>% 
# ungroup() %>%
# group_by(icpsr, year, agency) %>% 
# mutate(per_icpsr_year_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() %>%
# group_by(icpsr, congress, agency, TYPE) %>% 
# mutate(per_icpsr_congress_agency_type = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() %>%
# group_by(icpsr, congress, agency) %>% 
# mutate(per_icpsr_congress_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() %>%
# group_by(icpsr) %>% 
#  mutate(per_icpsr = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() 

# add members data
nrow(dcounts)
dcounts %<>%
  left_join(members) 
nrow(dcounts)

# dcounts %<>% 
# group_by(bioname, year, agency) %>% 
# mutate(per_bioname_year_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() %>% 
# group_by(bioname, year) %>% 
# mutate(per_bioname_year_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
# ungroup() 

# note chamber switchers 
dcounts %<>% 
  group_by(icpsr, congress, TYPE, agency) %>% 
  mutate(chamber_switcher = n() > 2) %>% 
  ungroup()

dcounts %>% filter(chamber_switcher) %>% select(bioname, chamber, congress) %>% distinct() %>% kable()

# note party switchers
dcounts %<>% 
  group_by(bioname, chamber, congress, TYPE, agency) %>% 
  mutate(party_switcher = n() > 2) %>% 
  ungroup()

dcounts %>% filter(party_switcher) %>% select(bioname, party_name, congress) %>% distinct() %>% arrange(bioname, congress) %>% kable()

unique(dcounts$year)

# agency-member-year vars that can't be in members becaues they depend on agency 
agency_vars <- df %>% select(agency, icpsr, chamber, year,
                             timeframe, complete, department, Department, 
                             Reporting.Committees, Number.of.Committees, Committeesconfirmingapps,
                             Employees, Independent.Funding, Rulemaking, 
                             oversight_committee, oversight_committee_chair) %>% distinct()
agency_vars
save(agency_vars, file = here("data/agency_vars.Rdata"))

nrow(dcounts)
dcounts %<>% left_join(agency_vars)
nrow(dcounts)
dcounts

# full count data 
save(dcounts, file = here("data/dcounts.Rdata"))


# mid-level sized count data 
dcounts %<>% filter(chamber != "President")

dcounts %<>% select(chamber, last_name, state_abbrev, party_name, congress, year, agency, icpsr, TYPE, starts_with("per_")) 

save(dcounts, file = here::here("data" , "dcounts_mid.Rdata"))


# minimal count data prior to merge with members and agency 
dcounts_min <- dcounts %>% select(agency, icpsr, chamber, year, TYPE, 
                                  per_icpsr_chamber_year_agency_vet,
                                  per_icpsr_chamber_year_agency_military,
                                  per_icpsr_chamber_year_agency_lowincome,
                                  per_icpsr_chamber_year_agency_senior,
                                  per_icpsr_chamber_year_agency_hardship,
                                  per_icpsr_chamber_year_agency_immigrant,
                                  per_icpsr_chamber_year_agency_type)
nrow(dcounts_min)
save(dcounts_min, file = here("data/dcounts_min.Rdata"))



df %<>% left_join(dcounts_min)
nrow(df)
all_contacts <- df
save(all_contacts, file = here("data/all_contacts.Rdata"))

# members data, slighty distinct from the members data in the members folder that has name patterns
save(members, file = here("data/members.Rdata"))

