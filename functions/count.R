
# FOR TESTING 
# df <- d %>% filter(!is.na(icpsr))
# df$TYPE = c(1,2,3,4,5,6,1,2,3,4,5) #FIXME TEMP

# BEGIN 
# add letter counts per name and icpsr (party switchers have a new icpsr after they switch)
df %<>% mutate(icpsryear = str_c(icpsr, chamber, year, sep = "-")) %>% 
  mutate(TYPE = replace_na(TYPE, "NA") %>% str_remove(";;;.*"))

# count members in the data, including 0s for agencies, years, TYPES where no letter exists
dfac <- df %>% 
  #mutate(across(select(agency, icpsr, TYPE, year, as.factor)) %>% 
  mutate_at( c("agency", "icpsryear", "TYPE"), as.character())
  
dcounts <- dfac %>% count(agency, icpsryear, TYPE, .drop = F, name = "per_icpsr_chamber_year_agency_type ") 

# inspect
dcounts %>% filter(per_icpsr_chamber_year_agency_type >0)

# unique member year obs
membersyear1 <- members %>% select(icpsr, congress, chamber) %>%
  mutate(year = (congress -100)*2 + 1987) %>% distinct() 
membersyear1 %>% count(congress, year)

membersyear2 <- members %>% select(icpsr, congress, chamber) %>%
  mutate(year = (congress -100)*2 + 1988)  %>% distinct() 
membersyear2 %>% count(congress, year)

membersyear <- full_join(membersyear1, membersyear2) %>% distinct() %>% 
  mutate(icpsryear = str_c(icpsr, chamber, year, sep = "-"))

# template grid 
grid <- expand_grid(agency = unique(df$agency), 
            TYPE = unique(df$TYPE), 
            icpsryear = unique(membersyear$icpsryear) )

# add counts 
dcounts %<>% 
  full_join(grid) %>% 
  distinct() 

# ramaining NAs are 0 observations (except where we have no data, see below)
dcounts %<>% mutate(per_icpsr_chamber_year_agency_type  = replace_na(per_icpsr_chamber_year_agency_type , 0))

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
dcounts %<>% 
  group_by(year, agency) %>% 
  mutate(per_agency_year = sum(per_icpsr_chamber_year_agency_type )) %>% #count(per_agency_year, agency)
  filter(per_agency_year > 0)
nrow(dcounts)


# rolled up counts 
dcounts %<>% 
  group_by(icpsr, year, TYPE, agency) %>% 
  mutate(per_icpsr_year_agency_type = sum(per_icpsr_chamber_year_agency_type) ) %>% 
  ungroup() %>%
  group_by(icpsr, year, agency) %>% 
  mutate(per_icpsr_year_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
  ungroup() %>%
  group_by(icpsr, congress, agency, TYPE) %>% 
  mutate(per_icpsr_congress_agency_type = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
  ungroup() %>%
  group_by(icpsr, congress, agency) %>% 
  mutate(per_icpsr_congress_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
  ungroup() %>%
  group_by(icpsr) %>% 
  mutate(per_icpsr = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
  ungroup() %>% 
  group_by(bioname, year, agency) %>% 
  mutate(per_bioname_year_agency = sum(per_icpsr_chamber_year_agency_type ) ) %>% 
  ungroup() 

# add members data
dcounts %<>%
  left_join(members) 
nrow(dcounts)

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

dcounts %>% filter(party_switcher) %>% select(bioname, party, congress) %>% distinct() %>% arrange(bioname, congress) %>% kable()

dcounts %<>% mutate(year = as.numeric(year))

save(dcounts, file =  "data/dcounts.Rdata")

