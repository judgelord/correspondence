

df <- d %>% filter(!is.na(icpsr))

df$TYPE = c(1,2,3,4,5,6,1,2,3,4,5) #FIXME TEMP

# BEGIN 
# add letter counts per name and icpsr (party switchers have a new icpsr after they switch)
df %<>% mutate(icpsryear = str_c(icpsr, year, sep = "-"))

# count members in the data, including 0s for agencies, years, TYPES where no letter exists
df %<>% 
  #mutate(across(select(agency, icpsr, TYPE, year, as.factor)) %>% 
  mutate_at( c("agency", "icpsryear", "TYPE"), as.factor)
  
dcounts <- df %>% count(agency, icpsryear, TYPE, .drop = F, name = "per_icpsr_year_type") 

# inspect
dcounts %>% filter(n>0)

# unique member year obs
membersyear1 <- members %>% select(icpsr, congress) %>%
  mutate(year = (congress -100)*2 + 1987) %>% 
  mutate_all(as.factor) %>% distinct() 
membersyear1 %>% count(congress, year)

membersyear2 <- members %>% select(icpsr, congress) %>%
  mutate(year = (congress -100)*2 + 1988) %>% 
  mutate_all(as.factor) %>% distinct() 
membersyear2 %>% count(congress, year)

membersyear <- full_join(membersyear1, membersyear2) %>% distinct() %>% 
  mutate(icpsryear = str_c(icpsr, year, sep = "-"))

# template grid 
grid <- expand_grid(agency = unique(df$agency), 
            TYPE = unique(df$TYPE), 
            icpsryear = unique(membersyear$icpsryear) )

# add counts 
dcounts %<>% 
  full_join(grid) %>% 
  distinct() 

# should be null
dcounts %>% count(icpsryear, TYPE, sort = T) %>% filter(n != 1)




# target N 
nrow(count(df, agency))*nrow(count(df, TYPE))*nrow(membersyear %>% select(icpsryear) %>% distinct())
# N
nrow(dcounts)


dcounts %<>% 
  left_join(membersyear) %>% # add in congress
  # split out icpsr
  mutate(icpsr = icpsr %>% as.character() %>% as.numeric(),
         congress = congress %>% as.character() %>% as.numeric()) 

# rolled up counts 
dcounts %<>% 
  group_by(icpsr, year) %>% mutate(per_icpsr_year = sum(per_icpsr_year_type) ) %>% ungroup() %>%
  group_by(icpsr, congress) %>% mutate(per_icpsr_congress = sum(per_icpsr_year_type) ) %>% ungroup() %>%
  group_by(icpsr) %>% mutate(per_icpsr = sum(per_icpsr_year_type) ) %>% ungroup() 



# DUPLICATES CHAMBER SWITCHERS
dcounts %<>%
  left_join(members) 

dcounts %<>% 
  group_by(icpsr, congress, TYPE, agency) %>% 
  mutate(chamber_switcher = n() > 2) %>% 
  ungroup()

dcounts %>% filter(chamber_switcher) %>% select(bioname, chamber, congress) %>% distinct() %>% kable()


