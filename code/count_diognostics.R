
test <- df %>% 
  group_by(icpsr) %>% 
  mutate(per_icpsr_validate = n() ) %>% 
  select(per_icpsr_validate, per_icpsr) %>% 
  distinct() %>% 
  mutate(diff = per_icpsr - per_icpsr_validate)

head(test)

df %>% select(starts_with("per_")) %>% head()

dcounts %>% select(starts_with("per_"), icpsr, agency, TYPE) %>% filter(icpsr == 10713) %>% kable()

# minimal counts 
nrow(dcounts)
dcounts_min <- dcounts %>% select(agency, icpsr, chamber, year, TYPE, per_icpsr_chamber_year_agency_type)
nrow(dcounts_min)
dcounts_min %<>% distinct()
nrow(dcounts_min)

# minimal letters data
df_min <- df %>% 
  ungroup() %>% 
  select(agency, icpsr, chamber, year, TYPE, # THE ONES WE MERGE ON 
         LetterID, ID,
         DATE, FROM, SUBJECT, ALT_TYPE, CERTAINTY, 
         POLICY_EVENT, EVENT_NAME, EVENT_DATE,
         CONSTITUENT_TYPE, CONSTITUENT_CLASS, 
         NOTES, ERROR)

nrow(df_min)
nrow(df)
df_min %<>% distinct() 
nrow(df_min)

# add counts to letters data
df_min %<>% left_join(dcounts_min)
nrow(df_min)
df_min %<>% distinct() 
nrow(df_min)

nrow(df)
df %<>% select(-per_icpsr_chamber_year_agency_type) %>% left_join(dcounts_min)
nrow(df)
df %<>% distinct() 
nrow(df)


# this works, but it has no 0s (it cant)
df_min %>% filter(is.na(per_icpsr_chamber_year_agency_type))

# this does not work? it has no 0s (it cant)
df %>% filter(is.na(per_icpsr_chamber_year_agency_type))



# agency-member-year vars that can't be in members becaues they depend on agency 
agency_vars <- df %>% select(agency, icpsr, chamber, year,
                             timeframe, complete, department, Department, 
                             Reporting.Committees, Number.of.Committees, Committeesconfirmingapps,
                             Employees, Independent.Funding, Rulemaking, 
                             oversight_committee, oversight_committee_chair) %>% distinct()
agency_vars

nrow(dcounts_min)
dcounts_min %<>% left_join(agency_vars)
nrow(dcounts_min)



# make sure member vars are not NA
dcounts_min %>% select(names(df_min)[names(df_min) %in% names(members)])


