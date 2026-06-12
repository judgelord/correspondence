# This script creates counts of letters per year "dcounts" (including zero-counts)
# The key idea is:
#   
#   Count the rows that actually exist.
# Build the desired “skeleton” of agency-year × member-year × TYPE combinations only for agency-years you specify.
# left_join() the real counts onto that skeleton.
# Replace missing counts with zero.

# do this if using previously saved all_contacts.Rdata
testing = F
if(testing){
## load functions 
source(here::here("setup.R"))

## load data 
load(here::here("data", "all_contacts.rda"))
}
# otherwise, this script continues with df from the end of merge2.R

# remove any old counts 
all_contacts %<>% select(-starts_with("per_"))
n_full_timeperiod <- nrow(all_contacts)
n_full_timeperiod

# years to include from data_list
source(here::here("data_list.R"))

# TO INCLUDE ALL YEARS (FOR ILLUSTRATION ONLY, THESE COUNTS WOULD HAVE INCORRECT 0s)
# data_list %<>% mutate(years_to_include = list(2000:2024))

# years to include as specifired in data list
agency_years_long <- data_list %>%
  tidyr::unnest_longer(years_to_include ) %>% 
  tidyr::unnest_longer(years_to_include, values_to = "year") %>%
  mutate(year = as.integer(year))

# factor version of data 
dfac_year <- all_contacts %>%
  mutate(
    year  = as.integer(str_sub(DATE, 1, 4)),
    TYPE  = coalesce(as.character(TYPE), "NA"),
    TYPE  = str_remove(TYPE, ";;;.*")
  ) %>%
  select(data_id, agency, icpsr, chamber, year, TYPE)

# subset data to years from data_list 
dfac_year_included <- dfac_year %>%
  inner_join(
    agency_years_long,
    by = c("agency", "year")
  )

# unique member years from members data - one row per congress x chamber (only chamber switchers have two rows per congres)
member_years <- members %>%
  select(icpsr, congress, chamber) %>%
  distinct() %>%
  filter(congress > 104) %>%
  mutate(
    first_year = (congress - 100) * 2 + 1987
  ) %>%
  tidyr::expand_grid(year_offset = 0:1) %>%
  mutate(
    year = first_year + year_offset,
    icpsryear = str_c(icpsr, chamber, year, sep = "-")
  ) %>%
  select(icpsr, congress, chamber, year, icpsryear) %>%
  distinct()

# counts 
observed_counts_year <- dfac_year_included %>%
  count(
    agency,
    year,
    icpsr,
    chamber,
    TYPE,
    name = "per_icpsr_chamber_year_agency_type"
  )

# types 
type_levels <- dfac_year %>%
  distinct(TYPE)

# years 
dcounts <- agency_years_long %>%
  inner_join(
    member_years,
    by = "year",
    relationship = "many-to-many"
  ) %>%
  tidyr::crossing(type_levels) %>%
  left_join(
    observed_counts_year,
    by = c("agency", "year", "icpsr", "chamber", "TYPE")
  ) %>%
  mutate(
    per_icpsr_chamber_year_agency_type =
      replace_na(per_icpsr_chamber_year_agency_type, 0L),
    agencyyear = str_c(agency, year, sep = "-"),
    icpsryear  = str_c(icpsr, chamber, year, sep = "-")
  ) %>%
  group_by(agency, year) %>%
  mutate(
    per_agency_year = sum(per_icpsr_chamber_year_agency_type, na.rm = TRUE)
  ) %>%
  ungroup()

# Checks
sum(dcounts$per_icpsr_chamber_year_agency_type)
nrow(dfac_year_included)


# OPTIONALLY add more counts
if(F){
  all_contacts$CONSTITUENT_TYPE %<>% str_to_lower()
  
  all_contacts %>% filter(agency == "SSA") %>% 
    count(CONSTITUENT_TYPE, agency) %>% kablebox()
  
  
  all_contacts %>% drop_na(CONSTITUENT_TYPE) %>%  count(agency) |> kable() 
  
  
  vet <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "veteran") | agency == "VA" & TYPE == "1") %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_vet")
  
  vet %>% group_by(TYPE) %>% tally(per_icpsr_chamber_year_agency_vet)
  
  military <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "veteran|military") | agency == "VA" & TYPE == "1") %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_military")
  
  senior <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "senior|medicare|social") | str_detect(SUBJECT, "medicare")) %>% 
    filter(TYPE == 1) %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_senior")
  
  count(senior, agency)
  # filter(all_contacts, agency == "DHHS_CMS") %>% distinct(CONSTITUENT_TYPE)
  
  lowincome <- all_contacts %>% 
    filter(CONSTITUENT_CLASS == 1 | str_detect(CONSTITUENT_TYPE, "medicaid")) %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_lowincome")
  
  hardship <-all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "foreclosure|hardship|debtor|delinquency")) %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_hardship")
  
  immigrant <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "immigra")) %>% 
    count(icpsr, chamber, agency, year, TYPE, name = "per_icpsr_chamber_year_agency_immigrant")
  
  # add constituent counts to base counts
  dcounts %<>%
    left_join(vet) %>% 
    left_join(military) %>% 
    left_join(senior) %>% 
    left_join(lowincome) %>% 
    left_join(hardship) %>% 
    left_join(immigrant) %>% 
    distinct()
  
  # should still be the same minus the ones dropped 
  dcounts$per_icpsr_chamber_year_agency_type |> sum() 
  nrow(all_contacts)
  
  
  # inspect vet count at the VA to confirm merge 
  dcounts %>% filter(agency == "VA") %>% tally(per_icpsr_chamber_year_agency_vet)
  
  senior %>% tally(per_icpsr_chamber_year_agency_senior)
  
  dcounts %>% filter(agency == "DHHS_CMS") %>% tally(per_icpsr_chamber_year_agency_senior)
  dcounts %>% filter(agency == "DHHS_CMS") %>% tally(per_icpsr_chamber_year_agency_type)
  
  # helper function
  replace_na_zero <- . %>% replace_na(0)
  
  # replace NAs with zeros 
  dcounts %<>% mutate(across(starts_with("per_"), replace_na_zero)) #%>% select(starts_with("per")) %>% head() %>% kable()
  
  dcounts %>% filter(agency == "VA") %>% count(per_icpsr_chamber_year_agency_vet)
  
  nrow(dcounts)
  
  # Check 
  # should be the same minus the n dropped 
  dcounts$per_icpsr_chamber_year_agency_type |> sum() 
  nrow(all_contacts)
}

# full yearly count data 
save(dcounts, file = here("data", "dcounts.rda"))

dcounts %<>% 
  distinct(agency, year, icpsr, chamber, TYPE, per_icpsr_chamber_year_agency_type) 

save(dcounts, file = here("data", "dcounts-min.rda"))

