# This script creates counts of letters per month "dcounts" (including zero-counts)
# The key idea is:
#   
#   Count the rows that actually exist.
# Build the desired “skeleton” of agency-month × member-month × TYPE combinations only for agency-years you specify.
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

# years to include as specified in data list
agency_years_long <- data_list %>%
  tidyr::unnest_longer(years_to_include ) %>% 
  tidyr::unnest_longer(years_to_include, values_to = "year") %>%
  mutate(year = as.integer(year))

# Agency-months to include
agency_months <- agency_years_long %>%
  tidyr::expand_grid(month_num = 1:12) %>%
  mutate(
    month = sprintf("%04d-%02d", year, month_num)
  ) %>%
  select(agency, year, month)

# Clean all_contacts
dfac <- all_contacts %>%
  mutate(
    month = str_sub(DATE, 1, 7),
    year  = as.integer(str_sub(DATE, 1, 4)),
    TYPE  = coalesce(as.character(TYPE), "NA"),
    TYPE  = str_remove(TYPE, ";;;.*")
  ) %>%
  select(data_id, agency, icpsr, chamber, year, month, TYPE)

# Keep only agency-years/months that should be included
dfac_included <- dfac %>%
  inner_join(
    agency_months,
    by = c("agency", "year", "month")
  )

# Member-month panel from members
member_months <- members %>%
  select(icpsr, congress, chamber) %>%
  distinct() %>%
  filter(congress > 104) %>%
  mutate(
    first_year = (congress - 100) * 2 + 1987
  ) %>%
  tidyr::expand_grid(
    year_offset = 0:1,
    month_num = 1:12
  ) %>%
  mutate(
    year = first_year + year_offset,
    month = sprintf("%04d-%02d", year, month_num),
    icpsrmonth = str_c(icpsr, chamber, month, sep = "-"),
    icpsryear  = str_c(icpsr, chamber, year, sep = "-")
  ) %>%
  select(icpsr, congress, chamber, year, month, icpsryear, icpsrmonth) %>%
  distinct() %>%
  semi_join(
    agency_months %>% distinct(year, month),
    by = c("year", "month")
  )

# Observed monthly counts
observed_counts <- dfac_included %>%
  count(
    agency,
    year,
    month,
    icpsr,
    chamber,
    TYPE,
    name = "per_icpsr_chamber_month_agency_type"
  )

# TYPE levels
type_levels <- dfac %>%
  distinct(TYPE)

# Create zero-filled monthly counts
dcounts <- agency_months %>%
  inner_join(
    member_months,
    by = c("year", "month"),
    relationship = "many-to-many"
  ) %>%
  tidyr::crossing(type_levels) %>%
  left_join(
    observed_counts,
    by = c("agency", "year", "month", "icpsr", "chamber", "TYPE")
  ) %>%
  mutate(
    per_icpsr_chamber_month_agency_type =
      replace_na(per_icpsr_chamber_month_agency_type, 0L),
    agencymonth = str_c(agency, month, sep = "-"),
    agencyyear  = str_c(agency, year, sep = "-"),
    icpsrmonth  = str_c(icpsr, chamber, month, sep = "-"),
    icpsryear   = str_c(icpsr, chamber, year, sep = "-")
  ) %>%
  group_by(agency, year) %>%
  mutate(
    per_agency_year = sum(per_icpsr_chamber_month_agency_type, na.rm = TRUE)
  ) %>%
  ungroup()

#Checks:
if(testing){
  
sum(dcounts$per_icpsr_chamber_month_agency_type)
nrow(dfac_included)
#Those should match if every observed contact maps to a valid member-month in members.

# To diagnose unmatched contacts:

  unmatched_contacts <- dfac_included %>%
  anti_join(
    member_months,
    by = c("icpsr", "chamber", "year", "month")
  )

nrow(unmatched_contacts)

unmatched_contacts %>%
  count(year, month, chamber, sort = TRUE)
}

# OPTIONALLY add more counts
if(F){
  all_contacts$CONSTITUENT_TYPE %<>% str_to_lower()
  
  all_contacts %>% filter(agency == "SSA") %>% 
    count(CONSTITUENT_TYPE, agency) %>% kablebox()
  
  
  all_contacts %>% drop_na(CONSTITUENT_TYPE) %>%  count(agency) |> kable() 
  
  
  vet <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "veteran") | agency == "VA" & TYPE == "1") %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_vet")
  
  vet %>% group_by(TYPE) %>% tally(per_icpsr_chamber_month_agency_vet)
  
  military <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "veteran|military") | agency == "VA" & TYPE == "1") %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_military")
  
  senior <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "senior|medicare|social") | str_detect(SUBJECT, "medicare")) %>% 
    filter(TYPE == 1) %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_senior")
  
  count(senior, agency)
  # filter(all_contacts, agency == "DHHS_CMS") %>% distinct(CONSTITUENT_TYPE)
  
  lowincome <- all_contacts %>% 
    filter(CONSTITUENT_CLASS == 1 | str_detect(CONSTITUENT_TYPE, "medicaid")) %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_lowincome")
  
  hardship <-all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "foreclosure|hardship|debtor|delinquency")) %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_hardship")
  
  immigrant <- all_contacts %>% 
    filter(str_detect(CONSTITUENT_TYPE, "immigra")) %>% 
    count(icpsr, chamber, agency, month, TYPE, name = "per_icpsr_chamber_month_agency_immigrant")
  
  # add constituent counts to base counts
  dcounts %<>%
    left_join(vet) %>% 
    left_join(military) %>% 
    left_join(senior) %>% 
    left_join(lowincome) %>% 
    left_join(hardship) %>% 
    left_join(immigrant) %>% 
    distinct()
  
  # should still be the same 
  dcounts$per_icpsr_chamber_month_agency_type |> sum() 
  nrow(all_contacts)
  
  
  # inspect vet count at the VA to confirm merge 
  dcounts %>% filter(agency == "VA") %>% tally(per_icpsr_chamber_month_agency_vet)
  
  senior %>% tally(per_icpsr_chamber_month_agency_senior)
  
  dcounts %>% filter(agency == "DHHS_CMS") %>% tally(per_icpsr_chamber_month_agency_senior)
  dcounts %>% filter(agency == "DHHS_CMS") %>% tally(per_icpsr_chamber_month_agency_type)
  
  # helper function
  replace_na_zero <- . %>% replace_na(0)
  
  # replace NAs with zeros 
  dcounts %<>% mutate(across(starts_with("per_"), replace_na_zero)) #%>% select(starts_with("per")) %>% head() %>% kable()
  
  dcounts %>% filter(agency == "VA") %>% count(per_icpsr_chamber_month_agency_vet)
  
  nrow(dcounts)
  
  # Check 
  # should be the same minus the n dropped 
  dcounts$per_icpsr_chamber_month_agency_type |> sum() 
  nrow(all_contacts)
}

dcounts_month <- dcounts

# full monthly count data 
save(dcounts_month, file = here("data", "dcounts_month.rda"))

# subset to minimal vars 
dcounts_month %<>% 
  distinct(agency, month, icpsr, chamber, TYPE, per_icpsr_chamber_month_agency_type) 

save(dcounts_month, file = here("data", "dcounts_month-min.rda"))

if(F){
 # annual version from this monthly dataframe rather than in counts.R
    dcounts_year <- dcounts %>%
      group_by(agency, year, icpsr, chamber, TYPE) %>%
      summarise(
        per_icpsr_chamber_year_agency_type =
          sum(per_icpsr_chamber_month_agency_type, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        agencyyear = str_c(agency, year, sep = "-"),
        icpsryear  = str_c(icpsr, chamber, year, sep = "-")
      ) %>%
      group_by(agency, year) %>%
      mutate(
        per_agency_year = sum(per_icpsr_chamber_year_agency_type, na.rm = TRUE)
      ) %>%
      ungroup()

# full yearly count data 
save(dcounts_year, file = here("data", "dcounts_year.rda"))

dcounts_year %<>% 
  distinct(agency, year, icpsr, chamber, TYPE, per_icpsr_chamber_year_agency_type) 

save(dcounts_year, file = here("data", "dcounts_year-min.rda"))
}