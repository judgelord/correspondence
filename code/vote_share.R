source("setup.R")


library(haven)

earlymoneydata <- read_dta(here::here("data", "earlymoneydata_primary.dta"))

emd <- select(earlymoneydata, year, state_abbrev = state, district_code = district, rep, 
              #special, blanket, safe, competitive, hopeless, dpres, 
              PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575)


emd %<>% mutate(year_) left_join(memebers)