load("gh-pages/correspondence.RData") # load data (df is d + covariates + dropping obs not matching an ICPSR)


requires <- c("tidyverse", "knitr","ineq", "dplyr", "ggplot2", "magrittr", "stargazer", "maps", "fiftystater", "mapproj")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)
library(stargazer)

zeros <- rbind(
  members %>% mutate(year = ((congress - 115)*2 + 2017)), # year 1 of term
  members %>% mutate(year = ((congress - 115)*2 + 2018)) ) # year 2
                     
zeros <- rbind(
  zeros %>% mutate(Type = "Indiv. Constituent"),
  zeros %>% mutate(Type = "Corp. Constituent"),
  zeros %>% mutate(Type = "501c3 or Local Gov."),
  zeros %>% mutate(Type = "Corp. Policy"),
  zeros %>% mutate(Type = "Policy")
)

# member year 
zeros %<>% mutate(icpsr_year_type = paste(icpsr, year, Type))


# zero per member year
zeros2 <- data_frame(
  icpsr_year_type = rep(unique(zeros$icpsr_year_type), n_distinct(df$agency)),
  agency =  rep(unique(df$agency), n_distinct(zeros$icpsr_year_type))) %>%
  mutate(icpsr_year_type_agency = paste(icpsr, congress)) 



# df %<>% 
#   group_by(agency, icpsr, year) %>% mutate(Overall = n()) %>% ungroup() %>% 
#   group_by(agency, icpsr, year, Type2) %>% 
#   mutate(Policy = ifelse(Type2 == "Policy", n(), NA)) %>%
#   mutate(Constituent = ifelse(Type2 == "Constituent Service", n(), NA)) %>% 
#   ungroup()

all <- df
p2 <- filter(df, Type2 == "Policy")
c2 <- filter(df, Type2 == "Constituent Service")


prestige_all <- lm(n ~ prestige + factor(congress) + factor(agency), data = all %>%
                 group_by(agency, year, congress, icpsr, prestige) %>%
                 summarise(n = n())  %>% filter(congress != 115) %>% distinct() )
summary(prestige_all)$coefficients[2,]


chair_all <- lm(Overall ~ chair + factor(congress) + factor(agency), data = all %>%
                 group_by(agency, year, congress, icpsr,  chair) %>%
                 summarise(Overall = n())  %>% filter(congress != 115) %>% distinct() )
summary(chair_all)$coefficients[2,]

prestige_chair_all <- lm(Overall ~ chair + factor(congress) + factor(agency), data = all %>%
                 group_by(agency, year, congress, icpsr,  chair) %>%
                 summarise(Overall = n())  %>% filter(congress != 115) %>% distinct() )
summary(chairall)$coefficients[2,]
chair_all[[1]]

matrix(
  names(coef(chair_all)[2]), coef(chair_all)[2]

stargazer(prestige_all, chair_all, prestige_chair_all, 
          coef = c(2),
          title="Core Model: Contacts per Year per Legislator", dep.var.labels = "Contacts per Year", 
          # column.labels=c("Committee Chair","Prestige Commitee", "Prestige Chair", "Party Leader"), 
          align=TRUE, keep.stat = "n") 
