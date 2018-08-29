load("gh-pages/correspondence.RData") # load data (df is d + covariates + dropping obs not matching an ICPSR)
load("gh-pages/zeros.RData") # load zero count [icpsr year TYPE agency] observations



requires <- c( "dplyr", "stargazer", "magrittr", "dotwhisker")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )

library(magrittr)
library(dotwhisker)
library(dplyr)
library(stargazer)

# Core Model


all <- df %>% filter(year < 2018) %>% 
  mutate(icpsr_year_type_agency = paste(icpsr, year, TYPE, agency))
zeros %>% filter(year > 2006, year < 2018, icpsr_year_type_agency %in% all$icpsr_year_type_agency)
p2 <- filter(all, Type2 == "Policy") 
c2 <- filter(all, Type2 == "Constituent Service")


# prestige committee
Overall <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
                    full_join()
  filter(term == "prestige")  %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = p2 %>%
                    group_by(agency, year, congress, icpsr, prestige) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = c2 %>%
                         group_by(agency, year, congress, icpsr, prestige) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Constituent")

prestige <- rbind(Overall, Policy, Constituent)



# chair 
Overall <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair")  %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = p2 %>%
                    group_by(agency, year, congress, icpsr, chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = c2 %>%
                         group_by(agency, year, congress, icpsr, chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "Constituent")

chair <- rbind(Overall, Policy, Constituent)


# prestige_chair
Overall <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige_chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair")  %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = p2 %>%
                    group_by(agency, year, congress, icpsr, prestige_chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = c2 %>%
                         group_by(agency, year, congress, icpsr, prestige_chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "Constituent")

prestige_chair <- rbind(Overall, Policy, Constituent)
prestige_chair

# majority*presidents_party
Overall <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency), 
                   data = all %>%
                     group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") ) %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency),
                  data = p2 %>%
                    group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency),
                       data = c2 %>%
                         group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "Constituent")

majority <- rbind(Overall, Policy, Constituent)
majority$term <- gsub(":", "", majority$term)
majority 




# combine 
m <- rbind(prestige, chair, prestige_chair, majority)
b <- list(c("Model 1", "Prestige Committee", "Prestige Committee", "Prestige Committee"), 
          c("Model 2", "Chair", "Chair","Chair"),
          c("Model 3", "Prestige Chair", "Prestige Chair","Prestige Chair"),
          c("Model 4", "Majority", "Presidents Party", "Majority x Presidents Party"))




{dwplot(m) %>% 
    relabel_predictors(c(prestige = "Prestige Committee",
                         chair = "Chair",
                         prestige_chair = "Prestige Chair",
                         majority = "Majority",
                         presidents_party = "Presidents Party",
                         majoritypresidents_party = "Majority x Presidents Party")) +
    theme_bw() + xlab("Estimated Additional Letters per Year (Congress and Agency FE)") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Cross Sectional Differences in Letter Writing") +  
    theme(legend.title = element_blank())} %>%
  add_brackets(b)



# prestige committee
Overall <- tidy(lm(n ~ prestige + factor(congress) + factor(agency) + factor(icpsr), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige) %>%
                     summarise(n = n())  %>% distinct() ) ) %>%
  filter(term == "prestige")  %>% mutate(model = "DiD Overall")

Policy <- tidy(lm(n ~ prestige + factor(congress) + factor(agency) + factor(icpsr), data = p2 %>%
                    group_by(agency, year, congress, icpsr, prestige) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "DiD Policy")

Constituent <- tidy(lm(n ~ prestige + factor(congress) + factor(agency) + factor(icpsr), data = c2 %>%
                         group_by(agency, year, congress, icpsr, prestige) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "DiD Constituent")

prestige <- rbind(Overall, Policy, Constituent)
prestige


# chair 
Overall <- tidy(lm(n ~ chair + factor(congress) + factor(agency) + factor(icpsr), data = all %>%
                     group_by(agency, year, congress, icpsr, chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair")  %>% mutate(model = "DiD Overall")

Policy <- tidy(lm(n ~ chair + factor(congress) + factor(agency) + factor(icpsr), data = p2 %>%
                    group_by(agency, year, congress, icpsr, chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "DiD Policy")

Constituent <- tidy(lm(n ~ chair + factor(congress) + factor(agency) + factor(icpsr), data = c2 %>%
                         group_by(agency, year, congress, icpsr, chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "DiD Constituent")

chair <- rbind(Overall, Policy, Constituent)


# prestige_chair
Overall <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency) + factor(icpsr), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige_chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair")  %>% mutate(model = "DiD Overall")

Policy <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency) + factor(icpsr), data = p2 %>%
                    group_by(agency, year, congress, icpsr, prestige_chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "DiD Policy")

Constituent <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency) + factor(icpsr), data = c2 %>%
                         group_by(agency, year, congress, icpsr, prestige_chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "DiD Constituent")

prestige_chair <- rbind(Overall, Policy, Constituent)


# majority*presidents_party
Overall <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency) + factor(icpsr), 
                   data = all %>%
                     group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") ) %>% mutate(model = "DiD Overall")

Policy <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency) + factor(icpsr),
                  data = p2 %>%
                    group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "DiD Policy")

Constituent <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency) + factor(icpsr),
                       data = c2 %>%
                         group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "DiD Constituent")

majority <- rbind(Overall, Policy, Constituent)
majority$term <- gsub(":", "", majority$term)





# combine 
m <- rbind(prestige, chair, prestige_chair, majority)
b <- list(c("Model 1", "Prestige Committee", "Prestige Committee", "Prestige Committee"), 
          c("Model 2", "Chair", "Chair","Chair"),
          c("Model 3", "Prestige Chair", "Prestige Chair","Prestige Chair"),
          c("Model 4", "Majority", "Presidents Party", "Majority x Presidents Party"))




{dwplot(m) %>% 
    relabel_predictors(c(prestige = "Prestige Committee",
                         chair = "Chair",
                         prestige_chair = "Prestige Chair",
                         majority = "Majority",
                         presidents_party = "Presidents Party",
                         majoritypresidents_party = "Majority x Presidents Party")) +
    theme_bw() + xlab("Estimated Additional Letters per Year (Congress and Agency FE)") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Difference-in-Differences in Letter Writing") +  
    theme(legend.title = element_blank())} %>%
  add_brackets(b)












### Committee chairs contact agencies that their committee oversees more than other members of the same committee. 



# oversight chair without member FE
Overall <- tidy(lm(n ~ oversight_committee_chair + factor(congress) + factor(agency), 
                   data = all %>% filter(oversight_committee == 1) %>%
                     group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                     summarise(n = n())  %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair") ) %>% mutate(model = "Overall")

Policy <- tidy(lm(n ~ oversight_committee_chair  + factor(congress) + factor(agency),
                  data = p2 %>% filter(oversight_committee == 1) %>%
                    group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                    summarise(n = n())  %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair") )  %>% mutate(model = "Policy")

Constituent <- tidy(lm(n ~ oversight_committee_chair + factor(congress) + factor(agency),
                       data = c2 %>% filter(oversight_committee == 1) %>%
                         group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                         summarise(n = n())  %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair", "majority:presidents_party") )  %>% mutate(model = "Constituent")

oversight_committee_chair  <- rbind(Overall, Policy, Constituent)


# oversight chair with member FE
Overall <- tidy(lm(n ~ oversight_committee_chair + factor(congress) + factor(agency), 
                   data = all %>% filter(oversight_committee == 1)%>%
                     group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                     summarise(n = n()) %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair") ) %>% mutate(model = "DiD - Overall")

Policy <- tidy(lm(n ~ oversight_committee_chair  + factor(congress) + factor(agency),
                  data = p2 %>% filter(oversight_committee == 1) %>%
                    group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                    summarise(n = n())  %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair") )  %>% mutate(model = "DiD - Policy")

Constituent <- tidy(lm(n ~ oversight_committee_chair + factor(congress) + factor(agency),
                       data = c2 %>% filter(oversight_committee == 1) %>%
                         group_by(agency, year, congress, icpsr, oversight_committee_chair) %>%
                         summarise(n = n())  %>% distinct() ) ) %>%
  filter(term %in% c("oversight_committee_chair") )  %>% mutate(model = "DiD - Constituent")

oversight_committee_chairFE  <- rbind(Overall, Policy, Constituent)


# combine 
m <- rbind(oversight_committee_chair, oversight_committee_chairFE)


{dwplot(m) %>% 
    relabel_predictors(c(oversight_committee_chair = "Chairs Relative to Other Oversight Committee Members")) +
    theme_bw() + xlab("Estimated Additional Letters per Year (Congress and Agency FE)") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggtitle("Oversight Committee Chairs") +  
    theme(legend.title = element_blank(),
          panel.grid.major.y = element_blank())} 

