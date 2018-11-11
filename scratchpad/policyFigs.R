requires <- c("tidyr", "broom", "knitr","ineq", "dplyr", "ggplot2", "magrittr", "stargazer", "maps", "fiftystater", "mapproj", "dotwhisker")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
library(tidyr)
library(ineq)
library(dplyr)
library(ggplot2)
library(magrittr)
library(stargazer)
library(maps)
library(fiftystater)
library(mapproj)
library(knitr)
library(broom)
library(dotwhisker)


df %>% 
  filter(!is.na(POLICY_EVENT)) %>% 
  mutate(POLICY_EVENT  = {gsub(";.*|/.*|\\(.*|\\?|MAYBE |PROBABLY |POSSIBLE |POSSIBLY ","", toupper(.$POLICY_EVENT), ignore.case = T)} ) %>%
  mutate(total = n()) %>% 
  group_by(POLICY_EVENT) %>% mutate(nPE = n()) %>% ungroup() %>%
  filter(nPE > 10) %>% 
  # mutate(total = sum(nPE)) %>% 
  mutate(percent = paste(POLICY_EVENT, 100*round(nPE/total, 3), "%")) %>% 
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>%
  ggplot() + 
  geom_point(aes(x = DATE, y = reorder(agency, n), color = percent), #gsub(";.*|/.*|\\(.*", POLICY_EVENT))), 
             #alpha = .4,
             shape = 73,
             size=2) + 
  labs(x = "Date of Correspondence",
       y = "Agency, N",
       title = paste("Coded Observations, N =", sum(!is.na(df$POLICY_EVENT) ) ) ) +
  facet_grid(complete ~ ., scales = "free_y", space = "free_y") + 
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x.top = element_text())


### Core Model

all <- df %>% filter(year < 2018) %>% 
  mutate(POLICY_EVENT  = {gsub(";.*|/.*|\\(.*|\\?|MAYBE |PROBABLY |POSSIBLE |POSSIBLY ","", toupper(.$POLICY_EVENT), ignore.case = T)} )# %>% filter(congress != 115)
leg <- filter(all, POLICY_EVENT == "LEGISLATION" | POLICY_EVENT == "APPROPRIATION"| POLICY_EVENT=='EARMARK') 
exe <- filter(all, POLICY_EVENT == "RULE")


# prestige committee
Overall <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige")  %>% mutate(model = "Overall")

Legislative <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = leg %>%
                    group_by(agency, year, congress, icpsr, prestige) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Legislative")

Executive <- tidy(lm(n ~ prestige + factor(congress) + factor(agency), data = exe %>%
                         group_by(agency, year, congress, icpsr, prestige) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige") %>% mutate(model = "Executive")

prestige <- rbind(Overall, Legislative, Executive)



# chair 
Overall <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair")  %>% mutate(model = "Overall")

Legislative <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = leg %>%
                    group_by(agency, year, congress, icpsr, chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "Legislative")

Executive <- tidy(lm(n ~ chair + factor(congress) + factor(agency), data = exe %>%
                         group_by(agency, year, congress, icpsr, chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "chair") %>% mutate(model = "Executive")

chair <- rbind(Overall, Legislative, Executive)


# prestige_chair
Overall <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = all %>%
                     group_by(agency, year, congress, icpsr, prestige_chair) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair")  %>% mutate(model = "Overall")

Legislative <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = leg %>%
                    group_by(agency, year, congress, icpsr, prestige_chair) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "Legislative")

Executive <- tidy(lm(n ~ prestige_chair + factor(congress) + factor(agency), data = exe %>%
                         group_by(agency, year, congress, icpsr, prestige_chair) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term == "prestige_chair") %>% mutate(model = "Executive")

prestige_chair <- rbind(Overall, Legislative, Executive)


# majority*presidents_party
Overall <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency), 
                   data = all %>%
                     group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                     summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") ) %>% mutate(model = "Overall")

Legislative <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency),
                  data = leg %>%
                    group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                    summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "Legislative")

Executive <- tidy(lm(n ~ majority*presidents_party + factor(congress) + factor(agency),
                       data = exe %>%
                         group_by(agency, year, congress, icpsr, majority, presidents_party) %>%
                         summarise(n = n())   %>% distinct() ) ) %>%
  filter(term %in% c("majority","presidents_party", "majority:presidents_party") )  %>% mutate(model = "Executive")

majority <- rbind(Overall, Legislative, Executive)
majority$term <- gsub(":", "", majority$term)





# combine 
m <- rbind(prestige, chair, prestige_chair, majority)
b <- list(c("Model 1", "Prestige Committee", "Prestige Committee", "Prestige Committee"), 
          c("Model 2", "Chair", "Chair","Chair"),
          c("Model 3", "Prestige Chair", "Prestige Chair","Prestige Chair"),
          c("Model 4", "Majority", "Presidents Party", "Majority x Presidents Party"))


kable(m)
