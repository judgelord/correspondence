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
