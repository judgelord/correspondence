# this file is a sketchpad for calculating stats. See summary.html for selected stats 

options(stringsAsFactors = FALSE)
requires <- c("dplyr", "magrittr")
to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
library(dplyr) 
library(magrittr)

# Refresh data? Or load archived data file from https://drive.google.com/drive/u/0/folders/1DSGGZP_v2zwdfxg9Do3Ii4Y8UdXultVg
ifelse( F ,  source("merge.R"), load("correspondence.RData") )

install.packages("Gini")
library(Gini)

install.packages("ineq")
library(ineq)
gini <- df %>% group_by(agency) %>% mutate(perAgency = n()) %>% ungroup() %>% 
  filter(perAgency >100) %>% 
  group_by(member_state, agency, chamber, year) %>% tally() %>% ungroup() %>% 
  group_by(member_state, agency, chamber) %>% mutate(n = mean(n)) %>% ungroup() %>% 
  group_by(agency, chamber) %>% 
  mutate(byAgency = Gini(n)) %>% ungroup() %>% 
  group_by(chamber) %>%
  mutate(byChamber = Gini(n)) %>% 
  select(chamber, agency, byChamber, byAgency) %>% distinct() %>% arrange(byAgency)
  
  unique(gini$byChamber[gini$chamber=="House"])
  unique(gini$byChamber[gini$chamber=="Senate"])
  
  The volume of correspondence per legislator is highly unequal. The Gini coeficient of letters per member per year is  `
  unique(gini$byChamber[which(gini$chamber=="Senate")])
  ` for Senators and `unique(gini$byChamber[gini$chamber=="House"])` for Representatives, similar to income inequality estimates for Mexico and the United States respectivly, and much higher than inequality in campaign spending (The Gini coeficient for House campaign spending in 2016 was .29). Within the `
  nrow(gini[gini$chamber=="House"])` agencies receiving more than 100 letters, gini coeficients exceed .29 for `
  length(unique(gini$agency))
  ` from Represenatives and for `
sum(gini$chamber=="Senate" & gini$byAgency > .29359)
  # spent by campaings for house in 2016 = .3

unique(count$gini)

spent <- read.csv("spent.csv")
spent$spent <- gsub("\\$|,| ", "", spent$spent)
spent$spent %<>% as.numeric()
Gini(spent$spent)
