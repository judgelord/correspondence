library(dplyr)
library(magrittr)
options(stringsAsFactors = F)

nps1 <- read.csv("NPS1.csv")
nps1$originalDATE1 <- nps1$DATE
nps1$DATE %<>% {gsub("[a-z]| ", "", .)}
nps1$DATE %<>% as.Date("%m/%d/%y")
nps1$ID %<>% {gsub("[A-Z]| ", "", .)}

nps1 %<>% distinct()

names(nps1)[4] <- "FROM2"

nps2 <- read.csv("NPS2.csv")
nps2$originalDATE2 <- nps2$DATE
nps2$DATE %<>% {gsub("[a-z]| ", "", .)}
nps2$DATE %<>% as.Date("%m/%d/%Y")
nps2$ID %<>% {gsub("[A-Z]| ", "", .)}

nps2 %<>% distinct()

nps <- full_join(nps1, nps2)
nps %<>% distinct()

nps %<>% mutate(FROM = ifelse(is.na(FROM), FROM2, FROM))

nps %<>% group_by(ID) %>% mutate(n = n()) %>%  arrange(-n) %>% ungroup()
nps$ID %<>% as.numeric()
nps %<>% arrange(-ID)

nps %<>% write.csv("DOI_NPS.csv")
