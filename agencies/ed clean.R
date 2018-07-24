
# install.packages("tidyverse")
# install.packages("magrittr")
library(tidyverse)
library(magrittr)
require("stringr")


d <- read.csv("ed.csv", stringsAsFactors = F)


for(i in 1:dim(d)[1] ) {
  d$X[i] <- paste(d[i,1:21], collapse = " ")
}


d$X[1]
d  <- d$X


for (i in length(d):1 ) {
  if (!grepl("^Name:", d[i])) {
    d[i-1] <- paste(d[i-1], d[i])
  }}

d[1:20]

d <- d[which(grepl("^Name:", d))]


d[1:20]

data <- data.frame(raw = d, FROM = NA, SUBJECT = NA, DATE = NA)

data %<>% 
  mutate(raw = gsub("\|", "1", raw)) %>% 
  mutate(raw = gsub(" /|/ ", "/", raw)) %>% 
  mutate(raw = gsub("\\n", "", raw)) %>%
  mutate(raw = gsub(" 1|1 ", "1", raw)) %>% 
  mutate(raw = gsub(" 0|0 ", "0", raw)) %>% 
  mutate(FROM = gsub("Name: " , "", raw)) %>%
  mutate(FROM = gsub("[0-9].*|NA.*|Subject.*|\\n.*","", FROM))  %>%
  mutate(FROM = gsub("\\n", "", FROM)) %>%
  mutate(SUBJECT = gsub(".*Subject:", "", raw)) %>%
  mutate(SUBJECT = gsub(" NA .*", "", SUBJECT)) %>%
  mutate(SUBJECT = gsub("\\n", "", SUBJECT)) %>%
  mutate(DATE = str_extract(raw, "[0-9]*?/[0-9]*?/\\d{4}"))
  
head(data)
  
write.csv(data, "ed.csv")
  
  