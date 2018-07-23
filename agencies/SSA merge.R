options(stringsAsFactors = F)
library(dplyr)
library(magrittr)
library(readr)
d1 <- read.csv("SSA_01a.csv", colClasses = "character")
d1b <- read.csv("SSA_01b.csv", colClasses = "character")
d2 <- read.csv("SSA_02.csv", colClasses = "character")
d3 <- read.csv("SSA_03.csv", colClasses = "character")
d4 <- read.csv("SSA_04.csv", colClasses = "character")
d5 <- read.csv("SSA_05.csv", colClasses = "character")
d7 <- read.csv("SSA_07.csv", colClasses = "character")
d8 <- read.csv("SSA_08.csv", colClasses = "character")



d1 %<>% 
  full_join(d1b) %>% 
  full_join(d2) %>% 
  full_join(d3) %>% 
  full_join(d4) %>% 
  full_join(d5) %>% 
  full_join(d7) %>%   
  full_join(d8) 

d1 %<>% mutate(DATE <- ifelse(DATE == ""| is.na(DATE), Date.Received, DATE)) %>%
  mutate(DATE <- ifelse(DATE == "" | is.na(DATE), Date.received..Forwarded, DATE)) %>%
  mutate(DATE <- ifelse(DATE == ""| is.na(DATE), Date.of.Referral, DATE)) %>%
  mutate(DATE <- ifelse(DATE == ""| is.na(DATE), Date.Due, DATE)) %>%
  mutate(DATE <- ifelse(DATE == ""| is.na(DATE), Date.Closed, DATE))

d1$originalDATE <- d1$DATE
d1$DATE %<>% {
  gsub("[a-z]|[A-Z]|\\*|\n", "", .)
}

data %<>% mutate(DATE = str_extract( data$DATE, "[0-9]*/[0-9]*/[0-9]{2,4}"))

d1$DATE %<>% {
  gsub("^111", "11/", .)
}
d1$DATE %<>% {
  gsub("^21", "2/", .)
}
d1$DATE %<>% {
  gsub("^31", "3/", .)
}
d1$DATE %<>% {
  gsub("^41", "4/", .)
}
d1$DATE %<>% {
  gsub("^51", "5/", .)
}
d1$DATE %<>% {
  gsub("^61", "6/", .)
}
d1$DATE %<>% {
  gsub("^71", "7/", .)
}
d1$DATE %<>% {
  gsub("^81", "8/", .)
}
d1$DATE %<>% {
  gsub("^91", "9/", .)
}
d1$DATE %<>% {
  gsub("120", "/0", .)
}
d1$DATE %<>% {
  gsub("/201", "/1", .)
}
d1$DATE %<>% {
  gsub("/200", "/0", .)
}

d1$DATE %<>% as.Date("%m/%d/%y")


d1 %<>% mutate(SUBJECT = paste(SUBJECT, SUBJECT2))
d1 %<>% mutate(ACTION = paste(Date.received..Forwarded, Follow.Up.Date...Completion.Date, Referred.to..CMS..FO..etc., Referred.to..DO..ODAR..RCD...., CCRS.Specialist, Constituent.Name))
               
d1 %<>% select(sort, DATE, originalDATE, FROM, SUBJECT, ACTION, everything()) %>% 
  filter(!is.na(DATE) | !is.na(FROM)) %>% 
  filter(originalDATE != "" | FROM != "") %>% 
  filter(FROM != "Congressional Office (State/District)") %>% 
  arrange(rev(DATE)) 

               

write.csv(d1, "SSA.csv")

