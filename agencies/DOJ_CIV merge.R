options(stringsAsFactors = F)
library(dplyr)
library(magrittr)
library(readr)
d1 <- read.csv("DOJ_CIV.csv", colClasses = "character")
d2 <- read.csv("DOJ_CIV2.csv", colClasses = "character")
d3 <- read.csv("DOJ_CIV3.csv", colClasses = "character")

d2 %<>% mutate(SUBJECT = paste(SUBJECT, Subject2, Subject3, Subject4)) %>% 
  select(-Subject2, -Subject3, -Subject4)

d3 %<>% mutate(SUBJECT = paste(SUBJECT, Subject2, Subject3, Subject4, Subject5, Subject6, Subject7, Subject8)) %>% 
  select(-Subject2, -Subject3, -Subject4, -Subject5, -Subject6, -Subject7, -Subject8)



d1 %<>% 
  full_join(d2) %<>% 
  full_join(d3) 


d1$originalDATE <- d1$DATE
d1$DATE %<>% {
  gsub("/ ", "/1", .)
}
d1$DATE %<>% {
  gsub("^/", "1/", .)
}
d1$DATE %<>% {
  gsub(" /", "1/", .)
}
d1$DATE %<>% {
  gsub(" ", "/", .)
}


data %<>% mutate(DATE = str_extract( data$DATE, "[0-9]*/[0-9]*/[0-9]{2,4}"))



d1$DATE %<>% {
  gsub("/201", "/1", .)
}
d1$DATE %<>% {
  gsub("/200", "/0", .)
}

d1$DATE[which(is.na(as.Date(d1$DATE, "%m/%d/%y")))] <- as.Date(d1$Date.Response.Due[which(is.na(as.Date(d1$DATE, "%m/%d/%y")))], "%m/%d/%y")
d1$DATE[which(is.na(as.Date(d1$DATE, "%m/%d/%y")))] <- as.Date(d1$Date.Closed[which(is.na(as.Date(d1$DATE, "%m/%d/%y")))], "%m/%d/%y")


d1 %<>% select(DATE, originalDATE, FROM, SUBJECT, everything()) %>% 
  arrange(rev(DATE)) 



write.csv(d1, "DOJ_CIV.csv")

