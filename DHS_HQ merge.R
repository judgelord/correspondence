options(stringsAsFactors = F)
getwd()

setwd("/Users/judgelord/correspondence/DHS_HQ")

d16 <- read.csv("DHS Anna.csv")
d16$DATE %<>% {
  gsub("[a-z]|[A-Z]| |\n|-.*", "", .)
}
d16$DATE %<>% {
  gsub("^111", "11/", .)
}
d16$DATE %<>% {
  gsub("^21", "2/", .)
}
d16$DATE %<>% {
  gsub("^31", "3/", .)
}
d16$DATE %<>% {
  gsub("^41", "4/", .)
}
d16$DATE %<>% {
  gsub("^51", "5/", .)
}
d16$DATE %<>% {
  gsub("^61", "6/", .)
}
d16$DATE %<>% {
  gsub("^71", "7/", .)
}
d16$DATE %<>% {
  gsub("^81", "8/", .)
}
d16$DATE %<>% {
  gsub("^91", "9/", .)
}
d16$DATE %<>% {
  gsub("120", "/0", .)
}
d16$DATE %<>% {
  gsub("/201", "/1", .)
}
d16$DATE %<>% {
  gsub("/200", "/", .)
}

d16$DATE %<>% as.Date("%m/%d/%y")










d1 <-
  read.csv("DHS_HQ_cong-log-2017.csv", header = F)[, 1:15]
d2 <-
  read.csv("DHS_HQ_cong-log-2016.csv", header = F)[, 1:15]
d3 <-
  read.csv("DHS_HQ_cong-log-2015.csv", header = F)[, 1:15]
d4 <-
  read.csv("DHS_HQ_cong-log-2014.csv", header = F)[, 1:15]
d5 <-
  read.csv("DHS_HQ_cong-log-2013.csv", header = F)[, 1:15]
d6 <-
  read.csv("DHS_HQ_cong-log-2012.csv", header = F)[, 1:15]
d7 <-
  read.csv("DHS_HQ_cong-log-2011.csv", header = F)[, 1:15]
d8 <-
  read.csv("DHS_HQ_cong-log-2010.csv", header = F)[, 1:15]
d9 <-
  read.csv("DHS_HQ_cong-log-2009.csv", header = F)[, 1:15]
d10 <-
  read.csv("DHS_HQ_cong-log-2008.csv", header = F)[, 1:15]
d11 <-
  read.csv("DHS_HQ_cong-log-2007.csv", header = F)[, 1:15]
d12 <-
  read.csv("DHS_HQ_cong-log-2006.csv", header = F)[, 1:15]
d13 <-
  read.csv("DHS_HQ_cong-log-2005.csv", header = F)[, 1:15]
d14 <-
  read.csv("DHS_HQ_cong-log-2004.csv", header = F)[, 1:15]
d15 <-
  read.csv("DHS_HQ_cong-log-2003.csv", header = F)[, 1:15]


names(d1) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d2) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d3) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d4) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d5) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d6) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d7) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d8) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d9) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d10) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d11) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d12) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d13) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d14) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")
names(d15) <- c("ID", "WF", "DATE", "FROM", "SUBJECT", "error")

data <- rbind(d1,
              d2,
              d3,
              d4,
              d5,
              d6,
              d7,
              d8,
              d9,
              d10,
              d11,
              d12,
              d13,
              d14,
              d15)

names(data) <- c("sort", "WF", "DATE", "FROM2", "SUBJECT2", "error")

dhsprob <- data[which(data$error != ""), ]

data <- data[,2:5]
names(data)

data$DATE %<>% {
  gsub("[a-z]|[A-Z]| |\n|-.*", "", .)
}
data %<>% mutate(DATE = str_extract( data$DATE, "[0-9]*/[0-9]*/[0-9]{4}"))
data$DATE %<>% as.Date("%m/%d/%Y")


# MERGE 


data %<>% full_join(d16)

data %<>% mutate(SUBJECT = ifelse(is.na(SUBJECT), SUBJECT2, SUBJECT))
data %<>% mutate(FROM = ifelse(is.na(FROM), FROM2, FROM))


data %<>% arrange(WF)
write.csv(data, "DHS_HQ Anna")

