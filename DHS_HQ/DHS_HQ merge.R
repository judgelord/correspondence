options(stringsAsFactors = c("ID", "WF", "DATE", "FROM", "SUBJECT", "error"))
getwd()

setwd("/Users/judgelord/correspondence/DHS_HQ")



  d1 <- 
    read.csv("DHS_HQ_cong-log-2017.csv", header =F)[,1:15]
  d2 <- 
    read.csv("DHS_HQ_cong-log-2016.csv", header =F)[,1:15]
  d3 <- 
    read.csv("DHS_HQ_cong-log-2015.csv", header =F)[,1:15]
  d4 <- 
  read.csv("DHS_HQ_cong-log-2014.csv", header =F)[,1:15]
  d5 <- 
  read.csv("DHS_HQ_cong-log-2013.csv", header =F)[,1:15]
  d6 <- 
  read.csv("DHS_HQ_cong-log-2012.csv", header =F)[,1:15]
  d7 <- 
  read.csv("DHS_HQ_cong-log-2011.csv", header =F)[,1:15]
  d8 <- 
  read.csv("DHS_HQ_cong-log-2010.csv", header =F)[,1:15]
  d9 <- 
  read.csv("DHS_HQ_cong-log-2009.csv", header =F)[,1:15]
  d10 <- 
  read.csv("DHS_HQ_cong-log-2008.csv", header =F)[,1:15]
  d11 <-
  read.csv("DHS_HQ_cong-log-2007.csv", header =F)[,1:15]
  d12 <- 
  read.csv("DHS_HQ_cong-log-2006.csv", header =F)[,1:15]
  d13 <- 
  read.csv("DHS_HQ_cong-log-2005.csv", header =F)[,1:15]
  d14 <- 
  read.csv("DHS_HQ_cong-log-2004.csv", header =F)[,1:15]
  d15 <- 
    read.csv("DHS_HQ_cong-log-2003.csv", header =F)[,1:15]

  
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

names(data) <- c("sort", "WF", "DATE", "FROM", "SUBJECT", "error")

dhsprob <- data[which(data$error != ""),]


d16 <- read.csv("DHS Anna.csv")
d16$DATE %<>% as.Date()
data$date %<>% {gsub("[a-z]|[A-Z]", "",.)}
data$DATE %<>% as.Date()




