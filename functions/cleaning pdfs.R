library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)



#######
# EPA #
#######
file.name <- "EPA-DJL.csv"
data <- read.csv(file.name)
unique(data$FROM) 
# keep only member names, not junk
data %<>% 
  mutate(FROM = ifelse (!grepl("House|Senate", FROM), NA, FROM)) 
unique(data$FROM) # check

# copy names to cover observations
for (i in 2:dim(data)[1]){
  if(is.na(data$FROM[i])){
    data$FROM[i] <- data$FROM[i-1]
  }
}

# clean subject (some are on multiple lines)
data$SUBJECT
data %<>% 
  filter(!is.na(SUBJECT)) %>%
  filter(SUBJECT != "Subject ") %>%
  filter(SUBJECT != "") 

data$SUBJECT[which(data$DATE == "")]
  
for (i in 2:dim(data)[1]){
  if(is.na(data$DATE[i])){
    data$SUBJECT[i] <- paste(data$SUBJECT[i], data$SUBJECT[i-1])
  }
}

# keep observations not junk
data %<>% 
  filter(!is.na(DATE)) %>%
  filter(DATE != "") 






#############################
# NAVY and DOD DLA Aviation #
############################
setwd("/Users/judgelord")
file.name <- "NAVY-DJL.csv"
data <- read.csv(file.name)
# clean subject (some are on multiple lines)
data$SUBJECT
data$DATE[which(data$SUBJECT == "")] # ok to filter subject?

# filter subject
data %<>% 
  filter(!is.na(SUBJECT)) %>%
  filter(SUBJECT != "") 

# misplaced subject lines for NAVY
data$SUBJECT[which(data$DATE == "" & data$FROM == "" & data$DCN == "" & data$Tasker.Status == "")]

data %<>% 
  mutate(DATE = ifelse (DATE == "", NA, DATE))


for (i in 1:(dim(data)[1]-1)){
  if(is.na(data$DATE[i+1])){
    data$Constituent[i] <- paste(data$Constituent[i], data$Constituent[i+1])
    data$SUBJECT[i] <- paste(data$SUBJECT[i], data$SUBJECT[i+1])
    data <- rbind(data[1:i, ], data[(i+2):dim(data)[1], ])
  }
}

names(data)
data %<>%
  mutate(SUBJECT = paste(SUBJECT, a, b, c, d, e, f, g))

data %<>% 
  select(FROM, DATE, SUBJECT, TYPE, CERTAINTY, ALT_TYPE, NOTES, Tasker.Status, DCN)

data %<>% 
  subset(nchar(SUBJECT)>14 | DATE != "" | FROM != "")



#######
# DHS #
#######
install.packages("gdata") # because OCR gave me sheets
library(gdata)

setwd("/Users/judgelord/Downloads/FOIA/DHS/DHS") # because there are a bunch
list.files() # check for correct files
file.names <- list.files()
data.temp <- NA
for (i in 1:length(file.names[i])){
  data.temp <- c(data.temp, readLines(file.names[i]))
}
head(data.temp)
data <- data.temp

for (i in 1:length(data)){
  if (grepl("11[0-9]", data[i]) & !grepl("11[0-9]", data[i+1])){
    data[i] <- paste(data[i],data[1+1])
    data[i] <- ""
  }
}

data  <- paste(data, collapse = " ")

data <- strsplit(data, "11[0-9]")

data %<>% 
  subset(grepl("11[0-9]",.))



# import and rbind sheets
data.temp <- read.xls(file.names[1], sheet = 1, header = F)
for (f in 1:length(file.names)){
  for (i in 1:sheetCount(file.names[f])){
    data.temp <- bind_rows(data.temp, read.xls(file.names[f], sheet = i, header = F))
  }
  data <- bind_rows(data, data.temp)
}
data %<>%
  {gsub("August Privacy Office Congressional Report|Opened between.*", "", .)}

# name vars
names(data)
# names(data) <- c("WF Number", "DATE", "FROM", "SUBJECT")

# concat subject 
for (i in 1:dim(data)[1]){
  x <- 1
  while (is.na(data$DATE[i+x])){
    x <- x + 1
  }
  data$SUBJECT <- paste(data$SUBJECT[i:i+x])
}

# delete extra lines 
data %<>% 
  # filter(!is.na(DATE)) %>%
  filter(!grepl("Subject", FROM))



data %<>% as.data.frame()
data <- paste("112" , )
?regex

install.packages("gsubfn")
library(gsubfn)
grepl("[0-9]/[0-9]{2}/[0-9]{4}", data)
names(data)
class(data)
write.csv(data, paste("new", file.name)) # save as new file



date <-str_extract_all(string=date, pattern='\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+')



