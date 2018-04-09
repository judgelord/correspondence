library(tidyverse)
library(magrittr)
library(readxl)
options(stringsAsFactors = FALSE)

#read in file and create agency variable
file.name <- "Postal Regulatory Commission Congressional inquiries 2009-2017.xlsx" 
data <- read_excel(file.name)
data$agency <- "Postal Regulatory Comission"



#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Sen.|Sen |Senator", X__1), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Rep\\.|Rep ", X__1), "Representative", title))

#create year and congress columns
data %<>% mutate(year = as.numeric(substring(Date,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


#reformat state column
data$State %<>% stateFromLower()


#create last name variable for Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = ".* (\\w+)$", replacement = '\\1', x = X__1))

#rearrange the columns 
data %<>% select(From, X__1, last_name, title, Date, everything() )

#write.csv(data, paste("new", file.name)) # save as new file
