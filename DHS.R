#install.packages("googlesheets")
library(googlesheets)
library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

file.name <- "DHS Megha"
gs_ls() # log in 
data <- gs_title(file.name) %>% gs_read() # get data



#rename agency column
colnames(data)[colnames(data) == 'AGENCY'] <- 'agency'
data %<>%
  mutate(agency = ifelse(is.na(agency), 'DHS', agency))


# Format date, year, Congress, member name etc.

#####   FIXME
data %<>%
  mutate(DATE = gsub(pattern = "120", replacement = "/20", x=DATE)) %>% 
  mutate(DATE = gsub(pattern = "(4|5|6|7|8|9|10|11|12)1(\\d)", replacement = "\\1/\\2", x=DATE)) 

# %>% 
#   mutate(DATE = gsub(pattern = "^1(\\d)/20", replacement = "\\1/\\2", x=DATE))




data$DATE %<>% as.Date("%m/%d/%y")

#sum(is.na(data$DATE))


#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001



#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Senator", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Congressman", FROM), "Representative", title)) 


#create variable for first name of the Sen/Rep
data %<>%
  mutate(first_name = gsub(pattern = "(Congressman|Senator) (\\w+).*", replacement = "\\2", x=FROM)) %>% 
  mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
  mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name))





#create variable for last name of the Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = ".* (\\w+)$", 
                          replacement = "\\1", x=FROM)) %>% 
  mutate(last_name = ifelse(grepl("Jason Cha", FROM), "Chaffetz", last_name)) %>%
  mutate(last_name = ifelse(grepl("O'Rourke", FROM), "O'Rourke", last_name)) %>% 
  mutate(last_name = ifelse(is.na(title), NA, last_name))

data %<>% select(X1, FROM, first_name, last_name, title, DATE, everything())

