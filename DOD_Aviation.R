library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

#read in file and create agency column
file.name <- "DOD Aviation Logistics 2009-2017.csv" 
data <- read.csv(file.name, stringsAsFactors = FALSE)
data$agency <- "DOD"



# Format date, year, Congress, member name etc. 
data$DATE %<>% as.Date("%m/%d/%y")
data$date.closed %<>% as.Date("%m/%d/%y")


#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
data$DATE[58] = as.Date('2010-07-14')



#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Sen", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Rep|ep ", FROM), "Representative", title))  


#create variable for first name of the Sen/Rep
data %<>%
  mutate(first_name = gsub(pattern = "(Rep|Rep.|-Rep|Sen|Senator) (\\w+).*", replacement = "\\2", x=FROM)) %>% 
  mutate(first_name = ifelse(first_name=="J", "J. Randy", first_name)) %>% 
  mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
  mutate(first_name= ifelse(grepl("Bill Huizenga", FROM), "Bill", first_name))


#create variable for last name of the Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = "(Rep|Rep.|-Rep|Sen|Senator) (\\w+) (\\w+|.. \\w+|. \\w+).*", 
                          replacement = "\\3", x=FROM)) %>%
  mutate(last_name = gsub(pattern = ".* (\\w+)", replacement = "\\1", x=last_name)) %>%
  mutate(last_name = ifelse(is.na(title), NA, last_name)) %>% 
  mutate(last_name = ifelse(grepl("(Buck)", FROM), "McKeon", last_name)) %>% 
  mutate(last_name = ifelse(grepl("Robert C", FROM), "Scott", last_name)) %>% 
  mutate(last_name = gsub(pattern= "(\\w+) .*", replacement= "\\1", x=last_name))


#rearrange new columns to the front 
data %<>% select(FROM, first_name, last_name, title, everything())


#specific correction
data[31,2:4] = NA


#write.csv(data, paste("new", file.name)) # save as new file