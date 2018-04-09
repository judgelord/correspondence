library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

#read in file and create agency column
file.name <- "NAVY 2013-2016-DJL.csv" 
data <- read.csv(file.name, stringsAsFactors = FALSE)
data$agency <- "DOD"


# Format date, year, Congress, member name etc. 
data$DATE %<>% as.Date("%m/%d/%y")

#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Sen|SEN", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Rep|REP", FROM), "Representative", title))  


#create variable for last name of the Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = "(REP|SEN)(.|- | - |. )(\\w+)(,| ,).*", 
                          replacement = "\\3", x=FROM)) %>% 
  mutate(last_name = ifelse(is.na(title), NA, last_name)) %>% 
  mutate(last_name = ifelse(grepl("\\W", last_name), NA, last_name))



#create variable for first name of the Sen/Rep
data %<>%
  mutate(first_name = gsub(pattern = ".*, (\\w+).*", 
                           replacement = "\\1", x=FROM)) %>% 
  mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
  mutate(first_name = ifelse(grepl("\\W", first_name), NA, first_name))

#rearrange new columns to the front 
data %<>% select(X, FROM, first_name, last_name, title, everything())
  
  
  
 
#write.csv(data, paste("new", file.name)) # save as new file
