library(googlesheets)
library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

file.name <- "FAA Sam"
gs_ls() # log in 
data <- gs_title(file.name) %>% gs_read() # get data

# create agency column
data$agency <- "FAA"

# Format date, year, Congress, member name etc. 
data$DATE %<>% as.Date("%d-%b-%y")


#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


#create variable for last name of the Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = "^(\\w+|\\w+ \\w+)( ,|,).*", 
                          replacement = "\\1", x=FROM)) %>% 
  mutate(last_name = ifelse(grepl("Diaz-Balart", FROM), "Diaz-Balart", last_name)) %>% 
  mutate(last_name = ifelse(grepl("Shea-Porter", FROM), "Shea-Porter", last_name))

  
sum(grepl("/", data$last_name))


#create variable for first name of the Sen/Rep
data %<>%
  mutate(first_name = gsub(pattern = ".*(,|, |,\\w |,\\w. |, \\w |, \\w. )(\\w+)( |.).*",
                           replacement = "\\2", x=FROM)) 
 
 
#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Senator", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Representative", FROM), "Representative", title)) %>% 
  mutate(title = ifelse(grepl("Representative", assigned), "Representative", title)) %>% 
  mutate(title = ifelse(grepl("Senate", assigned), "Senator", title)) 
  
  



data %<>% select(FROM, first_name, last_name, title, DATE, everything())




