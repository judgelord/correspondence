# This script defines a function to clean hand-coded google sheets 
clean.DOD_Aviation <- function(file.name){
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column 
  data$agency <- file.name

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


#specific correction
data[31,2:4] = NA # DAN, THIS NEEDS TO BE DEFINED WITH RESPECT TO AN ID NUMBER AND VARS, NOT POSITION AS THIS MAY CHANGE

#rearrange new columns to the front 
data %<>% select(DATE, FROM, SUBJECT, everything())

} # end function
