# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DHS Katie" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column
  data$agency <- file.name

  
#rename agency column
colnames(data)[colnames(data) == 'AGENCY'] <- 'subagency'
data %<>%
  mutate(agency = ifelse(is.na(agency), 'DHS', agency))


# Format date, year, Congress, member name etc.

#####   FIXM
data %<>%
  mutate(DATE = gsub(pattern = "120", replacement = "/20", x=DATE)) %>% 
  mutate(DATE = gsub(pattern = "(4|5|6|7|8|9|10|11|12)1(\\d)", replacement = "\\1/\\2", x=DATE)) 

# %>% 
#   mutate(DATE = gsub(pattern = "^1(\\d)/20", replacement = "\\1/\\2", x=DATE))

data <- extractMemberName(data, members, 'FROM')


data$DATE %<>% as.Date("%m/%d/%y")

#sum(is.na(data$DATE))


#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001



#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Senator", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Congressman", FROM), "Representative", title)) 


# #create variable for first name of the Sen/Rep
# data %<>%
#   mutate(first_name = gsub(pattern = "(Congressman|Senator) (\\w+).*", replacement = "\\2", x=FROM)) %>% 
#   mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
#   mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name))
# 
# 
# 
# #create variable for last name of the Sen/Rep
# data %<>%
#   mutate(last_name = gsub(pattern = ".* (\\w+)$", 
#                           replacement = "\\1", x=FROM)) %>% 
#   mutate(last_name = ifelse(grepl("Jason Cha", FROM), "Chaffetz", last_name)) %>%
#   mutate(last_name = ifelse(grepl("O'Rourke", FROM), "O'Rourke", last_name)) %>% 
#   mutate(last_name = ifelse(is.na(title), NA, last_name))
# 
# data$last_name %<>% toupper()


# chamber 
data %<>% 
  mutate(chamber = ifelse(title == "Senator", "Senate", NA)) %>%
  mutate(chamber = ifelse(title == "Representative", "House", chamber)) 


# arrange columns for hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())



data%<>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ON BEHALF OF CON", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ON BEHALF OF CON", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))



}

