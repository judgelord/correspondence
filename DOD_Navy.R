# This script defines a function to clean hand-coded google sheets 
clean.DOD_NAVY <- function(file.name){
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column 
  data$agency <- file.name
  
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
data %<>% select(DATE, FROM, SUBJECT, everything())

data$CERTAINTY %<>% as.character()
data$NOTES %<>% as.character()


return(data)

} # end function

