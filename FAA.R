# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

  
file.name <- "FAA Sam" # for testing


clean <- function(file.name) {
gs_ls() # log in 
data <- gs_title(file.name) %>% gs_read() # get data

# create agency column
data$agency <- file.name

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
  mutate(last_name = ifelse(grepl("Shea-Porter", FROM), "Shea-Porter", last_name)) %>% 
  mutate(last_name = str_to_upper(last_name))


#create variable for first name of the Sen/Rep
data %<>%
  mutate(first_name = gsub(pattern = ".*(,|, |,\\w |,\\w. |, \\w |, \\w. )(\\w+)( |.).*",
                           replacement = "\\2", x=FROM)) %>% 
  mutate(first_name = stri_trans_totitle(first_name))

 
 
#Create variable for chamber position  (Senator or Representative)
data %<>%
  mutate(chamber = ifelse (grepl("Senator|Senate", FROM), "Senate", NA)) %>% 
  mutate(chamber = ifelse(grepl("Representative", FROM), "House", chamber)) %>% 
  mutate(chamber = ifelse(grepl("Representative", assigned), "House", chamber)) %>% 
  mutate(chamber = ifelse(grepl("Senate", assigned), "Senate", chamber)) 

#create ID variable 
data$ID <- c(1:nrow(data))
  
# arrange columns for hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())

}



