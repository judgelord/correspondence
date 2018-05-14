# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA_ERS" # for testing



#### FIXME
## Some rows have multiple representativies --> need to split into duplicate rows




clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column 
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  names(data)[names(data) == 'Date on Letter'] <- 'DATE'
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.integer(substr(DATE,1,4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create variable for first name
  data %<>%
    mutate(first_name =  gsub(pattern="^(\\w+) .*", replacement = "\\1", FROM)) %>% 
    mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name)) %>% 
    mutate(first_name =  ifelse(grepl("Butch", first_name), "C.L. 'Butch'", first_name))
  
  # create variable for last name
  data %<>%
    mutate(last_name = gsub(pattern= ".* (\\w+)$", replacement = "\\1", FROM)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w+)-(\\w+)", replacement = "\\1-\\2", last_name)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)-(\\w+)", replacement = "\\1\\2-\\3", last_name)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)$", replacement = "\\1\\2", last_name))  
  
  #Create variable for position title (Senator or Representative)
  data %<>%
    mutate(title = ifelse (grepl("Senator", Position), "Senator", NA)) %>% 
    mutate(title = ifelse(grepl("Congress", Position), "Representative", title)) 
  
  
  # Consolidate and rename like subjects
  data %<>%
    mutate(SUBJECT = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING", SUBJECT), "Appropriations", SUBJECT)) %>% 
    mutate(SUBJECT = ifelse (grepl("Rural|RURAL", SUBJECT), "Rural Development", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH", SUBJECT), "Nutrition", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("FORESTRY|Forestry", SUBJECT), "Forestry", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("ENVIRONMENT|Environment|WATER", SUBJECT), "Environment", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("FARM|Farm", SUBJECT), "Farms", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("GRANT|Grant", SUBJECT), "Grants", SUBJECT)) %>% 
    mutate(SUBJECT = ifelse (grepl("Research|RESEARCH", SUBJECT), "Research", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Price Support", SUBJECT), "Price Support", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Government Affairs|HEARING", SUBJECT), "Government", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("CONTRACT|PURCHASE", SUBJECT), "Contracts", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Food|FOOD|DRUG|Drug|LABELING", SUBJECT), "Food and Drug Saftey", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Animal|ANIMAL", SUBJECT), "Animal Health", SUBJECT))
  
  
  
  # arrange columns for further hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}





