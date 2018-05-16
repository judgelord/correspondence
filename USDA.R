# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 file.name <- "USDA" #for testing

clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  # create agency column 
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.integer(substr(DATE,1,4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001  

# Next, clean up SUBJECT for auto-coding (notice if TYPE has been hand coded)  
unique(data$SUBJECT) # view SUBJECT strings
log <- data %>% group_by(SUBJECT) %>% count() %>% arrange(desc(n))

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
  
# most common SUBJECTS, useful for plotting 
# major.subjects <- c("Appropriations", "Nutrition", "Forestry", "Environment","Farms", "Research", "Price Support", "Government", "Food and Drug Saftey")

# create variable for first name
data %<>%
  mutate(first_name =  gsub(pattern="^(\\w+) .*", replacement = "\\1", FROM)) %>% 
  mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name)) %>% 
  mutate(first_name = stri_trans_totitle(first_name))


data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II|, IV|IV", "", data$FROM)



# create variable for last name
data %<>%
  mutate(last_name = gsub(pattern= ".* (\\w+)$", replacement = "\\1", FROM2)) %>% 
  mutate(last_name = gsub(pattern= ".* (\\w+)-(\\w+)", replacement = "\\1-\\2", last_name)) %>% 
  mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)-(\\w+)", replacement = "\\1\\2-\\3", last_name)) %>% 
  mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)$", replacement = "\\1\\2", last_name)) %>% 
  mutate(last_name = gsub(pattern = ".* (\\w+, Jr.|\\w+ Jr.)", replacement = "\\1", last_name)) %>% 
  mutate(last_name = gsub(pattern = ".* (\\w+, Sr.)", replacement = "\\1", last_name)) %>% 
  mutate(last_name = gsub(pattern = ".* (\\w+, ..)", replacement = "\\1", last_name))  %>% 
  mutate(last_name = ifelse(grepl(".* (\\w+, ..)", FROM2), gsub(pattern=".* (\\w+, ..)", 
                                                               replacement = "\\1", FROM2), last_name)) %>% 
  mutate(last_name = ifelse(grepl(".* (\\w+ ..)", FROM2), gsub(pattern=".* (\\w+ ..)", 
                                                               replacement = "\\1", FROM2), last_name)) %>% 
  mutate(last_name = str_to_upper(last_name)) %>% 
  mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
  mutate(last_name = gsub("DELAURO", replacement = "DeLAURO", last_name)) %>% 
  mutate(last_name = gsub("ANN EMERSON", replacement = "EMERSON", last_name)) %>% 
  mutate(last_name = gsub("MICHAEL CONAWAY", replacement = "CONAWAY", last_name)) %>% 
  mutate(last_name = gsub("DEFAZIO", replacement = "DeFAZIO", last_name)) %>%  
  mutate(first_name = gsub("BENJAMIN NELSON", replacement = "Earl", last_name)) %>% 
  mutate(last_name = gsub("BENJAMIN NELSON", replacement = "NELSON", last_name)) %>% 
  mutate(first_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "John", last_name)) %>% 
  mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% 
  mutate(first_name = gsub(".*SANDLIN.*", replacement = "Stephanie", last_name)) %>% 
  mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name))
  
  


  

  




# arrange columns for further hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}





