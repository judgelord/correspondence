# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# FIX multiple congressman in some rows



#file.name <- "USDA_NRCS" # for testing

#clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  
  names(data)[names(data) == 'Control Number'] <- 'ID'
  
  
  # create agency column 
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data$`Date on Letter`%<>% as.Date("%m/%d/%Y")
  data$`Date Received`%<>% as.Date("%m/%d/%Y")
  data$`Date Signed`%<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.integer(substr(DATE,1,4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # chamber 
  data %<>% 
    mutate(chamber = ifelse(`VIP Type` == "U.S. Senator", "Senate", NA)) %>%
    mutate(chamber = ifelse(`VIP Type` == "Member of Congress", "House", chamber)) 
  
  # create variable for first name
  data %<>%
    mutate(first_name =  gsub(pattern="^(\\w+) .*", replacement = "\\1", FROM)) %>% 
    mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name)) %>% 
    mutate(first_name = stri_trans_totitle(first_name)) 
  
  
  # create variable for last name
  data %<>%
    mutate(last_name = gsub(pattern= ".* (\\w+)$", replacement = "\\1", FROM)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w+)-(\\w+)", replacement = "\\1-\\2", last_name)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)-(\\w+)", replacement = "\\1\\2-\\3", last_name)) %>% 
    mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)$", replacement = "\\1\\2", last_name)) %>% 
    mutate(last_name = gsub(pattern = ".* (\\w+, Jr.)", replacement = "\\1", last_name)) %>% 
    mutate(last_name = gsub(pattern = ".* (\\w+, Sr.)", replacement = "\\1", last_name)) %>% 
    mutate(last_name = str_to_upper(last_name))
  
    
  
  
  # Consolidate and rename like subjects
  data %<>%
    mutate(SUBJECT = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING", SUBJECT), "Appropriations", SUBJECT)) %>% 
    mutate(SUBJECT = ifelse (grepl("Rural|RURAL", SUBJECT), "Rural Development", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH", SUBJECT), "Nutrition", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("FORESTRY|Forestry", SUBJECT), "Forestry", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("ENVIRONMENT|Environment|WATER", SUBJECT), "Environment", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("GRANT|Grant", SUBJECT), "Grants", SUBJECT)) %>% 
    mutate(SUBJECT = ifelse (grepl("Research|RESEARCH", SUBJECT), "Research", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Price Support", SUBJECT), "Price Support", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Government Affairs|HEARING", SUBJECT), "Government", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("CONTRACT|PURCHASE", SUBJECT), "Contracts", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Food|FOOD|DRUG|Drug|LABELING", SUBJECT), "Food and Drug Saftey", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Animal|ANIMAL", SUBJECT), "Animal Health", SUBJECT))
  
  
  
  
  data %<>%
    mutate(TYPE = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    mutate(POLICY_EVENT = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "Legislation", POLICY_EVENT)) %>%
    mutate(EVENT_NAME = ifelse (grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "Appropriations", EVENT_NAME)) %>% 
    mutate(TYPE = ifelse (grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>% 
    mutate(CERTAINTY = ifelse (grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("Price Support", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Price Support", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(EVENT_NAME = ifelse (grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "Nomination", EVENT_NAME)) %>%
    mutate(TYPE = ifelse (grepl("SOIL|PAYMENT", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("SOIL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("SOIL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("HOUSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("HOUSING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("HOUSING", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>% 
    mutate(TYPE = ifelse (grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("TELECOM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%      #SPACE seems to relate to office space/office closings
    mutate(CERTAINTY = ifelse (grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("REPORTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse(grepl("Reports", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("FERTILIZER", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("FERTLIZER", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("TRAVEL", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("TRAVEL|PAYMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("PERSONNEL|EMPLOYMENT", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%   #NOT QUITE SURE WHAT IS MEANT BY "PERSONNEL" BUT IT SHOWS UP A LOT
    mutate(CERTAINTY = ifelse (grepl("PERSONNEL|EMPLOYMENT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("FIRE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%      #FIRE PREVENTION
    mutate(CERTAINTY = ifelse (grepl("FIRE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%      
    mutate(CERTAINTY = ifelse (grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%        #THIS WOULD BE THE CASE IF "PUB REL" ENTAILED MEETING W/ CONSTITUENT GROUPS. CANNOT TELL IF THAT IS THE CASE THOUGH
    mutate(TYPE = ifelse (grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("PRESERVATION", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("PRESERVATION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (grepl("PRESERVATION", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (grepl("GOVERNMENT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (grepl("GOVERNMENT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
     # arrange columns for further hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}





