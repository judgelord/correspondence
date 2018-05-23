# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 file.name <- "USDA" #for testing

#clean <- function(file.name){
  
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
  mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name)) 
data <- formatFirstName(data)





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
                                                               replacement = "\\1", FROM2), last_name))
data <- formatLastName(data)

  
 
 

data <- data[,!(names(data) %in% "FROM2")]

  
data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>% 
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "Legislation", POLICY_EVENT)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("Appropriation|APPROPRIATION|Funding|FUNDING|Farm Bill", SUBJECT, ignore.case = TRUE), "Appropriations", EVENT_NAME)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Rural|RURAL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>% 
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Price Support", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("Price Support", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", TYPE) & grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "Nomination", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%      #SPACE seems to relate to office space/office closings
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPORTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse(grepl("Reports", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FERTILIZER", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("FERTLIZER", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TRAVEL", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("TRAVEL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PERSONNEL", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%   #NOT QUITE SURE WHAT IS MEANT BY "PERSONNEL" BUT IT SHOWS UP A LOT
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("PERSONNEL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FIRE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%      #FIRE PREVENTION
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("FIRE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%      
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%        #THIS WOULD BE THE CASE IF "PUB REL" ENTAILED MEETING W/ CONSTITUENT GROUPS. CANNOT TELL IF THAT IS THE CASE THOUGH
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE))    




  # arrange columns for further hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}






 