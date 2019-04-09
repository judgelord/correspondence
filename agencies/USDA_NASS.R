# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "USDA_NASS Henry" # for testing




clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  # create agency column 
  data$agency <- file.name
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
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



# preprocess
data$FROM <- gsub(", 2nd District Hawaii","",data$FROM)

###############    
# Creates duplicate rows for lines with multiple representatives
for(i in 1:nrow(data)){
  if(grepl(",", data$FROM[i])) {
    
    new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ",") + 1))
    new$FROM <- unlist(str_split(data$FROM[i], ","))
    
    data <- rbind(data, new)
    
  }
}
data <- data[-grep(",", data$FROM),] # removes orginal row with all data
################

# create variable  for first and last name
data <- extractMemberName(data, members, 'FROM')


# arrange columns for further hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}





