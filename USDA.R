library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

file.name <- "USDA-DJL.csv"
agency <- "USDA"
data <- read.csv(file.name)
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
  


# select only the common SUBJECTS, useful for plotting 
major.subjects <- c("Appropriations", "Nutrition",
                    "Forestry", 
                    "Environment","Farms", 
                    "Research", "Price Support", "Government", "Food and Drug Saftey")

  # data %<>% filter(SUBJECT %in% major.subjects)
  mocs <- data %>% group_by(FROM) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    ungroup()
  mocs <- mocs$FROM[1:10]
  mocs <- mocs[which(mocs != "")]
  
  
  unique(data$SUBJECT) # view SUBJECT strings
  

data$DATE %<>% as.Date("%m/%d/%y")

data %<>% mutate(year = as.integer(substr(DATE,1,4)))

# data %<>% group_by(year) %>% count()

ggplot(data %>% filter(FROM %in% mocs), aes(x = year, fill = FROM)) + geom_histogram() +
  labs(x = "", y = "", title = paste("Letters from top 9 Members of Congress to the", agency)) +
  theme(legend.position = "right", legend.title = element_blank(),
        panel.background = element_blank()) +
  scale_fill_hue(l=80)
  

