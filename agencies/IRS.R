# file.name <- "IRS" # for testing

clean <- function(file.name) {
data <- gs_title(file.name) %>% gs_read() # get data

# create ID variable
colnames(data)[colnames(data) == 'X1'] <- 'ID'

# create agency column
data$agency <- file.name

# Format date, year, Congress, member name etc. 
data$DATE %<>% as.Date("%Y-%m-%d")

#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

data <- getFirstLast.Comma(data, 'FROM')


# format state variable
data$state <- stateFromLower(data$state)
# arrange columns for hand coding
data %<>% select(ID, DATE, FROM, SUBJECT, everything())

data %<>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMPANIES", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMPANIES", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("COMPANIES", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HEALTHCARE REFORM|COMMISSIONER'S TRACKING|COMMISSIONER TRACKING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEALTHCARE REFORM|COMMISSIONER'S TRACKING|COMMISSIONER TRACKING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", TYPE)) 


}  
