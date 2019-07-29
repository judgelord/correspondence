
# file.name <- "IRS" # for testing

#file.name <- "IRS" #for testing 12 June

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

# create ID variable
data$ID <- c(1:nrow(data))

# create agency column
data$agency <- file.name

# Format date, year, Congress, member name etc. 
data$DATE <- data$`Received Date`
data$DATE %<>% as.Date("%m/%d/%y")

#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

data <- extractMemberName(data, members, 'SUBJECT')

#sample <- data %>%
#filter(is.na(first_name))  
#View(sample)

##checking code

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
