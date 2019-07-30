
#file.name <- "IRS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

# create ID variable
data$ID <- c(1:nrow(data))

# create agency column
data$agency <- file.name

# Format date, year, Congress, member name etc. 
data$DATE <- data$`Received Date`
data$DATE %<>% as.Date("%m/%d/%y")

#checking for NA dates
NOdate <- data %>%
  filter(is.na(DATE))


#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

#Format Typo
data %<>%
  mutate(SUBJECT = str_remove_all(SUBJECT, "\\'s")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Patrick McHenrv", "Patrick McHenry")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Nancv Pelosi", "Nancy Pelosi")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Garv Peter", "Gary Peter")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Justine Amash", "Justin Amash")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Billv Lona", "Billy Long")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Mark Meadow", "Mark Meadows")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Bill Cassidv", "Bill Cassidy")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Bennie Thomoson", "Bennie Thompson")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Tim Murohv", "Tim Murphy")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Tim Udall", "Tom Udall")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Debbie Dinaell", "Debbie DINGELL")) %>% 
  mutate(SUBJECT = str_replace_all(SUBJECT, "Bernard Sander", "Bernard Sanders")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "John Cornvn", "John Cornyn")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Vicente Gonxalez", "Vicente Gonzalez")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Llovd Doaaett", "Lloyd Doggett")) %>%
  mutate(SUBJECT = str_replace_all(SUBJECT, "Cortex Masto", "Catherine Cortez Masto"))
         

#extracting members from Subject
data <- extractMemberName(data, members, 'SUBJECT')

#FOIA NOTES
data %<>%
  mutate(NOTES = ifelse(ID==137, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==94, "Unnamed members of congress",  NOTES))%>%
  mutate(NOTES = ifelse(ID==919, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==882, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==873, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==868, "Unnamed members of congress", NOTES)) 
  

##checking for special characters
#data %>% filter(ID==574) %>% select('SUBJECT')

#Failing observations
Unfoundnames <- data %>%
  filter(is.na(last_name),
         is.na(ERROR),
         is.na(NOTES))

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
