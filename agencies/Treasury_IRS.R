
# file.name <- "Treasury_IRS Rochelle" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

# create agency column
data$agency <- file.name

# Format date, year, Congress, member name etc. 
data$DATE %<>% str_remove(" .*") # some cells have more than one date, taking the first, which should generally be `Received Date`
data %<>% mutate(DATE = ifelse(is.na(DATE), `Received Date`, DATE))

data$date1 <- data$DATE 
data$DATE <- data$date1
data$DATE %<>% as.Date("%m/%d/%y")

# fill in missing (may not actually get any more)
data$DATE[is.na(data$DATE)] <- as.Date(data$date1[is.na(data$DATE)], "%m/%d/%Y")
data$DATE[is.na(data$DATE)] <- as.Date(data$`Received Date`[is.na(data$DATE)], "%m/%d/%y")
data$DATE[is.na(data$DATE)] <- as.Date(data$`Due Date`[is.na(data$DATE)], "%m/%d/%y")

#checking for NA dates
NOdate <- data %>%
  filter(is.na(DATE))
NOdate %>% 
  select(date1, `Received Date`, SUBJECT) %>% 
  kable()

NOdate %>% select(Sort, date1, `Received Date`) %>% 
  filter(nchar(`Received Date`)>3, 
         nchar(`Received Date`)<23) %>% 
  kable()

NOdate %>% select(Sort, date1, `Received Date`) %>% 
  filter(!`Received Date` %in% c("NA", "Received Date", "DATE"),
         !is.na(`Received Date`)) %>% 
  kable()
# other bad dates 
data %>% filter(!str_detect(DATE, "^200|^201")) %>% select(Sort, DATE, `Received Date`) %>% kable()
  
  
#create year and congress columns
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


#extracting members from Subject becsome FROM IS BLANK 
data %<>% mutate(FROM = SUBJECT)


#Format Typo
data %<>%
  mutate(FROM = str_remove_all(FROM, "\\'s")) %>%
  mutate(FROM = str_replace_all(FROM, "Patrick McHenrv", "Patrick McHenry")) %>%
  mutate(FROM = str_replace_all(FROM, "Nancv Pelosi", "Nancy Pelosi")) %>%
  mutate(FROM = str_replace_all(FROM, "Garv Peter", "Gary Peter")) %>%
  mutate(FROM = str_replace_all(FROM, "Justine Amash", "Justin Amash")) %>%
  mutate(FROM = str_replace_all(FROM, "Billv Lona", "Billy Long")) %>%
  mutate(FROM = str_replace_all(FROM, "Mark Meadow", "Mark Meadows")) %>%
  mutate(FROM = str_replace_all(FROM, "Bill Cassidv", "Bill Cassidy")) %>%
  mutate(FROM = str_replace_all(FROM, "Bennie Thomoson", "Bennie Thompson")) %>%
  mutate(FROM = str_replace_all(FROM, "Tim Murohv", "Tim Murphy")) %>%
  mutate(FROM = str_replace_all(FROM, "Tim Udall", "Tom Udall")) %>%
  mutate(FROM = str_replace_all(FROM, "Debbie Dinaell", "Debbie DINGELL")) %>% 
  mutate(FROM = str_replace_all(FROM, "Bernard Sander", "Bernard Sanders")) %>%
  mutate(FROM = str_replace_all(FROM, "John Cornvn", "John Cornyn")) %>%
  mutate(FROM = str_replace_all(FROM, "Vicente Gonxalez", "Vicente Gonzalez")) %>%
  mutate(FROM = str_replace_all(FROM, "Llovd Doaaett", "Lloyd Doggett")) %>%
  mutate(FROM = str_replace_all(FROM, "Cortex Masto", "Catherine Cortez Masto")) %>%
  mutate(FROM = str_replace_all(FROM, "Tom Marion", "Tom Marino")) %>%
  mutate(FROM = str_replace_all(FROM, "Tim Rvan", "Tim Ryan")) %>%
  mutate(FROM = str_replace_all(FROM, "Thom Tlllis", "Thom Tillis")) %>%
  mutate(FROM = str_replace_all(FROM, "Dana Rohrbacher", "Dana ROHRABACHER")) %>%
  mutate(FROM = str_replace_all(FROM, "Pattv Murray", "Patty Murray")) %>%
  mutate(FROM = str_replace_all(FROM, "Robert Whittman", "Robert Wittman")) %>%
  mutate(FROM = str_replace_all(FROM, "Mark Warren", "Mark Warner")) %>%
  mutate(FROM = str_replace_all(FROM, "Edwards Royce", "Edward Royce")) %>%
  mutate(FROM = str_replace_all(FROM, "Nita Lowev", "Nita Lowey")) %>%
  mutate(FROM = str_replace_all(FROM, "Chris Val Hollen", "Chris Van Hollen")) %>%
  mutate(FROM = str_replace_all(FROM, "Martha McSallv", "Martha McSally")) %>%
  mutate(FROM = str_replace_all(FROM, "Patrick Toomev", "Patrick Toomey")) %>%
  mutate(FROM = str_replace_all(FROM, "Mark Sanders", "Mark Sanford")) %>%
  mutate(FROM = str_replace_all(FROM, "Grassley", "Chuck Grassley")) %>%
  mutate(FROM = str_replace_all(FROM, "Maxine Water", "Maxine Waters")) %>%
  mutate(FROM = str_replace_all(FROM, "Martha Me Sally", "Martha McSally")) %>%
  mutate(FROM = str_replace_all(FROM, "Sanford", "Mark Sanford")) %>%
  mutate(FROM = str_replace_all(FROM, "Schiff", "Adam Schiff")) %>%
  mutate(FROM = str_replace_all(FROM, "Schumer", "Chuck Schumer")) %>%
  mutate(FROM = str_replace_all(FROM, "Rubio", "Marco Rubio")) %>%
  mutate(FROM = str_replace_all(FROM, "Mark Sandford", "Mark Sanford")) %>%
  mutate(FROM = str_replace_all(FROM, "Sanford", "Mark Sanford")) %>%
  mutate(FROM = str_replace_all(FROM, "DeSantis", "DeSANTIS, Ron")) %>%
  mutate(FROM = str_replace_all(FROM, "John Comvn", "John Cornyn")) %>%
  mutate(FROM = str_replace_all(FROM, "Mark Veasey", "Marc Veasey"))
  


# apply extractmembername from legislators package 
data %<>% extractMemberName(col_name = 'FROM', congress = "congress")

# old ID still used in some places
if(!"ID" %in% names(data)){
  data %<>% mutate(ID = data_id)
}

#non-members of congress   
data %<>%
  mutate(ERROR = ifelse(str_detect(FROM, "Aumua Amata Coleman Radewagen"), "Non Voting Member", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Stacey Plaskett"), "Non Voting Member", ERROR))
  

#FOIA NOTES
data %<>%
  mutate(NOTES = ifelse(ID==137, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==94, "Unnamed members of congress",  NOTES)) %>%
  mutate(NOTES = ifelse(ID==919, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==882, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==873, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==868, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==831, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==802, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==709, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==572, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==753, "Unnamed members of congress", NOTES)) %>%  
  mutate(NOTES = ifelse(ID==752, "Unnamed members of congress", NOTES)) %>%  
  mutate(NOTES = ifelse(ID==676, "Unnamed members of congress", NOTES)) %>%  
  mutate(NOTES = ifelse(ID==601, "Unnamed members of congress", NOTES)) %>%  
  mutate(NOTES = ifelse(ID==673, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==540, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==568, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==658, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==666, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==37, "Unnamed members of congress", NOTES)) %>%
  mutate(NOTES = ifelse(ID==524, "Unnamed members of congress", NOTES))


##checking for special characters
#data %>% filter(ID==574) %>% select('FROM')

#Failing observations
Unfoundnames <- data %>%
  filter(is.na(last_name),
         is.na(ERROR),
         is.na(NOTES))


# arrange columns for hand coding
data %<>% select(ID, DATE, FROM, everything())

data %<>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", FROM, ignore.case = TRUE), "1", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", FROM, ignore.case = TRUE), "1", TYPE)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMPANIES", FROM, ignore.case = TRUE), "4", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMPANIES", FROM, ignore.case = TRUE), "2", CERTAINTY)) %>%
mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("COMPANIES", FROM, ignore.case = TRUE), "2", ALT_TYPE)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMISSIONER'S TRACKING|COMMISSIONER TRACKING", FROM, ignore.case = TRUE), "5", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HEALTHCARE REFORM|COMMISSIONER'S TRACKING|COMMISSIONER TRACKING", FROM, ignore.case = TRUE), "1", CERTAINTY)) %>%
mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", FROM, ignore.case = TRUE), "6", TYPE)) %>%
mutate(CERTAINTY = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", FROM, ignore.case = TRUE), "1", TYPE)) 

return(data)

}  
