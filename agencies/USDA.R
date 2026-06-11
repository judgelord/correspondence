# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA" #for testing

clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  
  
  # create agency column 
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #Fill NA dates with Date Signed
  data %<>%
    mutate(tempDATE = `Date Signed`) 
  data$tempDATE <- gsub("/201", "/1", data$tempDATE) 
  data$tempDATE <- gsub("/200", "/0", data$tempDATE)
  data$tempDATE %<>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = if_else(is.na(DATE), tempDATE, DATE))
  
    
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

data$FROM <- ifelse( grepl("Ben|E. B",data$FROM)&grepl("Nelson",data$FROM), "Earl Nelson", data$FROM)

data %<>%
  mutate(FROM = str_replace(FROM, "\\.\\. ", ". "))

#From Member staff
data %<>%
  mutate(FROM = str_replace(FROM, "Whitney Verett", "Office of Mike Dennis Rogers, Legislative Director Whitney Verett")) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Office of Mike Dennis Rogers, Legislative Director Whitney Verett"), "From Member Staff", NOTES))

#Format typos
data %<>%
  mutate(FROM = str_replace(FROM, "Kay B. Hutchison", "Kathryn HUTCHISON")) %>%
  mutate(FROM = str_replace(FROM, "E. Benjamin Nelson|Ben Nelson", "Earl Nelson")) %>%
  mutate(FROM = str_replace(FROM, "C \"Bill\" W. Young", "Charles YOUNG")) %>%
  mutate(FROM = str_replace(FROM, "M. Michael Rounds", "Marion ROUNDS")) %>%
  mutate(FROM = str_replace(FROM, "Margaret W. Hassan", "Margaret Wood HASSAN")) %>%
  mutate(FROM = str_replace(FROM, "Earl \\(Buddy\\) L. Carter", "Earl CARTER")) %>%
  mutate(FROM = str_replace(FROM, "John \\(Jay\\) D. Rockefeller", "John D. Rockefeller")) %>%
  mutate(FROM = str_replace(FROM, "Kristen E. Gilliboard", "Kirsten GILLIBRAND")) %>%
  mutate(FROM = str_replace(FROM, "Glenn \\(GT\\) Thompson|Glenn \\(CT\\) Thompson", "Glenn THOMPSON")) %>%
  mutate(FROM = str_replace(FROM, "J. Luis. Correa", "Jose Luis CORREA")) %>%
  mutate(FROM = str_replace(FROM, "Marie K. Hirono", "Mazie K. Hirono")) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Donald M. Payne") & str_detect(congress, "114"), str_replace(FROM, "Donald M. Payne", "Donald PAYNE"), FROM))

# apply extractmembername from legislators package 
data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")

# old ID still used in some places
if(!"ID" %in% names(data)){
  data %<>% mutate(ID = data_id)
}

# arrange columns for hand coding
data %<>%  select(ID, DATE,  FROM, everything())


#Check for duplicates
sample2data<- data

sample2data %<>%
  group_by(ID, SUBJECT, DATE) %>%
  mutate(n = n(),
         last_name = str_c(last_name, collapse = "; ")) %>%
  distinct()


#Membership Errors
NonMembers <- data$FROM %>%
  str_detect("Larry Taylor|Home In Partnership|Cape and Islands Community Develpoment|Gregg Engles|Gregg L\\. Engles|Gregg Leslie|Ray Souza|Robyn O'Brien|Roger Thomas|Sonny Perdue|Calvin Covington|David M\\. Gibbons|David M\\. Pomerantz|Doug Maddox|Alicia Molt|Alyssa Kennedy|Andrew Zabel|Bill Northey|Charles W\\. Bryant|Cheyenne Clements|Conae Black|Thyen|Daniel Wunderlich|Dave Chapman|DeLisa Lay|William H\\. Wigton|Wayne Palla|Tim Nisly|Thomas Mumey|Stephen P\\. Ashkin|Stephen Pearce|Shelley Hearne|Ron McCormick|Rreginald Kerns|Rudolph C\\. Cane|Sam Casella|Scott Simon|Shawna Johnson|Dennis Fife|Dennis Webb|DuBoise White|Errol Rice|Eunice Beal|G\\. Joe Lyon|Joe Lyon|Gail Watson\\. Chlang|Garry McGrath|Gerald Heatwole|J\\. Pat Mohan|James Ricky Williams|James Ricky\\. Williams|Jeff Johnson|Jeff Schmidt|JON CASPERS|Jonathan L\\. Healy|Judy Sanchez|Kelly Rudd|Ken Nobis|Kevin Phillips|Louis R\\. Zemek|Martha Torres|Melissa Greenbacker|Michael Doctor|Michael Platt|Mike and Kathy Poff|Patricia D\\. Stroup|Randy Mooney|Randy Shuring|Robert Redding|Robert Starr|Roger L\\. Richardson|Peter Sorenson|Nancy Sutley|Michael L\\. Bruhn|Mary Dean\\. Eckrote|Marc Brinkmeyer|Lyle Peterson|John W\\. Oliver|John M\\. Meyer|Jodi Kuhn|Jerry Nelson|Jerry C\\. Washburn|James Dain|James D\\. Wilson|James D\\. Wilson|Michael J\\. Schewel|Michael Lewis|Michael P\\. Botticelli|Robert Manchin|Lance Price|Kimberly Pitts|Julie Decker|John E\\. Townsend|Gregory Mignon|John Triune|Michael L. Young|Glenn Simon|Mike Strain|Parks Shackelford")


StatePoliticians <- data$FROM %>%
  str_detect("Roger Allbee|Scott Walker|C\\. W\\. Van Arsdale|Charles M\\. Brunner|Daniel Snarr|Dave Heinman|Luis G\\. Fortuno|Russell C\\. Redding|Sandra B\\. Cunningham|Luis G\\. Fortuno|Jim Lykam|Ned Norris|JoAnn B\\. Seghini|John Laird|Jennifer Gonzalez-Colon|Kate Brown|Kenneth F\\. Lowe, Jr\\.|Mike Brubaker")


NonVotingMember <- data$FROM %>%
  str_detect("Pierluisi, Pedro R\\.|Fortuno, Luis|Bordallo, Madeleine Z\\.|Bordallo, Madeleine \\.|Christensen, Donna M\\.|Sablan, Gregorio Kilili Camacho|Gregorio Sablan|Madeleine Bordallo")

data %<>%
  mutate(ERROR = ifelse(str_detect(FROM, "Trumka, Richard L\\."), "AFL-CIO President", ERROR)) %>%
  mutate(ERROR = ifelse(StatePoliticians, "State Politician", ERROR)) %>%
  mutate(ERROR = ifelse(NonMembers, "Non Member", ERROR)) %>%
  mutate(ERROR = ifelse(NonVotingMember, "Non Voting Member", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Eva Clayton|Eva M\\. Clayton") & congress %in% 112, "No longer in congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Eva Clayton|Eva M\\. Clayton") & congress %in% 113, "No longer in congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Dale Kildee") & congress %in% 114, "No longer in congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Frank H\\. Murkowski") & congress %in% 111, "No longer in congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Lynn Jenkins") & congress %in% 110, "Not yet in congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Kika de la Garza") & congress %in% 110, "No longer in congress", ERROR))


Unfoundnames <- data %>%
  filter(is.na(last_name),
         is.na(ERROR))
data %>%
  filter(ID == 2846) %>%
  select(FROM)


  
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
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Nutrition|NUTRITION|SNAP|WIC|FOODSTAMP|FOOD STAMP|LUNCH|LABEL|REGULATION", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("FORESTRY|Forestry", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ENVIRONMENT|Environment|WATER", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>% 
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GRANT|Grant", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Research|RESEARCH", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Price Support", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Price Support", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONTRACT|PURCHASE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Food|FOOD|DRUG|Drug|LABELING|PESTICIDE", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("Animal|ANIMAL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("MEAT|DAIRY|FRUIT|POULTRY|INSECT|Trees|Plants|Cotton|Nuts|BEEF|Potato|CROP|FUEL|MARKETING|VEGETABLE|LIVESTOCK|SUGAR|WHEAT|CORN", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("FARM CREDIT|FARM PR", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("NOMINATION", SUBJECT, ignore.case = TRUE), "Nomination", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SOIL", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CIVIL RIGHTS", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DISASTER ASSISTANCE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NUTRITION ASSISTANCE", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("HOUSING", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EDUCATION", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>% 
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("TELECOM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%      #SPACE seems to relate to office space/office closings
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SPACE|CLOSING", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPORTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse(grepl("Reports", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FERTILIZER", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FERTLIZER", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FOREIGN REL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TRAVEL", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TRAVEL", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PERSONNEL", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%   #NOT QUITE SURE WHAT IS MEANT BY "PERSONNEL" BUT IT SHOWS UP A LOT
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PERSONNEL", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FIRE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%      #FIRE PREVENTION
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FIRE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%      
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PUB REL|PUBLIC RELATIONS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%        #THIS WOULD BE THE CASE IF "PUB REL" ENTAILED MEETING W/ CONSTITUENT GROUPS. CANNOT TELL IF THAT IS THE CASE THOUGH
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("OUTREACH", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE))    



return(data)
}






 