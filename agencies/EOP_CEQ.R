# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EOP_CEQ" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  #Create ID  
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc.

  data$DATE %<>% as.Date("%d-%b-%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

#chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen\\.|Senator|Sen |Senate- ") & ! str_detect(FROM, "Member of Congress|Congressman"),
                            "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Member of Congress|Congressman") & ! str_detect(FROM, "\\(Sen|Sen\\.|Senator"),
                            "House", chamber))
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "committee|Committee"), "Committee", NOTES))
 
   #Split
  data %<>%
    mutate(FROM = str_split(FROM, "\\,| and|\\/|\\&")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen\\.|Senator|Senate- |Senate Majority Leader ") & ! str_detect(FROM, "Member of Congress|Congressman"),
                            "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Member of Congress|Congressman|House |Congresswoman |Rep\\. |Rep |SoH: |House- | - House|Representative |Congress of the United States|Reps.") & ! str_detect(FROM, "\\(Sen|Sen\\.|Senator"),
                            "House", chamber))
  
data %<>%
  mutate(FROM = str_remove_all(FROM, "Coastal States Caucus|Arizona Delegation|Senate|SD|-Land Conservation Caucus|US|AK|NH|US of|United States|U.S. |et al.|WA|from|US of|IL|men|II|CA Congreesmen|AK Reps|Hispanic Caucus Institute|Arizona Delegation|Congresional Hispanic Caucus Inst|Sen\\.|Senator|Senate- |Senate Majority Leader|Member of Congress|Congressman|House |Congresswoman |Rep\\. |Rep |SoH: |House-| - House|Representative |Congress of the United States|Hon. |\\'s Office|Sen |United States Senate|US Senate|the Hon |Reps.|Congressional|Congress|of Reps|Representative |Representatives|Majority Leader |-|Reps | \\(White Referral\\)|Members of Coingress |Congres "))

#Trim White Space
data %<>%
  mutate(FROM = str_trim(FROM))

#Chamber errors
data %<>%
  mutate(chamber = ifelse(str_detect(FROM, "Capps|Welch"), str_replace(chamber, "Senate", "House"), chamber))

#Name Typos
data %<>%
  mutate(FROM = str_replace(FROM, "Henry Reid", "Harry REID")) %>%
  mutate(FROM = str_replace(FROM, "Don You", "Don YOUNG")) %>%
  mutate(FROM = str_replace(FROM, "DiazBalart", "DIAZ-BALART")) %>%
  mutate(FROM = ifelse(FROM == "Capp", str_replace(FROM, "Capp", "CAPPS"), FROM)) %>%
  mutate(FROM = str_replace(FROM, "Tom Coburm", "Tom Coburn")) %>%
  mutate(FROM = str_replace(FROM, "Feinsteinn", "Feinstein")) %>%
  mutate(FROM = str_replace(FROM, "Bluauer", "BLUMENAUER")) %>%
  mutate(FROM = str_replace(FROM, "Fleischmann \\(White Referral\\)", "Fleischmann"))

#Match on Chamber
data %<>%
  mutate(FROM = ifelse(str_detect(FROM, "Timothy Johnson") & congress == 112, str_replace(FROM, "Timothy Johnson", "Tim V JOHNSON"), FROM))

#Paste chamber into FROM
data %<>%
  mutate(FROM = ifelse(! str_detect(FROM, " ") & str_detect(chamber, "House"), paste("Representative", FROM, sep = " "), FROM )) %>%
  mutate(FROM = ifelse(! str_detect(FROM, " ") & str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM ))

#Extract members in FROM
data <- extractMemberName(data, members, 'FROM')


data %<>% select(ID, DATE, FROM, everything())  

#FOIA List
data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Representative Miller"), "Multiple Miller's FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Senator Kirk"), "Multiple Kirk's FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Representative Thompson"), "Multiple Thompson's FOIA", NOTES)) %>% #Members of ocean policy task force may not need to FOIA
  mutate(NOTES = ifelse(str_detect(FROM, "Representative DIAZ-BALART"), "Multiple Diaz-Balart's FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Representative McCarthy"), "Multiple McCarthy's FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Representative Price"), "Multiple Price's FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Senator Nelson"), "Multiple Nelson's FOIA", NOTES))

#Duplicate List
data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Donna Edwards") & is.na(last_name), "Donna Edwards Duplicate", NOTES)) %>%
  mutate(NOTES = ifelse(str_detect(FROM, "Edward J Markey") & is.na(last_name), "Markey Duplicate", NOTES))

#Not members
data %<>%
  mutate(ERROR = ifelse(str_detect(FROM, "Darrell Steinberg|Peter Umhofer"), "Not Member", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Kim Carr"), "Australian Politician", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Representative Granholm|Dale Zorn|Kevin Ranker"), "State Politician", ERROR))

data %<>% 
  filter(! FROM == "" & ! SUBJECT == "")

#Unfoundnames
Unfoundnames <- data %>%
  filter(is.na(last_name),
         is.na(NOTES),
         is.na(ERROR))



data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Various| Others| Other|"), "Multiple Unnamed Members", NOTES))



#Check after run through merge
#Unfoundnames <- d %>%
#filter(is.na(bioname))

return(data)  
}