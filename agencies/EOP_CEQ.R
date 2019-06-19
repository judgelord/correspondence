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


data <- getFirstLast.Comma(data, col_name = "FROM")



data %<>% select(ID, DATE, FROM, everything())  

Unfoundnames <- data %>%
  extractMemberName(members = members, col_name = "SUBJECT")

Unfoundnames2 <- data %>%
  extractMemberName(members = members, col_name = "FROM")

Unfoundnames2 %<>%
  drop_na(last_name)

notfound <- Unfoundnames2 %>%
  filter(is.na(last_name))

Unfoundnames %<>%
drop_na(last_name)

data %<>%
  full_join(Unfoundnames)

data %<>%
  full_join(Unfoundnames2)

data %<>% filter(! FROM == "")

#Separates first and last name by comma
data %<>%
  mutate(FROM = str_trim(FROM)) %>%
  mutate(FROM = ifelse(! str_detect(FROM, "\\,"), str_replace(FROM, " ", "\\, "), FROM))


datanotfound <- data %>%
  filter(is.na(last_name))

data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Various| Others| Other|"), "Multiple Unnamed Members", NOTES))


data %<>%
  mutate(FROM = ifelse(! str_detect(FROM, "\\,|\\.") & is.na(last_name), casefold(FROM, upper = TRUE), FROM)) %>%
  mutate(last_name = ifelse(! str_detect(FROM, "\\,|\\.") & is.na(last_name), FROM, last_name))

datanotfound2 <- data %>%
  filter(is.na(last_name))

return(data)  
}