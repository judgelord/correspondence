# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# source("setup.R")
#file.name <- "DOL_ETA Rochelle" # for testing

clean <- function(file.name) {
  data_raw <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data_raw$LetterID <- 1:nrow(data_raw)
  
  # select distinct observations 
  data_distinct <- data_raw %>% select(-LetterID) %>% distinct()
  
  ##########################################################
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data_raw) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
  data %<>% 
    rename(FROM = `Originator`,
           DATE = `Date Entered`)
  
  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #custom string changes for individuals
  data %<>% str_replace_all(FROM, "gonzales, charles a", "gonzalez, charles a")
  data %<>% str_replace_all(FROM, "evans, lane", "EVANS, Lane Allen")
  data %<>% str_replace_all(FROM, "mccarthy, devin", "McCarthy, Kevin")
  data %<>% str_replace_all(FROM, "otter, c l \"butch\"", "butch otter")
  data %<>% str_replace_all(FROM, "albrecht, kevin", "Moore, Dennis")
  data %<>% str_replace_all(FROM, "burr, ricahrd", "Burr, Richard")
  data %<>% str_replace_all(FROM, "waltz, timothy j", "Walz, Tim")
  data %<>% str_replace_all(FROM, "altimire, jason", "Altmire, Jason")
  data %<>% str_replace_all(FROM, "levin, sender", "Levin, Sander")
  data %<>% str_replace_all(FROM, "rothmn, steven r", "Rothman, Steven")
  data %<>% str_replace_all(FROM, "mchenry, patrick", "patrick mchenry")
  data %<>% str_replace_all(FROM, "durbin, richard", "richard durbin")
  data %<>% str_replace_all(FROM, "shimkus, john", "john shimkus")
  data %<>% str_replace_all(FROM, "goode, virgil h", "virgil goode")
  data %<>% str_replace_all(FROM, "spector, arlen", "Specter, arlen")
  data %<>% str_replace_all(FROM, "gilman, benjamin a", "benjamin gilman")
  data %<>% str_replace_all(FROM, "latourett, steven c", "steve latourette")
  data %<>% str_replace_all(FROM, "gordner, john r", "john gardner")
  data %<>% str_replace_all(FROM, "matsu, doris", "doris matsui")
  data %<>% str_replace_all(FROM, "harken, tom", "tom harkin")
  data %<>% str_replace_all(FROM, "bocieri, john", "john boccieri")
  data %<>% str_replace_all(FROM, "doggertt, lloyd", "lloyd doggett")
  data %<>% str_replace_all(FROM, "clay lacy, wm", "bw clay")
  data %<>% str_replace_all(FROM, "emanuel, rahm", "rahm emanuel")
  data %<>% str_replace_all(FROM, "kaine, tim", "timothy kaine")
  data %<>% str_replace_all(FROM, "norton, eleanor holmes", "Eleanor Holmes Norton")
  data %<>% str_replace_all(FROM, "ciciline, david", "david cicilline")
  data %<>% str_replace_all(FROM, "shock, aaron", "aaron schock")
  data %<>% str_replace_all(FROM, "lowery, nita m", "nita lowey")
  data %<>% str_replace_all(FROM, "costello, jerry f", "jerry costello")
  data %<>% str_replace_all(FROM, "warren, elizabetlh ", "elizabeth warren")
  data %<>% str_replace_all(FROM, "fox, virginia", "virginia foxx")
  data %<>% str_replace_all(FROM, "kingston, jack", "jack kingston")
  data %<>% str_replace_all(FROM, "murry, patty", "patty murray")
  data %<>% str_replace_all(FROM, "levin, carl m", "carl levin")
  data %<>% str_replace_all(FROM, "pomeo, mike", "mike pompeo")
  data %<>% str_replace_all(FROM, "shcrader, kurt", "kurt schrader")
  data %<>% str_replace_all(FROM, "Mikulsi, barbara", "barbara mikulski")
  data %<>% str_replace_all(FROM, "harry, cresent", "cresent hardy")
  data %<>% str_replace_all(FROM, "raskin, jamie", "jamie raskin")
  data %<>% str_replace_all(FROM, "brooker, cory", "cory booker")
  data %<>% str_replace_all(FROM, "stabenbow, debbie", "debbie stabenow")
 
  #custom string changes for groups
  
  
  
  
  library(legislators)
  data %<>% 
    legislators::extractMemberName("FROM",
                                   congress = "congress")
  
  
  return(data)
}

if(F){
  data %>% count(is.na(Organization)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE) %>% print(n = 194)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()
}

print(distinct(data$FROM))
