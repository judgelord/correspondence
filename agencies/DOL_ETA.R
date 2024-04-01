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
  
  #removal of surplus columns
  
  #data <- data[ -c(21:22) ]
  
  data %<>% 
    mutate(FROM = `Originator`,
           DATE = `Date Entered`,
           SUBJECT = Subject)
  
  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  data %<>%
    mutate(FROM = str_to_lower(FROM))
  
  #remove "(cong)" string from names in the FROM column
  
  string <- ("(cong)")
  data$FROM %<>%
    str_remove_all(string)
  
  string2 <- ("(sen)")
  data$FROM %<>%
    str_remove_all(string2)
  
  string2_5 <- ("chair")
  data$FROM %<>%
    str_remove_all(string2_5)
  
  
  string3 <- ("[()]")
  data$FROM %<>%
    str_remove_all(string3)
  
  
  
  #custom string changes for individuals - these are redundant with latest version of setup.r
  
  data %<>%
    mutate(FROM = str_to_lower(FROM)) %>%
  mutate(FROM = str_replace(FROM, "gonzales, charles a", "gonzalez, charles a")) %>%
  mutate(FROM = str_replace(FROM, "evans, lane", "lane evans")) %>%
  mutate(FROM = str_replace(FROM, "mccarthy, devin", "McCarthy, Kevin")) %>%
  mutate(FROM = str_replace(FROM, "albrecht, kevin", "Moore, Dennis")) %>%
  mutate(FROM = str_replace(FROM, "burr, ricahrd", "Burr, Richard")) %>%
  mutate(FROM = str_replace(FROM, "waltz, timothy j", "Walz, Tim")) %>%
  mutate(FROM = str_replace(FROM, "altimire, jason", "Altmire, Jason")) %>%
  mutate(FROM = str_replace(FROM, "levin, sender", "Levin, Sander")) %>%
  mutate(FROM = str_replace(FROM, "rothmn, steven r", "Rothman, Steven")) %>%
  mutate(FROM = str_replace(FROM, "mchenry, patrick", "patrick mchenry")) %>%
  mutate(FROM = str_replace(FROM, "durbin, richard", "richard durbin")) %>%
  mutate(FROM = str_replace(FROM, "shimkus, john", "john shimkus")) %>%
  mutate(FROM = str_replace(FROM, "goode, virgil h", "virgil goode")) %>%
  mutate(FROM = str_replace(FROM, "spector, arlen", "Specter, arlen")) %>%
  mutate(FROM = str_replace(FROM, "gilman, benjamin a", "benjamin gilman")) %>%
  mutate(FROM = str_replace(FROM, "latourett, steven c", "steve latourette")) %>%
  mutate(FROM = str_replace(FROM, "gordner, john r", "john gardner")) %>%
  mutate(FROM = str_replace(FROM, "matsu, doris", "doris matsui")) %>%
  mutate(FROM = str_replace(FROM, "harken, tom", "tom harkin")) %>%
  mutate(FROM = str_replace(FROM, "bocieri, john", "john boccieri")) %>%
  mutate(FROM = str_replace(FROM, "doggertt, lloyd", "lloyd doggett")) %>%
  mutate(FROM = str_replace(FROM, "clay lacy, wm", "bw clay")) %>%
  mutate(FROM = str_replace(FROM, "emanuel, rahm", "rahm emanuel")) %>%
  mutate(FROM = str_replace(FROM, "kaine, tim", "timothy kaine")) %>%
  mutate(FROM = str_replace(FROM, "norton, eleanor holmes", "Eleanor Holmes Norton")) %>%
  mutate(FROM = str_replace(FROM, "ciciline, david", "david cicilline")) %>%
  mutate(FROM = str_replace(FROM, "shock, aaron", "aaron schock")) %>%
  mutate(FROM = str_replace(FROM, "lowery, nita m", "nita lowey")) %>%
  mutate(FROM = str_replace(FROM, "costello, jerry f", "jerry costello")) %>%
  mutate(FROM = str_replace(FROM, "warren, elizabetlh ", "elizabeth warren")) %>%
  mutate(FROM = str_replace(FROM, "fox, virginia", "virginia foxx")) %>%
  mutate(FROM = str_replace(FROM, "kingston, jack", "jack kingston")) %>%
  mutate(FROM = str_replace(FROM, "murry, patty", "patty murray")) %>%
  mutate(FROM = str_replace(FROM, "levin, carl m", "carl levin")) %>%
  mutate(FROM = str_replace(FROM, "pomeo, mike", "mike pompeo")) %>%
  mutate(FROM = str_replace(FROM, "shcrader, kurt", "kurt schrader")) %>%
  mutate(FROM = str_replace(FROM, "mikulsi, barbara", "barbara mikulski")) %>%
  mutate(FROM = str_replace(FROM, "harry, cresent", "cresent hardy")) %>%
  mutate(FROM = str_replace(FROM, "raskin, jamie", "jamie raskin")) %>%
  mutate(FROM = str_replace(FROM, "brooker, cory", "cory booker")) %>%
  mutate(FROM = str_replace(FROM, "stabenbow, debbie", "debbie stabenow"))
 

#custom strings for groups
  
  data %<>%
    mutate(FROM = str_to_lower(FROM)) %>%
    mutate(FROM = str_replace(FROM, "lincoln/pryor/berry/snyder/boozman/ross", 
                              "blanche lincoln, mark pryor, senator berry, senator snyder, 
                              senator boozeman, senator ross")) %>%
    mutate(FROM = str_replace(FROM, "kennedy/kerry/capuano/lynch", "senator kennedy, 
                              senator kerry, senator capuano, senator lynch")) %>%
    mutate(FROM = str_replace(FROM, "kennedy/kerry/frank/mcgovern", "senator kennedy, 
                              senatory kerry, senator frank, senator mcgovern")) %>%
    mutate(FROM = str_replace(FROM, "reid/ensign/berkley/porter/heller", "senator reid, 
                              senator ensign, senator berkley, senator porter, senator heller")) %>%
    mutate(FROM = str_replace(FROM, "miller, g/woolsey/lofgren/berman", "senator miller, senator woolsey, 
                              senator lofgren, senator berman")) %>%
    mutate(FROM = str_replace(FROM, "carson/burton/visclosky /buyer", "senator carson, 
                              senator burton, senator visclosky, senator buyer")) %>%
    mutate(FROM = str_replace(FROM, "burton/visclosky /buyer/sounder", "senator burton, 
                              senator visclosky, senator buyer, senator sounder")) %>%                           
    mutate(FROM = str_replace(FROM, "alexander/corker/davis/blackburn", "senator alexander, 
                              senator corker, senator davis, senator blackburn")) %>%
    mutate(FROM = str_replace(FROM, "steinberg, darrell (cong) & bass, karen (cong)", 
                              "senator steinberg, senator bass,")) %>%
    mutate(FROM = str_replace(FROM, "markey/frank/neal/delauro", "senator markey, 
                              senator frank, senator neal, senator delauro")) %>%                           
    mutate(FROM = str_replace(FROM, "dayton, mark (gov)/ klobuchar/ franken/ nolan", 
                              "senator klobuchar, senator franken, senator nolan"))
                              
                              
                              
                                                          
  
  library(legislators)
  data %<>% 
    legislators::extractMemberName("FROM",
                                   congress = "congress")
  
  
  return(data)
}

if(F){
  data %>% count(is.na(icpsr)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE) %>% print(n = 540)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()
  
  print(distinct(data$FROM))
  
}

