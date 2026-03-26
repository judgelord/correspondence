# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# source("setup.R")
#file.name <- "DOL_OICA" # for testing

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
    rename(FROM = `NAME`)
  
  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Indivdual line/member string changes
  
  data %<>%
    mutate(FROM = str_to_lower(FROM)) %>%
    mutate(FROM = str_replace(FROM, "rand, paul", "gonzalez, charles a")) %>%
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
    mutate(FROM = str_replace(FROM, "shimkus, john", "john shimkus"))
  
  
  
  #custom string changes for groups
  
  #custom strings for groups
  
  data %<>%
    mutate(FROM = str_to_lower(FROM)) %>%
    mutate(FROM = str_replace(FROM, "ramstad, jim (cong) & kennedy, patrick (cong)", 
                              "jim ramstad, patrick kennedy"))
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  return(data)
}

if(F){
  data %>% count(is.na(icpsr)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE) %>% print(n = 194)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()
}

print(distinct(data$FROM))
  
