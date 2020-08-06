# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "NLRB" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  data
  # create agency column
  data$agency <- file.name
  
  # remove unwanted rows
  data %<>% filter(!(is.na(FROM) & is.na(SUBJECT) & is.na(DATE)))
  #data <- data[-which((is.na(data$FROM)&is.na(data$SUBJECT)&is.na(data$DATE))|
  #                      data$FROM == "Requestor (Last Name, First Name)"|data$FROM == "Signatories"),]
  
  data %<>% filter(!str_detect(DATE, "Congressional Log|Office of the General"))

    

  # Format date, year, Congress, member name etc. 
 
  data %<>% mutate(DATE = coalesce(DATE, ResponseDate, `Date Due`))
  data$DATEoriginal <- data$DATE
  data$DATE %<>% str_remove_all(",| .*") %>% str_replace_all("-|\\.", "/")
  data$DATE1 <- ifelse( grepl("/\\w{4}$",data$DATE), data$DATE, NA  )
  data$DATE1 %<>% as.Date("%m/%d/%Y")
  data$DATE2 <- ifelse( grepl("/\\w{2}$",data$DATE), data$DATE, NA  )
  data$DATE2 %<>% as.Date("%m/%d/%y")
  data$DATE3 <- data$DATE %>% str_extract("[0-9]{1,2}(\\.|-|/)[0-9]{1,2}.20[0-9]{2}")
  data$DATE3 %<>% as.Date("%m/%d/%Y")
  
  data$DATE4 <- data$DATE %>% str_extract("[0-9]{1,2}/[0-9]{1,2}/[0:1][1-9]")
  data$DATE4 %<>% as.Date("%m/%d/%y")
  
  data %<>% mutate(DATE = coalesce(DATE3, DATE4, DATE1, DATE2))
  
  NOdate <- filter(is.na(DATE)| DATE < as.Date("2000-01-01") | DATE > as.Date("2020-01-01")) %>%
    select(DATEoriginal, FROM, sort)
  NOdate
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, ";")) %>% 
    unnest(FROM) 
  
  data$FROM %<>% str_squish()
  
  
  # chamber
  data %<>%
    mutate(chamber = ifelse (grepl("(^S(-| ))|Senator|Sen\\.", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("(^(R|C)(-| ))|Repres|Congress|Rep", FROM), "House", chamber)) 
  
  
  data %<>%
    mutate(FROM = str_replace_all(FROM, "(^S(-| ))|Senator|Sen\\.", "Senator ") %>%
             str_replace_all("(^(R|C)(-| ))|Repres\\b|Congress\\b|Rep\\b", "Representative ") )  
  
  data$FROM %<>% str_replace_all("\\.|-", " ") %>% str_squish()
  
  data <- extractMemberName(data, members, 'FROM')

  Unfoundnames <- data %>% filter(is.na(last_name), 
                                  is.na(ERROR))  %>% 
    count(FROM, string, congress, pattern)
  Unfoundnames
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)  
}

