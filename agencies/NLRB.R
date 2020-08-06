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
  
  
  # create agency column
  data$agency <- file.name
  
  # remove unwanted rows
  data <- data[-which((is.na(data$FROM)&is.na(data$SUBJECT)&is.na(data$DATE))|
                        data$FROM == "Requestor (Last Name, First Name)"|data$FROM == "Signatories"),]
  data <- data[-which(grepl("Congressional Log|Office of the General", data$DATE)), ]

    
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, ";")) %>% 
    unnest(FROM) 
  
  data$FROM %<>% str_squish()
 
  # Format date, year, Congress, member name etc. 
  data$DATE1 <- ifelse( grepl("/\\w{4}$",data$DATE), data$DATE, NA  )
  data$DATE1 %<>% as.Date("%m/%d/%Y")
  data$DATE2 <- ifelse( grepl("/\\w{2}$",data$DATE), data$DATE, NA  )
  data$DATE2 %<>% as.Date("%m/%d/%y")
  data$DATE <- data$DATE1
  data$DATE <- dplyr::if_else(is.na(data$DATE), data$DATE2, data$DATE)
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  # chamber
  data %<>%
    mutate(chamber = ifelse (grepl("(^S(-| ))|Senator|Sen\\.", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("(^(R|C)(-| ))|Repres|Congress|Rep", FROM), "House", chamber)) 
  
  
  data %<>%
    mutate(FROM = str_replace(FROM, "(^S(-| ))|Senator|Sen\\.", "Senator") %>%
             str_replace("(^(R|C)(-| ))|Repres\\b|Congress\\b|Rep\\b", "Representative") )  
  
  
  data <- extractMemberName(data, members, 'FROM')

  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)  
}

