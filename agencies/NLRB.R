# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "NLRB" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column
  data$agency <- file.name
  
  # remove unwanted rows
  data <- data[-which((is.na(data$FROM)&is.na(data$SUBJECT)&is.na(data$DATE))|data$FROM == "Requestor (Last Name, First Name)"),]
  data <- data[-which(grepl("Congressional Log|Office of the General", data$DATE)), ]
  data$ID <- seq(1:nrow(data))
  
 
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
  
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  
  # chamber
  data %<>%
    mutate(chamber = ifelse (grepl("(^S(-| ))|Senator|Sen\\.", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("(^(R|C)(-| ))|Repres|Congress|Rep\\.", FROM), "House", chamber)) 
  
  
  # fix FROM 
  data$FROM <- gsub("Senator |Congressman ", "", data$FROM)
  data$FROM <- gsub(",", ".", data$FROM)
  data$FROM <- gsub("Ti m |Tim ", "", data$FROM)
  data$FROM <- gsub("l l|ll", "", data$FROM)
  data$FROM <- gsub("Bun-", "Bun", data$FROM)
  data$FROM <- gsub("Ban-", "Ban", data$FROM)
  data$FROM <- gsub("C.Johnson", "C. Johnson", data$FROM)
  data$FROM <- gsub("y' ", "y ", data$FROM)
  data$FROM <- gsub("A1 ", "Al ", data$FROM)
  data$FROM <- gsub(" 1. ", " L. ", data$FROM)
  data$FROM <- gsub("Hany", "Harry", data$FROM)
  data$FROM <- gsub("John Abney Culberson", "John Culberson", data$FROM)
  
  # adds "ll" to names that were misread and other ocr errors
  data$FROM <- ocr.errors(data$FROM)
  # names 
  
  data %<>%
    mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name)) %>%
    mutate(last_name = ifelse(grepl("M. Tia Johnson", FROM), "Johnson", last_name))
  
  
  # #create variable for first name of the Sen/Rep
  # data %<>%
  #   mutate(first_name = gsub(pattern = "(Congressman|Senator) (\\w+).*", replacement = "\\2", x=FROM)) %>% 
  #   mutate(first_name = ifelse(is.na(title), NA, first_name)) %>% 
  #   mutate(first_name = ifelse(grepl("M. Tia", FROM), "M. Tia", first_name))
  # 
  # 
  # 
  # #create variable for last name of the Sen/Rep
  # data %<>%
  #   mutate(last_name = gsub(pattern = ".* (\\w+)$", 
  #                           replacement = "\\1", x=FROM)) %>% 
  #   mutate(last_name = ifelse(grepl("Jason Cha", FROM), "Chaffetz", last_name)) %>%
  #   mutate(last_name = ifelse(grepl("O'Rourke", FROM), "O'Rourke", last_name)) %>% 
  #   mutate(last_name = ifelse(is.na(title), NA, last_name))
  # 
  # data$last_name %<>% toupper()
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
  
  data %<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NONPROFIT|JEWISH", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ON BEHALF OF CON|CONSTITUENT|CUSTOMER IS|SENTRI|PERMANENT RESIDENCE|REGARDING HER STATUS|REGARDING HIS STATUS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ON BEHALF OF CON|CONSTITUENT|CUSTOMER IS|SENTRI|PERMANENT RESIDENCE|REGARDING HER STATUS|REGARDING HIS STATUS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("FIRE DEPARTMENT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FIRE DEPARTMENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("REQUESTS INFORMATION|HEARING|DELAYED PROCESSING|PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "5", TYPE))  %>% 
    mutate(CERTAINTY = ifelse(!grepl("[0-9]", CERTAINTY) & grepl("REQUESTS INFORMATION|HEARING|DELAYED PROCESSING|PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>% 
    mutate(POLICY_EVENT = ifelse(!grepl("[0-9]", POLICY_EVENT) & grepl("REQUESTS INFORMATION", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse(!grepl("[0-9]", TYPE) & grepl("GRANT PROGRAM|GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse(!grepl("[0-9]", CERTAINTY) & grepl("GRANT PROGRAM|GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("GRANT REQUEST|CITY OF", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING", SUBJECT, ignore.case = TRUE), "HEARING", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROPOSED RULEMAKING", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANKS", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANKS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ADOPTION|REQUESTS STATUS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ADOPTION|REQUESTS STATUS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
}

