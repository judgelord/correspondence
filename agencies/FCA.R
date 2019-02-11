# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Less than 100 obs
# needs work

# file.name <- "FCA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  #create agency column
  data$agency <- file.name 
  
  data <- data[-which(is.na(data$SUBJECT)),]
  data$DATE <-  gsub("(..\\/..\\/....) .*", "\\1", data$SUBJECT)
  data$DATE %<>% as.Date("%m/%d/%Y")

  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
 # data$FROM <- gsub("^.*\\/.... .*(Senator|Sen\\.|Congressman|Rep|Rep\\.|Cong|Cong\\.|Congress) (\\w{+}) .*$", '\\2', data$SUBJECT)
   data$FROM <- gsub("^.*(Senator|Sen\\.|Congressman|Rep|Rep\\.|Cong|Cong\\.|Congress) (\\w{+}) .*$", '\\2', data$SUBJECT)
  
  data$FROM <- gsub("'s", "", data$FROM)
  # creat variable for first and last name
  data <- extractMemberName(data, members, 'SUBJECT')
  
  data$last_name <- ifelse(grepl("^[A-Za-z]+$", data$FROM), formatLastName(data, 'FROM'), data$last_name)
  data$first_name <- data$first_name <- addFirst(data$first_name,data$last_name)
  
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senate|Senator", SUBJECT), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("Congress|Cong |Cong\\.|Rep |Rep\\.|Represe|House", SUBJECT), "House", chamber)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  
}
