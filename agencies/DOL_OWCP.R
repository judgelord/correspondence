# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOL_OWCP" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  colnames(data)[colnames(data) == 'SIMS ID'] <- 'ID'
  
  # format DATE to multiple formats
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #create agency column
  data$agency <- file.name
  
  
  ###############
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl("/", data$FROM[i])) {

      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "/"))

      data <- rbind(data, new)

    }
  }
  data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  #getFirstLast runs better than extractmembername
  
  #data <- extractMemberName(data, members, 'FROM')
 
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)|Senate|Senator", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, SUBJECT, everything())
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  
  
}
