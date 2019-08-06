# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOL_OWCP" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  colnames(data)[colnames(data) == 'SIMS ID'] <- 'ID'
  
  #format DATE to multiple formats
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #finding NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #create agency column
  data$agency <- file.name
  
  
  ###############
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, "/")) %>% 
    unnest(FROM)
  
  # for(i in 1:nrow(data)){
  #   if(grepl("/", data$FROM[i])) {
  # 
  #     new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "/") + 1))
  #     new$FROM <- unlist(str_split(data$FROM[i], "/"))
  # 
  #     data <- rbind(data, new)
  # 
  #   }
  # }
  # data <- data[-grep("/", data$FROM),] # removes orginal row with all data
  # data <- data[!data$FROM == "",] # removes blank observations
  ################
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Foxx. Virginia", "Foxx, Virginia")) %>% 
    mutate(FROM = str_replace(FROM, "Schumer", "Schumer, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Young, C.W. Bill", "Young, Bill")) %>%
    mutate(FROM = str_replace(FROM, "Young, C.W.", "Young, Bill")) %>%
    mutate(FROM = str_replace(FROM, "Byrd. Robert C.", "Byrd, Robert C.")) %>% 
    mutate(FROM = str_replace(FROM, "Barrett, J. Gresham", "Barrett, James"))
    
    
  data %<>% extractMemberName(members, 'FROM')
 
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)|Senate|Senator", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber)) 
  
  #ERRORS
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Washington, Pauletta D.|Bordallo, Madeleine Z|Norton, Eleanor Holmes|Avella, Tony|Bordallo, Madeleine|Bordallo, Madeleine .|Wilson, Ruth|Knox, Wayne|Storms, Ronda|Shapiro, Alan"), "Not Member", ERROR))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, SUBJECT, everything())
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  return(data)
}
