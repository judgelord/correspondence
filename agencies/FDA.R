# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FDA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # remove NA rows
  data <- data[-which(is.na(data$FROM)),]
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE  <- gsub(" .*", "", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # paste all relevent info into subject col
  data %<>% 
    mutate(SUBJECT = paste(SUBJECT)) 
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl("Senate|SENATE|Senator|SENATOR|MAJORITY LEADER", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("House|HOUSE|Representative|REPRESENTATIVE|REPRESENTATIAVE|^REP ", FROM), "House", chamber))
  
  # state
  # data$state <- gsub(" ","", data$state)
  
 
  # Add semi colons in rows with multiple congressman
  data$FROM <- gsub("(.*?)(REPRESENTATIVES|SENATOR|OF THE UNITED STATES|UNITED STATES SENATE|SENATE) (\\w+)",'\\1\\2; \\3',data$FROM, ignore.case = T)

 
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";|\\[norg\\]|norgl", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";|\\[norg\\]|norgl") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";|\\[norg\\]|norgl"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";|\\[norg\\]|norgl", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  data <- data[-grep("^(Originator/ Org|Constituent|\\[norg\\]|\\[norg\\] \\[norg\\]|Corr. Date:|Due Date:|Signature Level:|Source:|Status Date:)$",data$FROM),] # removes observations
  
  # clean from
  data %<>%
    mutate(FROM = (str_remove(FROM, " UNITED.*| SENATE.*| HOUSE.*|\\[no org\\] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et.al|et. al|Honorable|\\[No Org\\]|Mr.|\\[NO ORG\\]| House of Representatives| OFFICE.*| \\[no org\\]| \\[no orgl|
                              ASSOCIATE.*| SENATOR.*| HOUSE OF REPRESENTATIVES.*| ASSOCIATE COMMISSIONER.*| HOUSE OF REPRESENTATIVES.*| CONGRESS|fno orgl |Ino orgl |\\[no orgl | U.S. Senate|FDA/OC/OPP/| G FDA/OPPLA/OL/|Mr. |Ms. |Dr. |Inc | Honorable| Sen")))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "Warner, Mark R US", "Warner, Mark R")) %>%
    mutate(FROM = str_replace(FROM, "BROWN, SHERROD .", "Brown, Sherrod")) %>%
    mutate(FROM = str_replace(FROM, "STOCKMAN, STEVE (R) TEXAS", "Stockman, Steve"))
  
  #data$FROM <- gsub(" UNITED.*| SENATE.*| SENATOR.*| HOUSE.*|[no org] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et. al|et.al","", data$FROM)
  
data$FROM <- gsub("^ ","", data$FROM)
  

  ################
  
  

  # extract member names
  data %<>%
    getFirstLast.Comma("FROM")
  
  data %<>%
    filter( ! str_detect(FROM, "\\[no org\\]")) %>%
    filter( ! str_detect(FROM, "Rec/Create Date:")) %>%
    filter( ! str_detect(FROM, "Office:")) %>%
    filter( ! str_detect(FROM, "U.S.-China Economic, .")) %>%
    filter( ! str_detect(FROM, "E&C Committee, U. S. Congress"))
  
 unfoundnames<- data %>%
   filter(is.na(last_name))
  
  
  
  
}
