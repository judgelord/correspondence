# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 
# file.name <- "DOC_NOAA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub(" .*","", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  # member name
  data %<>% 
    mutate(SUBJECT = paste(FROM, SUBJECT)) 
  
  
  # errors for non members of Congress
  data %<>%
    mutate(ERROR = ifelse(grepl("^Suzanne George$", data$FROM), "Not in Congress", ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("^AA for Fisheries$", data$FROM), "Not in Congress", ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("^NWS - National Weather Service$", data$FROM), "Not in Congress", ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("^OS-ITA POC Renee Chase$", data$FROM), "Not in Congress", ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("^OCIO NESDIS$", data$FROM), "Not in Congress", ERROR)) %>% 
    mutate(ERROR = ifelse(is.na(data$FROM), "NA FROM information", ERROR)) # No name info in SUBJECT for missing FROMs
  
  # Remove observations that aren't from congressman  
  data <- data[-grep("^(NMFS/F-Assistant Administrator for Fisheries|NOAA/OCAO|OS-Dep/Sec POC|
                       |OS-ESA-POC L Bonney|OS-NIST POC Michelle Harman|OS-Office of The Secretary|
                       |OS-EDA POC Eartha Ball|OS-Executive Secretariat|OS-MBDA POC Pam Cox|
                       |OS-OBL POC Richelle Saunders|OS-OLIA POC M Freeman)$", data$FROM),]
  
  
  data %<>%
      extractMemberName(members, "FROM")
  
  
  
return(data)  
  
}