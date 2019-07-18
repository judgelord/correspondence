# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "NASA" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # format DATE to multiple formats
  data$DATE <- multidate(data$DATE, c("%d-%b-%y", "%b %d,%Y"))
   
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # create LetterID variable
  data$LetterID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name

 
  
  #Makes note for multiple authors
  data %<>%
    mutate(NOTES = ifelse(str_detect(chamber, "HOUSE AND SENATE"), "Multiple members", NOTES))
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  
  data %<>%
    mutate(FROM = str_split(FROM, ",")) %>%
    unnest(FROM)
  

  data$FROM <- gsub("House|Senate|Incoming", "", data$FROM, ignore.case = TRUE)
  ################
  
  
  # chamber
  data$chamber <- ifelse(data$chamber == "HOUSE", 'House', data$chamber)
  data$chamber <- ifelse(data$chamber == "SENATE", "Senate", data$chamber)
  
  #Create ID variable
  data$ID <- c(1:nrow(data))
  
  # preprocess
  data %<>%
    mutate(FROM = str_remove(FROM, "INFORMATION "))
  
  data$FROM <- gsub("(^| )(EB|E\\.B\\.) ", "Eddie ", data$FROM)
  
  
  #Making chamber NA for "HOUSE AND SENATE"
  is.na(data$chamber) <- data$chamber == "HOUSE AND SENATE"
  
  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))
  
  #Paste chamber into FROM column
  data %<>%
    mutate(FROM = ifelse(! str_detect(FROM, " ") & str_detect(chamber, "House"), paste("Representative", FROM, sep = " "), FROM )) %>%
    mutate(FROM = ifelse(! str_detect(FROM, " ") & str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM ))
  
  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "LUHAN", "LUJAN"))
  
  #Format Typos
  data %<>%
    mutate(FROM = ifelse(FROM == "JACKSON LEE", str_replace(FROM, "JACKSON LEE", "Sheila JACKSON LEE"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "VAN HOLLEN", str_replace(FROM, "VAN HOLLEN", "Christopher VAN HOLLEN"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "WATSON COLEMAN", str_replace(FROM, "WATSON COLEMAN","Bonnie WATSON COLEMAN"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "WASSERMAN SCHULTZ", str_replace(FROM, "WASSERMAN SCHULTZ", "Debbie WASSERMAN SCHULTZ"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "MOORE CAPITO", str_replace(FROM, "MOORE CAPITO", "Shelley Moore Capito"), FROM)) %>%
    mutate(FROM = ifelse(FROM == "LUJAN. GRISHAM|LUJ��N GRISHAM", str_replace(FROM, "LUJAN. GRISHAM|LUJ��N GRISHAM","Michelle LUJAN"), FROM))
   # mutate(FROM = ifelse(FROM == "Representative COBURN", ))
  
  NOChamber <- data %>%
    filter(is.na(chamber))
  
  #Extract members in FROM
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "BILL NELSON") & is.na(last_name), "Bill Nelson Duplcate", NOTES))


  data$last_name <- gsub("^ |^  | $|  $", "", data$last_name)
  #data <- data[!data$last_name == "",] # removes blank observations
  
  data %<>%
    mutate(ERROR = ifelse(grepl("^(AND|STATE)$",FROM), 'Inspect', ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, SUBJECT, first_name, last_name, everything())
  
  
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           str_detect(pattern, "404error"),
           is.na(NOTES))
  
  #FOIA
data %<>%
  mutate(NOTES = ifelse(FROM == "Representative ROGERS", "Multiple Rogers FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(FROM == "JAMES PAUL", "FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(FROM == "Representative MARTINEZ", "FOIA", NOTES)) %>%
  mutate(NOTES = ifelse(FROM == "Representative THOMPSON", "Multiple Thompson's FOIA", NOTES))

#Check NAs after merge  
#unmatched <- d %>%
# filter(is.na(bioname))
  
  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|LAUNCH PASSES|EMPLOYEE SEEKS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
}
