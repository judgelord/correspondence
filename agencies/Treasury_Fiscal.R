# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 702 non-matches on last_name out of 10284

# file.name <- "Treasury_Fiscal" # for testing

#file.name <- "Treasury_Fiscal" ##for testing 13 June 

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
 #Create ID 
data %<>% 
    mutate(ID = row_number())
 
  #Create FROM column 
  data$FROM <- paste(data$AUTHOR.FIRST.NAME, data$AUTHOR.LAST.NAME, sep = " ")

  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
 #Sample Test code
    sample <- data[sample(1:nrow(data), 3000, replace=FALSE),]
    
    data <- sample
    
    #Format Typo
    data %<>%
      mutate(FROM = str_replace(FROM, "FEINSTEIN DIANNE", "FEINSTEIN, DIANNE"))
    
  #Extract Member names
  data <-  extractMemberName(data,members,"FROM") 
  
  #Check for Duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "))
  
  
  #Failing observations
  Unfoundnames <- data %>%
  filter(is.na(last_name),
         ! str_detect(FROM, "\\(b\\)\\(6\\) \\(b\\)\\(6\\)|NA NA"))  
 
  
  
  ## Are we sure that we want to delete all of these observations?
  data %<>% 
    filter(! str_detect(FROM, "\\(b\\)\\(6\\) \\(b\\)\\(6\\)|NA NA"))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())
  
  # apply codebook
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("WHY DID I GET CHECK", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("WHY DID I GET CHECK", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
  
  
  
  
}

