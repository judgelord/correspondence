# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 702 non-matches on last_name out of 10284

# file.name <- "Treasury_Fiscal" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data %<>% 
    mutate(ID = row_number())
  
  data$FROM <- paste(data$AUTHOR.FIRST.NAME, data$AUTHOR.LAST.NAME, sep = " ")
  
  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # ###############    
  # # Creates duplicate rows for lines with multiple representatives
  # FIXME 
  # This cane be done better with str_split(FROM) %>% unnest(FROM)
  # for(i in 1:nrow(data)){
  #   if(grepl(" AND ", data$FROM[i])) {
  #     
  #     new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = " AND ") + 1))
  #     new$FROM <- unlist(str_split(data$FROM[i], " AND "))
  #     
  #     data <- rbind(data, new)
  #     
  #   }
  # }
  # data <- data[-grep(" AND ", data$FROM),] # removes orginal row with all data
  data %<>% distinct()
  ################
  
  data <-  extractMemberName(data,members,"FROM")
  
  ## Are we shure that we want to delete all of these observations?
  data %<>% filter(!str_detect(FROM, "\\(b\\)\\(6\\) \\(b\\)\\(6\\)|NA NA"))
  
  
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

