# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 291 mismatches


# file.name <- "RRB" # for testing

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
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%d-%b-%y")
  
  #checking for dates that are NA
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for full name
  data$FROM <- gsub("Tanko", "Tonko", data$FROM)
  data$FROM <- ocr.errors(data$FROM)

  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  data %<>% arrange(rev(last_name))
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  
  
  
  
  
return(data)  
  
  
  
}






