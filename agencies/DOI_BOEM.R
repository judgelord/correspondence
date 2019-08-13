# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOI_BOEM Aaron" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  # Remove duplicated rows
  #data <- data[!duplicated(data[,c('ID')]),]  
  
  # create easier to recognize ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc.
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  # add in notes if a number of unspecified congressman contributed
  data %<>%
    mutate(NOTES = ifelse(grepl("other|members", FROM, ignore.case = TRUE), paste(NOTES, FROM), NOTES))
  
  ###     ###     ###
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";"))
      
      data <- rbind(data, new)
      
    }
  }
  #String Split for Multiple Members
  data %<>%
    mutate(FROM = str_split(FROM, ";")) %>%
    mutate(FROM = str_remove_all(FROM, "MOC ")) %>%
    unnest(FROM)
  
  #data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  #data$FROM <- gsub("^ ", "", data$FROM)
  ###     ###     ###
  
  # create variable for first and last name
  #data <- getFirstLast.Comma(data, "FROM")
  
  #getFirstLast runs better than extractMemberName
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  return(data)
}