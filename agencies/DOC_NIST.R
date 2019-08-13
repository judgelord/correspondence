# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOC_NIST" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("/201", "/1", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl("Rep. |Rep |Cong", FROM), "House", NA)) %>%
    mutate(chamber = ifelse(grepl("^Sen. |^Sen ", FROM), "Senate", chamber))
  
  
  data %<>%
    mutate(ERROR = ifelse(is.na(data$FROM), "NA FROM information", ERROR))
  
  
  # member name
  data %<>% extractMemberName(members,"FROM")
  
  
  
return(data)  
  
}