# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOC_NTIA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- as.Date(data$Date_Created,"%m/%d/%y")
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  names(data)[names(data) == 'From'] <- 'FROM'
  
  data %<>% mutate(FROM = ifelse(is.na(data$FROM)& !is.na(data$First_Name), paste(data$First_Name,data$Last_Name), FROM ) )
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(" and |/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = " and |/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], " and |/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(" and |/", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  ################
  
  
  # member name
  data <-  extractMemberName(data, members,"FROM")
  
  
  
  
  
}






