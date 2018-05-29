# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DHS_ICE" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # ID variable
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$Date %<>% as.Date("%m/%d/%y")
  colnames(data)[colnames(data) == 'Date'] <- 'DATE'
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- ifelse(is.na(data$'Member/Committee (HOR)'),  data$"Member/Committee (Senate)", 
                      data$'Member/Committee (HOR)')
 
  # create variable for first and last name
  data <- getFirstLast.Comma(data, "FROM")
  
  
  # create chamber variable
  data$chamber <- ifelse(is.na(data$'Member/Committee (HOR)')& !is.na(data$'Member/Committee (Senate)'),
                         "Senate", NA )
  data$chamber <- ifelse(is.na(data$'Member/Committee (Senate)')& !is.na(data$'Member/Committee (HOR)'),
                         "House", data$chamber )
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM,  chamber, everything())
}