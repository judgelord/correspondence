# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# duplicates members in some rows, needs to be fixed


#file.name <- "DOL_VETS" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #rename ID column and remove duplicated observations
  colnames(data)[colnames(data) == 'SIMS ID'] <- 'ID'
  data <- data[!duplicated(data[,c('ID')]),]  
  
  
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, chamber, everything())
  
  
}