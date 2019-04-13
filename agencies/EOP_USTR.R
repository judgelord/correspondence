# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "EOP_USTR" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  names(data)[names(data) == 'Date Received'] <- 'DATE'
  names(data)[names(data) == 'Source'] <- 'FROM'
  names(data)[names(data) == 'Title'] <- 'SUBJECT'
  names(data)[names(data) == 'Signature(s)'] <- 'FROM'
  
  i <- 1
  
  # for (i in 1:nrow(data)) {
  #   print(i)
  #   
  #   if(grepl("^[A-Z]", data$DATE[i])) {
  #       
  #     data$FROM[i-1] <- paste( c(data$FROM[i-1]), ";", data$DATE[i], collapse = " ") 
  #     data <- data[-i,]
  #     i <- i-1
  #     } else{
  #         
  #   }
  #   
  #   
  #   
  # }
  # 
  
  # # create ID variable
  # data$ID <- c(1:nrow(data))
  # #create agency column
  # data$agency <- file.name 
  # 
  # # Format date, year, Congress
  # data$DATE %<>% as.Date("%m/%d/%Y")
  # data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  # data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  # 
  # 
  # data <- extractMemberName(data, members, 'FROM')
  # 
  # 
  # # arrange columns for hand coding
  # data %<>% select(ID, DATE,  FROM,  everything())
}
