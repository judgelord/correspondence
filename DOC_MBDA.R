# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOC_MBDA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # create agency column
  data$agency <- file.name
  
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber))
  
  # arrange columns for hand coding
  # data %<>% select(ID, DATE, FROM, SUBJECT, chamber, everything())
  
  
}
