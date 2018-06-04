# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 84 out of 1992 not matching

#file.name <- "DOD_OSDJS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data$FROM <- gsub("^MOC","", data$FROM)
  data$FROM <- gsub("  "," ", data$FROM)
  data$FROM <- gsub("  "," ", data$FROM)
  data$FROM <- gsub("PELOSLN", "PELOSI,N", data$FROM)
  data$FROM <- gsub("\\\\1", "VI", data$FROM)
  
  data$last_name <-  gsub("(.*)(,|\\.)(.*)", "\\1", data$FROM)
  data$last_name <- formatLastName(data, 'last_name')
  
  data$first_initial <- gsub("(.*)(,|\\.)(.*)", "\\3", data$FROM)
  
  data %<>%
    mutate(last_name = ifelse(last_name %in% members$last_name, last_name, 
                              gsub("^(\\w+)(\\w| \\w)$", '\\1', last_name)))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
}