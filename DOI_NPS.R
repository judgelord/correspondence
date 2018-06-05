# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 232 out of 1403 last names not matched. Wait for better data from agency. 

   file.name <- "DOI_NPS" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # Remove duplicated rows
  data <- data[!duplicated(data[,c('ID')]),]  
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # create variable for last name
  data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, M.C.,|, III,|, P.E.,| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$FROM2)
  data$last_name <- formatLastName(data, 'FROM2')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
}