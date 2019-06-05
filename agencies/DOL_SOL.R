# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "DOL_SOL" # for testing



clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  names(data)[names(data) == 'SIMS ID'] <- 'ID'
  
  
   #create agency column
  data$agency <- file.name 

  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
 
  ############### 
  #Filter FROM for / , & looking for observations with multiple authors 
  
  #Filter FROM to find only the data that includes "\\/|&" and has multiple authors
#sampledata1<- data %>% 
  #filter(str_detect(FROM, "\\/|&"))

  #Creates duplicate rows for line with multiple representatives to check code
  #sampledata1 %>% 
  #  mutate(FROM = str_split(FROM, "\\/|&")) %>%
   # unnest(FROM) 
  
  #Final Version with full data splits data on / and & to account for multiple authors
data %<>%
    mutate(FROM = str_split(FROM, "\\/|&")) %>%
    unnest(FROM)
#Will want to split on ";" as well
  
  ################
  
  #data <- getFirstLast.Comma(data, 'FROM')

  

  #arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
}
