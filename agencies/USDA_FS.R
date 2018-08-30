# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA_FS" # for testing

clean <- function(file.name){
  
  # get data from google drive 
  data <- gs_title(file.name) %>% gs_read() 
  
  # create agency column 
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data$DateSigned %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.integer(substr(DATE,1,4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data$FROM <- (gsub("+","",data$FROM)) # remove +
  data <- data[-which(is.na(data$FROM)&is.na(data$DATE)&is.na(data$Addressee)),]
  
  # create first and last name variables
  data <- extractMemberName(data, members, 'FROM')
  
  
  ### Nearly 1500 names where extractMemberName() didn't match anything. These names were scanned through
  # and none of the names were recognizable as congressman. Possible there were a handful of lesser known
  # representatives excluded, but I think it's unlikely. 
  data %<>%
    mutate(ERROR = ifelse(is.na(data$last_name), "Probably not in congress. Worth checking again.", ERROR))
  
  
  # #preprocess FROM column for creating names variables
  # data %<>%
  #   mutate(FROM = gsub("\\+", replacement= "", FROM))
  # 
  # # create variable for first name
  # data %<>%
  #   mutate(first_name =  gsub(pattern="^(\\w+) .*", replacement = "\\1", FROM)) %>% 
  #   mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name)) %>% 
  #   mutate(first_name =  ifelse(grepl("Butch", first_name), "C.L. 'Butch'", first_name)) %>% 
  #   mutate(first_name = stri_trans_totitle(first_name)) 
  # 
  # 
  # 
  # 
  # # create variable for last name
  # data %<>%
  #   mutate(last_name = gsub(pattern= ".* (\\w+)$", replacement = "\\1", FROM)) %>% 
  #   mutate(last_name = gsub(pattern= ".* (\\w+)-(\\w+)", replacement = "\\1-\\2", last_name)) %>% 
  #   mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)-(\\w+)", replacement = "\\1\\2-\\3", last_name)) %>% 
  #   mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)$", replacement = "\\1\\2", last_name))  %>% 
  #   mutate(last_name = str_to_upper(last_name))

  
  # arrange columns for further hand coding
  data %<>% select(ID, DATE, FROM, everything())
}





