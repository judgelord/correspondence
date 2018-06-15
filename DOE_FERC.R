# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Duplicate members in some rows needs to be addressed
# Many spelling errors need to be addressed


file.name <- "DOE_FERC" # for testing

clean <- function(file.name) {
 
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID column
  names(data)[names(data) == 'X1'] <- 'ID'
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("(^.*\\d{4})\n.*",  '\\1', data$Date)
  #data$date_received <- gsub("(^.*\\d{4})\n(.*)",  '\\2', data$Date)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data$FROM <- data$SUBJECT
  data <- extractMemberName(data, members, "FROM")
  
#   data %<>%
#     mutate(Summary = gsub(pattern = "(.*)(Represenative|Representativess)(.*)", replacement = '\\1Representatives\\3', Summary) )
#   
#    data %<>%
#     mutate(FROM = gsub(pattern = '.*(US|United States) (Representative|Senator|Senate|Congress) (\\w+ \\w+|\\w+ \\w \\w+|\\w+ \\w\\. \\w+|\\w \\w+ \\w+|\\w\\. \\w+ \\w+).*',
#                        replacement = "\\3", data$Summary)) %>% 
#     mutate(FROM = gsub(pattern = '.*(Congressman|Congresswoman|Congresswomen|Representative|Senator|Representatives|Chairman) (\\w+ \\w+|\\w+ \\w \\w+|\\w+ \\w\\. \\w+|\\w \\w+ \\w+|\\w\\. \\w+ \\w+).*', replacement = "\\2", FROM))
# #remove Represenatatives to see duplicates in rows
#    
#    
#   data %<>%
#     mutate(first_name =  gsub(pattern="^(\\w+) .*", replacement = "\\1", FROM)) %>% 
#     mutate(first_name =  gsub(pattern="^(\\w). (\\w+) .*", replacement = "\\1. \\2", first_name))
#   data <- formatFirstName(data)
#   
#   
#   data %<>%
#     mutate(last_name = gsub(pattern= ".* (\\w+)$", replacement = "\\1", FROM)) %>% 
#     mutate(last_name = gsub(pattern= ".* (\\w+)-(\\w+)", replacement = "\\1-\\2", last_name)) %>% 
#     mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)-(\\w+)", replacement = "\\1\\2-\\3", last_name)) %>% 
#     mutate(last_name = gsub(pattern= ".* (\\w')(\\w+)$", replacement = "\\1\\2", last_name))
#     
#   data <- formatLastName(data)
  
  
  data %<>%
    mutate(chamber = ifelse(grepl("(Senate|Senator)",Summary), 'Senate', NA)) %>% 
    mutate(chamber = ifelse(grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", Summary), "House", chamber)) %>% 
    mutate(chamber = ifelse(grepl("(Senate|Senator)", data$Summary) &grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", data$Summary), 'FIXME', chamber ))
  
  # arrange columns for hand coding
    data %<>% select(ID, DATE, FROM, everything())

  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE))
  
}




