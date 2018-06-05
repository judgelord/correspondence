# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information



# Spelling Errors and name errors from duplicating rows, need to fix


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
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";|&| and |/", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";|&| and |/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";|&| and |/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";|&| and |/", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  
  ################
  
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  data %<>%
    mutate(last_name = ifelse(grepl("^\\w+$",data$FROM2), formatLastName(data,'FROM2'), last_name))
  
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)", FROM), "House", chamber)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, chamber, everything())
  
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VETERAN NEEDS ASSISTANCE|WRONGFUL|TERMINATE|USERRA VIOLATION|COMPLAINT|ASSISTANCE|UNFAIR|SUPPORT APPLICATION|GRANT APPLICATION|DISCRIMINATION|REEMPLOYMENT|REINSTATEMENT|PENSION|TREATMENT|APPLICATION|ACTIVE DUTY|HIS|REQUEST|USERRA QUESTIONS|VIOLATIONS|VIOLATED|USERRA RIGHTS|SEEKING EMPLOYMENT|SEEKS EMPLOYMENT|EMPLOYER|BENEFITS|QUESTIONS REGARDING|USERRA", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VETERAN NEEDS ASSISTANCE|WRONGFUL|TERMINATE|USERRA VIOLATION|COMPLAINT|ASSISTANCE|UNFAIR|SUPPORT APPLICATION|GRANT APPLICATION|DISCRIMINATION|REEMPLOYMENT|REINSTATEMENT|PENSION|TREATMENT|APPLICATION|ACTIVE DUTY|HIS|REQUEST|USERRA QUESTIONS|VIOLATIONS|VIOLATED|USERRA RIGHTS|SEEKING EMPLOYMENT|SEEKS EMPLOYMENT|EMPLOYER|BENEFITS|QUESTIONS REGARDING|USERRA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))  %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LEGISLATION|OPPOSE|SUPPORT CONTINUED|SEEKING FUNDING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LEGISLATION|OPPOSE|SUPPORT CONTINUED|SEEKING FUNDING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("LEGISLATION|OPPOSE", SUBJECT, ignore.case = TRUE), "LEGISLATION", EVENT_NAME)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("GRANT PROPOSAL", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("VETERANS RIGHTS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DOL PROGRAMS|SUPPORT VOA|SUPPORT FOR THE|SUPPORT FUND", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DOL PROGRAMS|SUPPORT VOA|SUPPORT FOR THE|SUPPORT FUND", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))%>%
  mutate(EVENT_NAME = ifelse (!grepl("[0-9]", EVENT_NAME) & grepl("DOL PROGRAMS|SUPPORT VOA|SUPPORT FOR THE|SUPPORT FUND", SUBJECT, ignore.case = TRUE), "PROGRAM SUPPORT", EVENT_NAME))  %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SUPPORT FOR BUSINESS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SUPPORT FOR BUSINESS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))

  
  
  
  
  
  
  
  
  
  
  
  
}
