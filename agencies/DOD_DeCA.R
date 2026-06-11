# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


 #170 out of 190 matching. No first name, state, or chamber information. 

 # file.name <- "DOD_DeCA Devin" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #remove unwanted rows
  data <- data[-which(-is.na(data$FROM)& is.na(data$'CNTL NO')),]

  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # correct typos 
  #FIXME with more purrr
  for (i in 1:dim(typos)[1]){
    r <- typos$correct[i]
    p <- typos$typos[i]
    
    # Fix name typos
    data %<>% 
      # find common typos
      mutate(string = FROM %>% purrr::map_chr(str_replace, 
                                              pattern = p, 
                                              replacement = r %>% paste("")))
  }
  
  # create variable for  last name
  data$last_name <- formatLastName(data, 'string')
  
  data$last_name %>% 
    str_replace("OROURKE", "O'ROURKE") %>% 
    str_replace("G NGREY", "GINGREY") %>% 
    str_remove(" .*")
  

  # add first name column
  data %<>% add_first()
  
  #inspect
  paste(data$first_name, data$last_name, data$FROM, sep = "<--")
  
  data  %<>% mutate(FROM = paste(first_name, FROM) %>% str_remove("NA "))
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  Unfoundnames %>% select(congress, FROM) %>% distinct() #%>% kable()
  
  Unfoundnames %>% filter(congress == 0)

  data %>% filter(FROM =="WALZ")

  return(data)  
}