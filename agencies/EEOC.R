#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EEOC Rochelle" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() 
  
  data %<>%
    mutate(
      FROM = str_squish(FROM),
      `Addressee Street 1` = str_squish(`Addressee Street 1`),
      `Addressee State` = str_squish(`Addressee State`),
      FROM = paste0(`Addressee Street 1`, " ", FROM, ", ", `Addressee State`) %>% 
        str_remove_all("N/A")
    )
  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()

  #create agency column
  data$agency <- file.name


  ## not needed?
  #data$DATE %>% str_to_sentence()

  #Format Date
  data$DATE %<>% as.Date("%d-%b-%y")
  
  #Check for NA Dates
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
 
  #Extract member names from SUBJECT

  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  Unfoundnames %>% 
    group_by(FROM) %>% 
    summarise(congress = str_c(congress, collapse = ";")) %>% distinct()  #%>% kable
  
  
  return(data)
  
}
  