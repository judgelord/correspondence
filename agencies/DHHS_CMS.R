# file.name <- "DHHS_CMS Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name

  data$DATE %<>% as.Date("%m/%d/%y")
  
  noDate <- data %>%
    filter(is.na(DATE))
  
  fullFROM <- data %>%
    filter(! is.na(From))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
    
  data %<>%
    mutate(FROM = ifelse(is.na(From), paste(`From First Name`, `From Last Name`, sep = " "), FROM))
  
  #data <- data[sample(1:nrow(data), 10000, replace=FALSE),]
  
  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Wm\\. Clay", "William CLAY")) %>%
    mutate(FROM = str_replace(FROM, "Robert P\\., Jr\\. Casey", "Robert Casey")) %>%
    mutate(FROM = str_replace(FROM, "Joe, III Manchin", "Joe Manchin"))

  #Extract Member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")  
  
  
  unfoundNamesSample <- data %>%
    filter(is.na(last_name))
  
  return(data)
  
}