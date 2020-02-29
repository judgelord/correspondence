# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOL_MSHA Hope" # for testing
 
clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # # create agency column
  data$agency <- file.name
  
  data$DATEoriginal <- data$DATE
  # # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>%
    mutate(tempDATE = `DueDate`) 
  data$tempDATE <- gsub("/201", "/1", data$tempDATE) 
  data$tempDATE <- gsub("/200", "/0", data$tempDATE)
  data$tempDATE %<>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = if_else(is.na(DATE), tempDATE, DATE))
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data %<>% mutate(FROM = ifelse(grepl("^69",data$FROM), data$Organization, data$FROM))
  
  
  data %<>% 
    mutate(FROM = str_split(FROM, "/|;|&| and ")) %>% 
    unnest(FROM)
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)|Senat", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong|\\(Song.\\)|Congressman", FROM), "House", chamber)) 
  
  data %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House") & !str_detect(FROM, ","), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate") & !str_detect(FROM, ","), paste("Senator", FROM, sep = " "), FROM))
  
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, "Johnson, Tim \\(Sen\\)") & congress %in% 112, str_replace(FROM, "Johnson, Tim \\(Sen\\)", "Timothy Peter JOHNSON"), FROM)) %>%
    mutate(FROM = str_replace(FROM, "Capito, Shelley Moore \\(Cong\\)", "Shelley Moore Capito"))
  
  # # create separate dataset with for names with only last name
  # data2 <- data[grepl("^\\w+$", data$FROM),]
  # # remove these observations from the original
  # data <- data[!grepl("^\\w+$", data$FROM),]
  # 
  # # Format last_name column in dataset 2 
  # data2$last_name <- data2$FROM
  # data2$last_name <- formatLastName(data2, 'last_name')
  # # add first names where applicable
  # data2$first_name <- addFirst(data2$first_name,data2$last_name)
  # 
  # 
  # # merge the two separated datasets
  # data <- full_join(data,data2)
  # 
  data <-  extractMemberName(data,members,"FROM") 


  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, first_name, last_name, chamber, SUBJECT, everything())
  
  
  # data%<>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  #   mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  #   mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "98% SURE THESE SUBJECTS REPRESENT CERTAIN PEOPLE WORKING FOR THE COMPANIES AND NOT THE COMPANIES THEMSELVES, BUT CAN'T SAY WITH ABSOLUTE CERTAINTY", NOTES)) %>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
return(data)  
  
}






