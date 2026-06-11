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
    mutate(FROM = str_replace(FROM, "Capito, Shelley Moore \\(Cong\\)", "Shelley Moore Capito")) %>%
    mutate(FROM = str_replace(FROM, "Harkin Tom", "Harkin, Tom")) %>%
    mutate(FROM = str_replace(FROM, "Johann6, Mike \\(Sen\\)", "Johanns, Mike")) %>%
    mutate(FROM = str_replace(FROM, "Rand, Paul \\(Sen\\)", "Paul, Rand")) %>%
    mutate(FROM = str_replace(FROM, "Senator  Johanns \\(Senators\\)|Senator Johanns \\(Sens\\)", "Mike JOHANNS")) %>%
    mutate(FROM = str_replace(FROM, "Aderholt Robert B \\(Cong\\,", "Aderholt, Robert")) %>%
    mutate(FROM = str_replace(FROM, "Johnson, Tim \\(Cong\\.\\)", "Timothy V JOHNSON")) %>%
    mutate(FROM = str_replace(FROM, "Johnson, Tim \\(Sen\\.\\)|Johnson, Tim \\(Sen\\)", "Timothy Peter JOHNSON")) %>%
    mutate(FROM = str_replace(FROM, "Lugren, Daniel E. \\(Cong\\)", "Lungren, Daniel")) %>%
    mutate(FROM = str_replace(FROM, "Palmar, Gary J \\(Cong\\)", "Palmer, Gary J \\(Cong\\)")) %>%
    mutate(FROM = str_replace(FROM, "Thompson, I Glenn \'GT\' \\(Cong\\)", "THOMPSON, Glenn")) %>%
    mutate(FROM = str_replace(FROM, "Shuster\\. Bill", "Shuster, Bill")) %>%
    mutate(FROM = str_replace(FROM, "Whltehouse, Sheldon", "WHITEHOUSE, Sheldon")) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Representative  Harris \\(Congs\\)") & congress %in% 114, str_replace(FROM, "Representative  Harris \\(Congs\\)", "HARRIS, Andy"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Senator Timothy J\\. \\(Sen\\)") & congress %in% 112, str_replace(FROM, "Senator Timothy J\\. \\(Sen\\)", "Timothy Peter JOHNSON"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Representative Markwayne \\(Cong\\)"), str_replace(FROM, "Representative Markwayne \\(Cong\\)", "Representative MULLIN"), FROM)) %>%
    mutate(FROM = str_replace(FROM, "Zlnke, Ryan \\(Cong\\)", "Zinke, Ryan"))
  
  
  #Fix Chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Rahall, Nick") & chamber == "Senate", str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Shelley Moore Capito") & congress %in% 114 & chamber == "House", str_replace(chamber, "House", "Senate"), chamber))
  
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
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  

  
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, first_name, last_name, chamber, SUBJECT, everything())
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Horsford, Steven A\\. \\(Sen\\.\\)") & congress %in% 112, "Not yet in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Cousins, Steven N\\.|Cousins, Steven N|Gordon, Robert|Navarro-Cabrer, NildaM\\.|Coull1na, Steven N\\.|Cousins, Sloven N|cousins, Steven N\\.|Barber, Elizabeth|Wooten, Ronald|Lewis, Elliot|Beener, George R\\.|Bennazar, Zuquelra, A.J. \\(Allorneyo\\)|Fernandez- Martinez, Alfredo|Kecojevic, Vladislav|Freedberg, Abraham|Gordon, Roberl|Gordon, R\\.|Hambly, Gary"), "Non member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Schermer, Barry s\\.|Schermer, Barry|Schermer, Barry S \\(Judge\\)|Schermer, Barry S\\. \\(Judge\\)|Fleissig, Audrey G\\. \\(US District Judge\\)|Fleissig, Audrey G\\. \\(US Dlstnct Judge\\)"), "judge", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Pallasch, John"), "assistant secretary", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Cuomo, Andrew M\\.\\(Gov\\.\\)"), "state legislator", ERROR))
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           !is.na(FROM),
           !is.na(DATE)) 
  
  # data%<>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  #   mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  #   mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "98% SURE THESE SUBJECTS REPRESENT CERTAIN PEOPLE WORKING FOR THE COMPANIES AND NOT THE COMPANIES THEMSELVES, BUT CAN'T SAY WITH ABSOLUTE CERTAINTY", NOTES)) %>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  
  
  
return(data)  
  
}






