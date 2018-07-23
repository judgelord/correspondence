# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOL_MSHA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data)) 
  
  # # create agency column
  data$agency <- file.name
  # 
  # # Format date, year, Congress, member name etc. 
  data$DATE <-  as.Date(data$DATE, "%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  # 
  # 
  # ###############    
  # # Creates duplicate rows for lines with multiple representatives
  # for(i in 1:nrow(data)){
  #   if(grepl(";|&| and |/", data$FROM[i])) {
  #     
  #     new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";|&| and |/") + 1))
  #     new$FROM <- unlist(str_split(data$FROM[i], ";|&| and |/"))
  #     
  #     data <- rbind(data, new)
  #     
  #   }
  # }
  # data <- data[-grep(";|&| and |/", data$FROM),] # removes orginal row with all data
  # data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  # data <- data[!data$FROM == "",] # removes blank observations
  # 
  # ################
  # 
  # # create variable for full name
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)|Senat", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong|\\(Song.\\)|Congressman", FROM), "House", chamber)) 
  
  
  data <- getFirstLast.Comma(data, 'FROM')
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, first_name, last_name, chamber, SUBJECT, everything())
  
  #
  # 
  # data%<>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  #   mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  #   mutate(NOTES = ifelse (!grepl("[0-9]", NOTES) & grepl("PENSION", SUBJECT, ignore.case = TRUE), "98% SURE THESE SUBJECTS REPRESENT CERTAIN PEOPLE WORKING FOR THE COMPANIES AND NOT THE COMPANIES THEMSELVES, BUT CAN'T SAY WITH ABSOLUTE CERTAINTY", NOTES)) %>%
  #   mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  #   mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONCERNING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
  # 
  # 
  
  
  
}






