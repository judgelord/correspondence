 # This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# Duplicate members in some rows needs to be addressed (a few are comma separated)
# Many spelling errors need to be addressed

# source("setup.R")
file.name <- "DOE_FERC Extended" # for testing

clean <- function(file.name) {
 
  data <- gs_title(file.name) %>% gs_read() # get data
  data <- read.csv("DOE_FERC Extended.csv")
  
  # create ID column
  #data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  # data$DATE <- gsub("(^.*\\d{4})\n.*",  '\\1', data$Date)
  #data$date_received <- gsub("(^.*\\d{4})\n(.*)",  '\\2', data$Date)
  # data$DATE %<>% as.Date("%m/%d/%Y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # create FROM column from SUBJECT column
  data$FROM <- data$SUBJECT 
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl("&", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = "&") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], "&"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep("&", data$FROM),] # removes orginal row with all data
  ################
  
  
  
  data <- extractMemberName(data, members, "FROM")
  data$FROM <- data$FROM2
  data %<>% select(-FROM2, -X, -Summary)
  ## extract from text 
  # data %<>% map_if(is.na(FROM2), extractMemberNames) #FIXME need to write function for vector, not df

  # write.csv(data, file = "DOE_FERC Extended.csv")
  
  data %<>%
    mutate(chamber = ifelse(grepl("(Senate|Senator)",Summary), 'Senate', NA)) %>% 
    mutate(chamber = ifelse(grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", Summary), "House", chamber)) %>% 
    mutate(chamber = ifelse(grepl("(Senate|Senator)", data$Summary) &grepl("Represenatative|Representative|US Rep|Congressman|Congresswoman|Congresswomen", data$Summary), 'FIXME', chamber ))
  
  # arrange columns for hand coding
    data %<>% select(ID, DATE, FROM, everything())

  data%<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("ROCKIES EXPRESS PIPELINE|ELECTRIC GENERATOR PROJECT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("COMMENTS OF US SENATOR|REQUEST INFORMATION|SENATOR.*COMMENTS|HEARING|MEETING|US SENATE SUBMITS|OVERSIGHT OF THE|EXPRESSING CONCERNS|SENATE SUBMITS COMMENTS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CITY OF.*APPLICATION|APPLICATION.*CITY OF|ON BEHALF OF.*COUNTY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("NEW COAL", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PUBLIC UTILITIES COMMISSION", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
}


nchar("NA<pagebreak>NA<pagebreak>NA<pagebreak>NA")

data %>% 
  mutate(maybe_cosigned = ifelse(str_detect(SUBJECT, "et al"), "et al",
                                 ifelse(str_detect(SUBJECT, "&"), "&", " neither"))) %>%
  group_by(year, maybe_cosigned) %>% 
  tally %>% 
  ggplot() + 
  aes(x = year, y = n, fill = maybe_cosigned) +
  geom_col() + theme_minimal()
