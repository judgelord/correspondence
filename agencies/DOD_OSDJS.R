# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 84 out of 1992 not matching

# file.name <- "DOD_OSDJS" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  data$ID <- c(1:nrow(data))
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  data$FROM <- gsub("^MOC","", data$FROM)
  data$FROM <- gsub("  "," ", data$FROM)
  data$FROM <- gsub("  "," ", data$FROM)
  data$FROM <- gsub("PELOSLN", "PELOSI,N", data$FROM)
  data$FROM <- gsub("\\\\1", "VI", data$FROM)
  
  data$last_name <-  gsub("(.*)(,|\\.)(.*)", "\\1", data$FROM)
  
  #data$last_name <- formatLastName(data, 'last_name')
  
  #Extract Member names (New edit from formatLastName)
  data <-  extractMemberName(data,members,"FROM") 
  

  data %<>%
    mutate(last_name = ifelse(last_name %in% members$last_name, last_name, 
                              gsub("^(\\w+)(\\w| \\w)$", '\\1', last_name)))
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #checking for names that are NA
  unfoundnames<- data %>%
    filter(is.na(last_name))
  
  unfoundnames %<>%
    select(ID, DATE, FROM, SUBJECT, last_name, everything())
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data%<>%
 # mutate(SUBJECT=paste(`Subject Code`,"-",SUBJECT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT SERVICE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT SERVICE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POLICY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POLICY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONTRACTING", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONTRACTING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONGRESSIONAL TRAVEL|COMITY", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONGRESSIONAL TRAVEL|COMITY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(EVENT_NAME = ifelse (!grepl("[A-Z]", EVENT_NAME) & grepl("CONGRESSIONAL TRAVEL", SUBJECT, ignore.case = TRUE), "CONGRESSIONAL TRAVEL", EVENT_NAME)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("HEARING", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "HEARING", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("REQUESTS|REQUEST|ASKS", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "RULE", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("CONCERNS|CONCERN", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "RULE", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("LEGISLATION", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "LEGISLATION", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROPOSAL", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "RULE", POLICY_EVENT)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("URGES", SUBJECT, ignore.case = TRUE) & (TYPE == 5), "RULE", POLICY_EVENT)) 
  
  
  
  data %<>%
    mutate(TYPE = ifelse(powellType == "Constituent Service", 1, TYPE)) %>% 
    mutate(TYPE = ifelse(powellType == "Policy", 5, TYPE)) %>% 
    mutate(TYPE = ifelse(powellType == "Contracting", 2, TYPE)) %>% 
    mutate(TYPE = ifelse(powellType == "Congressional Travel/ Movement", 6, TYPE)) %>% 
    mutate(TYPE = ifelse(powellType == "Not Enough Info", 0, TYPE)) 
  
  return(data)

}

