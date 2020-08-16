# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 84 out of 1992 not matching

# file.name <- "DOD_OSDJS" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #data$FROM <- gsub("^MOC","", data$FROM)
  data$FROM %<>% str_remove("^MOC") %>% str_squish()
  data$FROM %<>% str_replace("\\.|,", ", ") %>% str_squish() # repace periods with comma space, then remove extra spaces
  data$FROM %<>% str_replace("1", "VI")
  
  # corrections for this script only
  data$FROM %<>% str_replace("BOEHNER, I", "BOEHNER, J") %>% 
    str_replace("MACK, M|MACKM", "BONO, M") %>% 
    str_replace("BOEHNER, I", "BOEHNER, J") %>% 
    str_replace("GALLEGL Y", "GALLEGLY") %>% 
    str_replace("KIRKM", "KIRK, M") %>%
    str_replace("MCHUGRJ|MCHUGR, J", "MCHUGH, J") %>%
    str_replace("LINDERJ", "LINDER, J") %>%
    str_replace("SCHAKOWSKl", "SCHAKOWSKY") %>%
    str_replace("MCMORRIS", "McMORRIS RODGERS") %>%
    str_replace("OBERST AR", "OBERSTAR") %>%
    str_replace("POSEYB", "POSEY, B") %>%
    str_replace("PELOSLN|PELOSL, N", "PELOSI, N") %>%
    str_replace("FILNERB", "FILNER, B") %>%
    str_replace("WA XMAN|WA, XMAN", "WAXMAN") %>% 
    str_replace("CANTORE", "CANTOR, E") %>% 
    str_replace("CARNEYC", "CARNEY, C") %>%
    str_replace("CARTERJ", "CARTER, J") %>% 
    str_replace("DIAZåáBALAR.", "DIAZ-BALART") %>% 
    str_replace(" SCLOSKY P", " VISCLOSKY, P")
  
  
  # replace spaces with comma space
  data$FROM %<>% str_replace(" ", ", ") 
  data$FROM %<>% str_replace(",", ", ") 
  data$FROM %<>% str_replace("(, )+", ", ")
  #data$FROM <- gsub("\\\\1", "VI", data$FROM)

  # fix problems caused by spaces in compound names 
  data$FROM %<>% str_replace("HERSETH, SANDLIN", "HERSETH SANDLIN") %>% 
    str_replace("McMORRIS, RODGERS", "McMORRIS RODGERS") %>% 
    str_replace("JACKSON, LEE", "JACKSON LEE") %>% 
    
    str_replace("VAN, HOLLEN", "VAN HOLLEN") 
  
  
  data$FROM %<>% str_remove_all("\\\\|\\!")
  data$FROM %<>% str_squish()
  
  
  # THERE IS NO CHAMBER 
  # data %<>% mutate(FROM = ifelse(!is.na(chamber), 
  #                                paste(chamber, FROM) %>% 
  #                                  str_replace("House", "Represenative") %>% 
  #                                  str_replace("Senate", "Senator"), 
  #                                FROM))
  # 
  # data %<>% select(-chamber)
  
  
  
  #Extract Member names (New edit from formatLastName)
  data <-  extractMemberName(data,members,"FROM") 

  
  
  #checking for names that are NA
  unfoundnames<- data %>%
    filter(is.na(last_name))
  
  unfoundnames %<>%
    select(ID, DATE, congress, FROM, string, pattern, everything())
  
  unfoundnames %>%  count(FROM, string, sort= T) %>% kable()
  
  unfoundnames$FROM
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM,  everything())
  
  data %<>%
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

