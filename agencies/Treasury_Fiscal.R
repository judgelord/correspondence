# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 702 non-matches on last_name out of 10284

# file.name <- "Treasury_Fiscal" # for testing

#file.name <- "Treasury_Fiscal" ##for testing 13 June 

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
 #Create ID 
data %<>% 
    mutate(ID = row_number())
 
  #Create FROM column 
  data$FROM <- paste(data$AUTHOR.FIRST.NAME, data$AUTHOR.LAST.NAME, sep = " ")

  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
 #Sample Test code
    #sample <- data[sample(1:nrow(data), 3000, replace=FALSE),]
    
    #data <- sample
    
    #Format Typo
    data %<>%
      mutate(FROM = str_replace(FROM, "FEINSTEIN DIANNE", "FEINSTEIN, DIANNE")) %>%
      mutate(FROM = str_replace(FROM, "WM. LACY CLAY", "William Lacy CLAY")) %>%
      mutate(FROM = str_replace(FROM, "BENJAMIN NELSON", "Earl Benjamin NELSON")) %>%
      mutate(FROM = str_replace(FROM, "JGRESHAM BARRETT|GRESHAM BARRETT", "James Gresham BARRETT")) %>%
      mutate(FROM = str_replace(FROM, "CORTEZ MASTO", "Catherine CORTEZ MASTO")) %>%
      mutate(FROM = str_replace(FROM, "RAJA KRISHNAMOORTNI", "RAJA KRISHNAMOORTHI")) %>%
      mutate(FROM = str_replace(FROM, "JAMES SENSEBRENNER", "JAMES SENSENBRENNER")) %>%
      mutate(FROM = str_replace(FROM, "J. LUIS CORREA", "Jose Luis CORREA")) %>%
      mutate(FROM = str_replace(FROM, "GK BUTTERFIELD", "George Kenneth BUTTERFIELD")) %>%
      mutate(FROM = str_replace(FROM, "STEPHEN WOMACK", "Steve WOMACK")) %>%
      mutate(FROM = str_replace(FROM, "CHRIS MUROHY", "Chris MURPHY")) %>%
      mutate(FROM = str_replace(FROM, "JOB HENSARLING", "JEB HENSARLING")) %>%
      mutate(FROM = str_replace(FROM, "MARK ARMODEI", "Mark AMODEI")) %>%
      mutate(FROM = str_replace(FROM, "STEVEN COHEN", "Stephen COHEN")) %>%
      mutate(FROM = str_replace(FROM, "LOUE GOHMERT", "LOUIE GOHMERT")) %>%
      mutate(FROM = str_replace(FROM, "JOHN RUTHEFORD", "JOHN RUTHERFORD")) %>%
      mutate(FROM = str_replace(FROM, "COHEN STEVE", "COHEN, STEVE")) %>%
      mutate(FROM = str_replace(FROM, "T.J. COX", "TJ COX")) %>%
      mutate(FROM = str_replace(FROM, "H. MORGAN GRIFFIN", "H. Morgan GRIFFITH")) %>%
      mutate(FROM = str_replace(FROM, "KAMALA DAVIS", "Kamala HARRIS")) %>%
      mutate(FROM = str_replace(FROM, "MIKE BROWN", "Michael BRAUN")) %>% 
      mutate(FROM = str_replace(FROM, "J. FORBES", "J. Randy FORBES")) %>%
      mutate(FROM = str_replace(FROM, "BONO MACK MARY", "MARY MACK BONO"))
     
    data %<>%
      mutate(FROM = ifelse(str_detect(FROM, "J FORBES") & str_detect(chamber, "House"), str_replace(FROM, "J FORBES", "James FORBES"), FROM)) %>%
      mutate(FROM = ifelse(FROM == "LUJAN GRISHAM", str_replace(FROM, "LUJAN GRISHAM", "Michelle LUJAN GRISHAM"), FROM)) %>%
      mutate(FROM = ifelse(FROM == "X PETERSON" & str_detect(congress, "115") & str_detect(chamber, "House"), str_replace(FROM, "X PETERSON", "Collin PETERSON"), FROM))
    
  #Extract Member names
  data <-  extractMemberName(data,members,"FROM") 
  
  #Check for Duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "))
  
  #ERRORS
  data %<>%
    mutate(ERROR = ifelse(FROM == "FRANK PADAVAN" & congress == 110 & str_detect(chamber, "Senate"), "State Politican", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "KEVIN FROMER|AMIEE SNYDER|AIMEE SNYDER|ELEANOR HOLMES-NORTON|PATRICIA LARKE|MADELEINE BORDALLO|MATT HUTCHINSON|CAROLYN PRICE|MATT HUTCHISON"), "Not Member", ERROR))
  
  
  #Failing observations
  Unfoundnames <- data %>%
  filter(is.na(last_name),
         ! str_detect(FROM, "\\(b\\)\\(6\\) \\(b\\)\\(6\\)|NA NA"),
         str_detect(pattern, "404error"),
         is.na(ERROR))  
 
  
  
  ## Are we sure that we want to delete all of these observations?
  data %<>% 
    filter(! str_detect(FROM, "\\(b\\)\\(6\\) \\(b\\)\\(6\\)|NA NA"))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, chamber, everything())
  
  # apply codebook
  data %<>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("DEBT|CHECK CLAIM|DIRECT DEPOSIT|EFT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("WHY DID I GET CHECK", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("WHY DID I GET CHECK", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) 
  
  
  
  
  
  
}

