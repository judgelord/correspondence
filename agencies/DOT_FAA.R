# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 # file.name <- "DOT_FAA Sam" # for testing
 

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read()  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE <- multidate(data$DATE, c("%d-%b-%y","%B %d, %Y"))
  
  #checking for dates that are NA
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001

  # Add semi colons in rows with multiple congressman
  data$FROM <- gsub("(Senate|Representatives)  (\\w+,)","\\1;\\2", data$FROM, ignore.case = T)
  data$FROM <- gsub("(Infrastructure|Aviation|Transportation|Technology|Reform)  (\\w+,)","\\1;\\2", data$FROM, ignore.case = T)
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
  mutate(FROM = str_split(FROM, ";")) %>%
  unnest(FROM)
  
 
  #Format Typos
  data %<>%
    mutate(FROM = str_replace_all(FROM, " ,", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, " , ", ", ")) %>%
    mutate(FROM = str_remove_all(FROM, " Jr\\.| JR\\.| Jr\\.,")) %>%
    mutate(FROM = str_replace_all(FROM, ",, ", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, "\\. ", " "))
  
  #Name Format Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Sensenbrenner, F James|Sensenbrenner,, F James|Sensenbrenner,  F James", "Frank SENSENBRENNER")) %>%
    mutate(FROM = str_replace(FROM, "FrR\\/AZ anks, Trent\\.|FrR\\/AZ  anks, Trent\\.", "Trent FRANKS \\/AZ")) %>%
    mutate(FROM = str_replace(FROM, "ClD\\/MO ay, Wm Lacy\\.|Clay, Wm Lacy  D\\/MO|ClD\\/MO  ay, Wm Lacy\\.", "William CLAY \\/MO")) %>%
    mutate(FROM = str_replace(FROM, "WD\\/VA arner, Mark\\.|WD\\/VA  arner, Mark\\.", "Mark WARNER \\/VA")) %>%
    mutate(FROM = str_replace(FROM, "Young, C\\.W Bill|Young, C W Bill|Young, CW Bill", "Charles YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "McKinley, P\\.E\\., David B", "David McKINLEY")) %>%
    mutate(FROM = str_replace(FROM, "Gosar, D\\.D\\.S\\., Paul A", "Paul GOSAR")) %>%
    mutate(FROM = str_replace(FROM, "Conaway, K Michael", "Kenneth CONAWAY")) %<>%
    mutate(FROM = str_replace(FROM, "Johnson, Timothy V", "Timothy V JOHNSON")) %<>%
    mutate(FROM = str_replace(FROM, "Rogers, Mike D\\/AL U.S House of Representatives", "Mike Dennis ROGERS \\/AL U.S House of Representatives")) %>%
    mutate(FROM = str_replace(FROM, "Brownley, Julie", "Julia BROWNLEY")) %>%
    mutate(FROM = str_replace(FROM, "Rogers, Mike D\\/AL U\\.S House of Representatives", "Mike Dennis ROGERS \\/AL U\\.S House of Representatives")) %>%
    mutate(FROM = str_replace(FROM, "Brown, Paul C", "Paul C BROUN")) %>%
    mutate(FROM = str_replace(FROM, "Hastert, J Dennis", "John Dennis HASTERT")) %>%
    mutate(FROM = str_replace(FROM, "Larsen, John", "John LARSON")) %>%
    mutate(FROM = str_replace(FROM, "Lieu, Ted W", "Ted LIEU")) %>%
    mutate(FROM = str_replace(FROM, "Ferguson IV, A Drew|Ferguson, IV, A Drew", "Drew FERGUSON")) %>%
    mutate(FROM = str_replace(FROM, "Amodel, Mark", "Mark AMODEI"))
  
  #Name Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Ros-Lehtinen, Heana", "leana ROS-LEHTINEN")) %>%
    mutate(FROM = str_replace(FROM, "Buschon, Larry", "Larry BUCSHON"))
    
  
  data %<>% extractMemberName(members, 'FROM')
  
  
  #create variable for state
  
  data %<>% 
    mutate(state = ifelse(grepl(".*\\w{1,}(/|-)(\\w{2})( |)($| U\\.S\\.| United)", FROM), gsub(".*\\w{1,}(/|-)(\\w{2})( |)($| U\\.S\\.| United).*", replacement="\\2", FROM), NA))
  data$state = stateFromLower(data$state)
  
  
  # ERROR
  data %<>%
    mutate(ERROR = ifelse(grepl("FAA Employee",FROM, ignore.case = T), "FAA Employee", ERROR))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Fortuno, Luis|Pierluisi, Pedro R|Bordallo, Madeleine Z|Holmes Norton, Eleanor|Norton, Eleanor Holmes|Holmes Norton,  Eleanor"), "Non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Avella, Tony|Local Government|Governor"), "State Politician", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Eck, James T ANG Administrator|Dalrymple, Jack |Heurta, Michael P|Jones,  Stephanie  Department of Transportation|Thomson, Kathryn B  General Counsel  U\\.S Department of Transportation|Bolton, Edward L\\.|Gilligan, Margaret|Jones,  Stephanie Senior Counselor and Chief Opportunitie s Officer|Kurland, Susan L "), "Agency Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Robinson, Russell E\\.|Baker, Mark|Robinson, Russell E Aviation Industry|Beasley, James E\\."), "Non member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Ted LIEU") & congress %in% c(110), "Not yet in congress", ERROR))

  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 

  return(data)
}



