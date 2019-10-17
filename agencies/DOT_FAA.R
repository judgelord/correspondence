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
  
  data %<>%
    mutate(FROM = str_trim(FROM))
 
  #Format Typos
  data %<>%
    mutate(FROM = str_replace_all(FROM, " ,", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, " , ", ", ")) %>%
    mutate(FROM = str_remove_all(FROM, " Jr\\.| JR\\.| Jr\\.,")) %>%
    mutate(FROM = str_replace_all(FROM, ",, ", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, "\\. ", " ")) %>%
    mutate(FROM = str_replace_all(FROM, "  R\\/", " R\\/")) %>%
    mutate(FROM = str_replace_all(FROM, "  R-", " R-")) %>%
    mutate(FROM = str_replace_all(FROM, "  D\\/", " D\\/")) %>%
    mutate(FROM = str_replace_all(FROM, "  D-", " D-"))
  
  
  #Fix state space
   data %<>%
     mutate(FROM = str_replace(FROM, "Murkowski, Lisa   R\\/KS  United States Senate", "Murkowski, Lisa  R\\/AK  United States Senate")) %>%
     mutate(FROM = str_replace(FROM, "Rogers, Mike  R-MI", "Rogers, Mike R-MI")) %>%
     mutate(FROM = str_replace(FROM, "Rogers, Mike J   R\\/MI", "Rogers, Mike J R\\/MI")) %>%
     mutate(FROM = str_replace(FROM, "Rogers, Mike  D\\/AL", "Rogers, Mike D\\/AL"))
  
  #Match on state
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, "Johnson, Tim D-SD|Johnson, Tim R-SD|Johnson, Tim  D-SD|Johnson, Tim  R-SD"), str_replace(FROM, "Johnson, Tim", "Timothy Peter JOHNSON"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Rogers, Mike D\\/AL"), str_replace(FROM, "Rogers, Mike", "Mike Dennis ROGERS"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Udall, Mark E D\\/NM"), str_replace(FROM, "Udall, Mark E", "Thomas UDALL"), FROM)) %>% #check that this is true
    mutate(FROM = ifelse(str_detect(FROM, "Rogers, Mike R-MI|Rogers, Mike J R\\/MI"), str_replace(FROM, "Rogers, Mike", "Mike J ROGERS"), FROM)) %>%
    mutate(FROM = ifelse(str_detect(FROM, "Murphy, Patrick J D-PA") & congress %in% c(114) & str_detect(SUBJECT, "Florida"), str_replace(FROM, "Murphy, Patrick J D-PA", "Murphy, Patrick E D-FL"), FROM)) %>%
    mutate(FROM = str_replace(FROM, "Murkowski, Lisa  R\\/KS  United States Senate", "Murkowski, Lisa  R/KS  United States Senate R\\/AK United States Senate")) %>%
    mutate(FROM = str_replace(FROM, "Scott, Austin R\\/FL", "Scott, Austin R\\/GA")) %>%
    mutate(FROM = str_replace(FROM, "Tsongas, Niki  D\\/NJ", "Tsongas, Niki  D\\/MA"))
  
  #Match on congress and chamber
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, "Johnson, Tim United States Senate|Johnson, Tim  United States Senate") & congress %in% c(110), str_replace(FROM, "Johnson, Tim", "Timothy Peter JOHNSON"), FROM))
  
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
    mutate(FROM = str_replace(FROM, "Amodel, Mark", "Mark AMODEI")) %>%
    mutate(FROM = str_replace(FROM, "Charles, Schumer", "Charles Schumer")) %>%
    mutate(FROM = str_replace(FROM, "Robert, Menendez ", "Robert Menendez ")) %>%
    mutate(FROM = str_replace(FROM, "Rockefeller, VI, John D", "Rockefeller, John")) %>%
    mutate(FROM = str_replace(FROM, "Schultz, Debbie Wasserman", "Debbie WASSERMAN SCHULTZ")) %>%
    mutate(FROM = str_replace(FROM, "Barrett, J Gresham|Gresham Barrett, J", "James Gresham BARRETT")) %>%
    mutate(FROM = str_replace(FROM, "Bono Mack, Mary", "Mary BONO")) %>%
    mutate(FROM = str_replace(FROM, "Owens, The Honorable Bill", "William OWENS")) %>%
    mutate(FROM = str_replace(FROM, "Cook \\(Ret Col\\.\\), Paul", "COOK, Paul")) %>%
    mutate(FROM = str_replace(FROM, "Lee, Shelia Jackson", "JACKSON LEE, Sheila")) %>%
    mutate(FROM = str_replace(FROM, "Grassley Charles E", "Grassley, Charles E"))
  
  #Name Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Ros-Lehtinen, Heana", "Ileana ROS-LEHTINEN")) %>%
    mutate(FROM = str_replace(FROM, "Buschon, Larry", "Larry BUCSHON")) %>%
    mutate(FROM = str_replace(FROM, "DelBene, Susan ", "DelBene, Suzan ")) %>%
    mutate(FROM = str_replace(FROM, "Shea-Porter, Carl", "Shea-Porter, Carol")) %>%
    mutate(FROM = str_replace(FROM, "Beutler, Jairme Herrera", "Jaime HERRERA BEUTLER")) %>%
    mutate(FROM = str_replace(FROM, "Strivers, Steve", "Steve STIVERS")) %>%
    mutate(FROM = str_replace(FROM, "Hines, Jim", "Himes, Jim")) %>%
    mutate(FROM = str_replace(FROM, "Jackson, Johnny", "ISAKSON, Johnny")) %>%
    mutate(FROM = str_replace(FROM, "Peterson, Gary", "Peters, Gary")) %>%
    mutate(FROM = str_replace(FROM, "Sirens, Albio", "Sires, Albio")) %>%
    mutate(FROM = str_replace(FROM, "Sterns, Cliff", "Stearns, Cliff")) %>%
    mutate(FROM = str_replace(FROM, "Coata, Dan", "Coats, Dan")) %>%
    mutate(FROM = str_replace(FROM, "Shock, Aaron|Scheck, Aaron", "Schock, Aaron")) %>%
    mutate(FROM = str_replace(FROM, "Hartzier, Vicky", "Hartzler, Vicky")) %>%
    mutate(FROM = str_replace(FROM, "Lankford, Janes", "Lankford, James")) %>%
    mutate(FROM = str_replace(FROM, "Walder, Greg", "Walden, Greg ")) %>%
    mutate(FROM = str_replace(FROM, "Gonzales, Charles", "Gonzalez, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Bachus, Max", "Baucus, Max")) %>%
    mutate(FROM = str_replace(FROM, "Ronney, Tom", "Rooney, Tom")) %>%
    mutate(FROM = str_replace(FROM, "Carson, Aaron", "CARSON, Andre")) %>%
    mutate(FROM = str_replace(FROM, "Crapos, Mike", "Crapo, Mike"))
    
    
  
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
  
  data %>%
    filter(ID == 3893) %>%
    select(FROM)
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Sablan, Gregorio Kilili Camacho|Faleomavaega, Eni F|Fortuno, Luis|Pierluisi, Pedro R|Bordallo, Madeleine Z|Holmes Norton, Eleanor|Norton, Eleanor Holmes|Holmes Norton,  Eleanor|Christensen, Donna M"), "Non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Avella, Tony|Local Government|Governor|Hutchinson, Toi|Hueso, Ben|County Executive|Hughes, Shelley|Brown, Alvin"), "State Politician", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Vanderleest, Dirk B |Eck, James T ANG Administrator|Dalrymple, Jack |Heurta, Michael P|Marootian, Jeff|Carraway, Melvin J|Jones,  Stephanie  Department of Transportation|Thomson, Kathryn B  General Counsel  U\\.S Department of Transportation|Bolton, Edward L\\.|Gilligan, Margaret|Jones,  Stephanie Senior Counselor and Chief Opportunitie s Officer|Kurland, Susan L |Bowman, Ben F|Fornarotto, Christa|Haas, Karen L|Burkett, Alex"), "Agency Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Melchor, Ricardo|Dickerson, J Spencer|Leong, Aaron G|Estes, Mark|Williams, Susan|Robinson, Russell E\\.|Baker, Mark|Robinson, Russell E Aviation Industry|Robinson, Russell E   Aviation Industry|Beasley, James E\\.|Benjiman, Moy.|Bauserman, Gilbert L|Rhodes, David Martin\\."), "Non member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Peters, Gary|Ted LIEU|Begich, Mark") & congress %in% c(110), "Not yet in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Joyce, David L|Huffman, Jared|Hudson, Richard") & congress %in% c(110), "Not yet in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Oberstar, James L") & congress %in% c(112), "No longer in congress", ERROR))

  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  nonmembers <- data %>%
    filter(! is.na(ERROR))

  return(data)
}




