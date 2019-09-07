# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOT_FHWA" # for testing

clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  data %<>%
    group_by(SUBJECT, DATE, `Control Number`) %>%
    mutate(n = n(),
           FROM = str_c(FROM, collapse = ". ")) %>%
    ungroup() %>%
    distinct()
  
  #Check Group
  # data %>%
  #   filter(`Control Number` == "FHWA-150213-003") %>%
  #   select(FROM)
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m/%d/%y","%Y-%m-%d"))
  
  data %<>%
    mutate(tempDATE = str_extract(X7, "[0-9][0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9][0-9]/[0-9][0-9]|[0-9]/[0-9]/[0-9][0-9]|[0-9][0-9]/[0-9]/[0-9][0-9]")) 
  data$tempDATE %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = if_else(is.na(DATE), tempDATE, DATE))
  
  NoDate <- data %>%
    filter(is.na(DATE))
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  data %<>%
    mutate(FROM = str_replace(FROM, "e'", "e"))
  
  
  #create variable for chamber
  data %<>%
    mutate(chamber = ifelse (grepl("United States Senate|Senate", Organization), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("U.S. House of Representatives|House|Representatives", Organization), "House", chamber)) 
  
  
  #Paste multiple authors into FROM column
   data %<>%
     mutate(FROM = ifelse( ! str_detect(X6, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X6), paste(FROM, X6, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X7, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X7), paste(FROM, X7, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X8, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X8), paste(FROM, X8, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X9, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X9), paste(FROM, X9, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X10, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X10), paste(FROM, X10, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X11, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X11), paste(FROM, X11, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X12, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X12), paste(FROM, X12, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X13, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X13), paste(FROM, X13, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X14, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X14), paste(FROM, X14, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X15, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X15), paste(FROM, X15, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X16, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X16), paste(FROM, X16, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X17, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X17), paste(FROM, X17, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X18, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X18), paste(FROM, X18, sep = " "), FROM)) %>%
     mutate(FROM = ifelse( ! str_detect(X19, "[[:lower:]]|U\\.S\\.|D-|D\\/|R-|R\\/|1\\)|ADMINISTRATION \\(NHTSA\\)|ADMINISTRATION|REALTY|I\\/|RZACCAGNINO|I\\/VT|\\(P\\)") & ! is.na(X19), paste(FROM, X19, sep = " "), FROM)) 
   

   #Fix name format
   data %<>%
     mutate(FROM = str_replace(FROM, "Floyd, Dr. Isaac L.", "Dr Isaac Floyd"))
   
  #Remove non members from dataset
  data$FROM <- gsub("Writer\\(s\\):( |$)|Writer/Editor: |Writers): |\\.$", "", data$FROM)
  
  
  #Paste split subject into SUBJECT
  data %<>%
    mutate(SUBJECT = ifelse( ! str_detect(X20, "[[:lower:]]|U\\.S\\.") & ! is.na(X20), paste(SUBJECT, X20, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X21, "[[:lower:]]|U\\.S\\.") & ! is.na(X21), paste(SUBJECT, X21, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X22, "[[:lower:]]|U\\.S\\.") & ! is.na(X22), paste(SUBJECT, X22, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X23, "[[:lower:]]|U\\.S\\.") & ! is.na(X23), paste(SUBJECT, X23, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X24, "[[:lower:]]|U\\.S\\.") & ! is.na(X24), paste(SUBJECT, X24, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X25, "[[:lower:]]|U\\.S\\.") & ! is.na(X25), paste(SUBJECT, X25, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X26, "[[:lower:]]|U\\.S\\.") & ! is.na(X26), paste(SUBJECT, X26, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X27, "[[:lower:]]|U\\.S\\.") & ! is.na(X27), paste(SUBJECT, X27, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X28, "[[:lower:]]|U\\.S\\.") & ! is.na(X28), paste(SUBJECT, X28, sep = " "), SUBJECT)) %>%
    mutate(SUBJECT = ifelse( ! str_detect(X29, "[[:lower:]]|U\\.S\\.") & ! is.na(X29), paste(SUBJECT, X29, sep = " "), SUBJECT))
  
  
  #Format Typos
  data %<>%
    mutate(FROM = str_remove_all(FROM, " Jr\\.| JR\\.")) %>%
    mutate(FROM = str_replace(FROM, ",, ", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, "U\\.S\\.|U\\.S", "US")) %>%
    mutate(FROM = str_replace(FROM, ", n,| n, ", ", "))
  
  #Name Format Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "YOUNG, C\\. W\\. BILL", "Charles YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "Schoch, P\\.E\\., Barry J", "Schoch, Barry")) %>%
    mutate(FROM = str_replace(FROM,"Prasad, P\\.E\\., Ananth", "Prasad, Ananth")) %>%
    mutate(FROM = str_replace(FROM, "Cassidy, M\\.D\\., Bill", "Bill CASSIDY")) %>%
    mutate(FROM = str_replace(FROM, "Cleaver, n, Emanuel|CLEAVER n, EMANUEL|Cleaver, II, Emanuel|Cleaver, n , Emanuel", "Cleaver, Emanuel")) %>%
    mutate(FROM = str_replace(FROM, "BUTTERFIELD, G\\.K|BUTTERFIELD, G\\.K|BUTTERFIELD, G\\. K|Butterfield, G\\. K\\.|BUTTERFIELD, G\\.K\\.|Butterfield, G\\.K\\.|BUTTERFIELD, G|Butterfield, GK|G\\.K\\. Butterfield|G\\. K\\. Butterfield", "George BUTTERFIELD")) %>%
    mutate(FROM = str_replace(FROM, "James, Sr\\., Charles E", "James, Charles E")) %>%
    mutate(FROM = str_replace(FROM, "Bera, M\\.D\\., Ami", "Bera, Ami")) %>%
    mutate(FROM = str_replace(FROM, "Costello n, Jerry F", "Costello, Jerry F")) %>%
    mutate(FROM = str_replace(FROM, "Hardy, MD, Joseph", "Hardy, Joseph")) %>%
    mutate(FROM = str_replace(FROM, "Kitzhaber, M\\.D\\., John A", "Kitzhaber, John A")) %>%
    mutate(FROM = str_replace(FROM, "SENSENBRENNER, F\\. JAMES\\.|Sensenbrenner, F\\. James|SENSENBRENNER, F\\. JAMES", "SENSENBRENNER, Frank.")) %>%
    mutate(FROM = str_replace(FROM, "V01N0VICH, GEORGE", "VOINOVICH, George")) %>%
    mutate(FROM = str_replace(FROM, "RUPPERSBERGER, C\\.A\\. DUTCH\\.|RUPPERSBERGER, C\\.A\\. DUTCH|Ruppersberger, C\\.A\\. Dutch", "Dutch RUPPERSBERGER")) %>%
    mutate(FROM = str_replace(FROM, "CONAWAY, K\\. MICHAEL\\.", "Kenneth CONAWAY")) %>%
    mutate(FROM = str_replace(FROM, "Pocan,Mark", "Pocan, Mark")) %>%
    mutate(FROM = str_replace(FROM, "CLAY, WM\\. LACY\\.", "William Lacy CLAY\\.")) %>%
    mutate(FROM = str_replace(FROM, "CLAY, WM\\. LACY|Wm\\. Lacy Clay", "William Lacy CLAY")) %>%
    mutate(FROM = str_replace(FROM, "Saenz, P\\.E\\., Amadeo", "Saenz, Amadeo")) %>%
    mutate(FROM = str_replace(FROM, "SECRETARY LAHOOD", "Ray LaHood"))
  

  
  #Match on state
  data %<>%
   mutate(FROM = ifelse(str_detect(SUBJECT, "SOUTH DAKOTA| OGLALA SIOUX TRIBE|CHEYENNE RIVER SIOUX") & ! is.na(SUBJECT), str_replace(FROM, "JOHNSON, TIM|JOHNSON, TIMOTHY", "Timothy Peter JOHNSON"), FROM)) %>%
   mutate(FROM = ifelse(str_detect(SUBJECT, "ILLINOIS|CURTIS ROAD CORRIDOR STUDY|SANGAMON COUNTY") & ! is.na(SUBJECT), str_replace(FROM, "JOHNSON, TIM|JOHNSON, TIMOTHY", "Timothy V JOHNSON"), FROM))
   
   
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Timothy Peter JOHNSON") & congress %in% c(112), "Senate", chamber))
  
  #Name Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Clybum, James E", "Clyburn, James")) %>%
    mutate(FROM = str_replace(FROM, "MKULSKI, BARBARA", "MIKULSKI, Barbara")) %>%
    mutate(FROM = str_replace(FROM, "THOMPSON, MKE", "THOMPSON, MIKE"))
  
   #chamber Typos
   data %<>%
     mutate(chamber = ifelse(str_detect(FROM, "Schock, Aaron"), "House", chamber)) %>%
     mutate(chamber = ifelse(str_detect(FROM, "Duckworth, Tammy") & congress %in% c(113, 114), "House", chamber)) %>%
     mutate(chamber = ifelse(str_detect(FROM, "Bill CASSIDY") & congress %in% c(113), "House", chamber)) %>%
     mutate(chamber = ifelse(str_detect(FROM, "Daines, Steve") & congress %in% (113), "House", chamber)) 
   
  #Match on chamber
  # data %<>%
   #  mutate(FROM = ifelse(str_detect(FROM, "Johnson, Timothy V") & str_detect(chamber, "House") & str_detect(congress, "111|112"), str_replace(FROM, "Johnson, Timothy V", "Timothy V JOHNSON"), FROM))
  
  
  #Remove non members before string split
  data %<>%
    mutate(FROM = str_remove_all(FROM, "House of Representatives|Significant: Yes"))
  
  #String split for multiple member
  data %<>%
    # remove periods after single letters 
    mutate(FROM = str_replace(FROM, "( )(\\w)\\.", "\\1\\2")) %>%
    # split on remaining periods 
    mutate(FROM = str_split(FROM, "\\.| and |&")) %>%
    unnest(FROM) %>%
    distinct()
  

data <- extractMemberName(data, members, 'FROM')

  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())
  
  # add errors
  data %<>%
    mutate(ERROR = ifelse(grepl("Jenna Maslyn", data$FROM), "Jenna Maslyn not in Congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Obama, Barack") & str_detect(congress, "113|112|111"), "President", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "O'DONNELL, WILLIAM|Grundmann, Susan Tsui|Duncan, Arne|Bezio, Brian|Onge, Robert J|Miller, Deb"), "Agency Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Lovingood, Robert A|LeBas, Sherri H|Hall, Christy A|Connaughton, Sean T|Connaughton, Sean|Saenz, Amadeo|Smith, James T|Conti, Eugene A|Hannig, Gary"), "State Agency Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "King, Mike|Murphy, Joan Patricia|McLeod, William D|Weber, J F|WILLIAMS, LISA|Williams, Eugene|Williams, Dennis P|Williams, Dana|Williams, Eugene|Young, John F|Gillan, Jacqueline S|Garcia Martino, Andres R|Fraley, Jeff|Chaney, Nancy J|Berry, John|Atkins, Miles|Anderson, Elliot T|THOMAS, DAVID|Renjel, Louis E|Levi, Grant|Hancock, Michael W|HAGANS, ENOCH|HUTCHERSON, GASTON|THEISEN, MARK|MOTZKO, RICHARD|SCHNEIDER, JAMES|BROWN, KYLE|CRAIG, JOHN|DEVELLE, LOLA|DUDZIAK, JOHN|Hersman, Deborah AP|Smith, Samuel H|STARR, NANCY|DESCHERER, CHRISTOPHER|CONWAY, JOSEPH|BILLIOT, CURTIS|WEBSTER, JOHN|VERBEEK, ANN|RATULOWSKI, ED|NEWTON, CHARLIE|BRYAN, JAMES|WASSERMANN, JOSEPH|James, Charles E|CLOUD, CHARLES|PIZITZ, NORMAN|Cronin, Daniel J|Carona, John|SETTLES, ASHLEY|PINCKNEY, DELICIA|Redeker, James P|Schoch, Barry|Prasad, Ananth|Steudle, Kirk T|KAUFMAN, ROY|Kelly, Brian P|Kelly, Brian|Orseno, Don|Cooper, John R|Horsley, John|Wright, Bud|CHEATHAM, JAMES|Dr Isaac Floyd|MEININGER, RANDY|VERBEEK, GERALD|WHEELER, WILLIAM|Teresa Heinz|BELT, SCOTT|BEULAH|STANFIELD, CHUCK|MCCLURE, THOMAS AND LYNN|Nicholson, Homer| BRANIGAN, LYNNE|BURTON, SANDRA FAULKNER|COLLINS, GARY| COMPTON, RANDY| COUCH, DAVID| COX, CLOVIS|	 CUNNING, GWENDELLA|CUNNINGHAM, BETSY| DALTON, DAVID | DALTON, DONNIE | DALTON, GREG| DALTON, JAMES|O'Malley, Martin"), "Non Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Malloy, Dannel P|Lee, Edwin M|Kriseman, Rick|Kitzhaber, John A|Hieftje, John|Walker, Scott|Webber, Jay|Griffo, Joseph A|Gray, Vincent C|Giacopini, Dorene M|Gerard, Don|Garcia-Padilla, Alejandro|Garcetti, Eric|Feiner, Paul J|Evans, Noreen|Demmer, Tom|Dalrymple, Jack|Cooley, Ken|Campana, Gabriel|Brown, Troy E|Blankenbush, Ken|Bing, Dave|Beshear, Steven L|Wilson, E Dotson|Whirley, Gregory A|Wharton, AC|Quinn, Pat|Martinez, Nelda|Lent, Patty|Gottlieb, Mark|Faulconer, Kevin L|Fletcher, Ernie|PLALE, JEFF|SWEET, ROBERT|Beebe, Mike D|Brewer, Janice K|Brooks, Michele|Lingle, Linda|Longietti, Mark|Markosek, Joseph F|McCall, Keith R|Paterson, David A|Reukauf, William E|Riley, Joseph P|Villaraigosa, Antonio R|Rybak, RT|Kulongoski, Theodore R|Pawlenty, Tim|Gregoire, Christine O|Avella, Tony|Hanna, Mike|Cannella, Anthony|Dayton, Mark|ROACH, RANDY|Harl, Scott J|HARL, SCOTT|Kahele, Gilbert|McDonnell, Robert F"), "State Politican", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Abercrombie, Neil") & congress %in% c(112,113), "No longer in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "NORTON, ELEANOR|Norton, Eleanor Holmes|Pierluisi, Pedro R|FORTUNO, LUIS"), "Non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Salazar, Ken") & congress %in% c(112), "No longer in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Chafee, Lincoln D|Inslee, Jay") & congress %in% c(113), "No longer in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Hassan, Margaret Wood") & congress %in% c(113), "Not yet in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Sanford, Mark") & congress %in% c(110), "No longer in congress", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Ray LaHood") & congress %in% c(111), "Secretary of Transportation", ERROR))
  


  
  unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           ! is.na(DATE))
  

  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, everything())

  
  # apply coding rules
  data%<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("UNIVERSITY|COOK COUNTY|SMART CITY|CITY OF DETROIT|JANUARY 13|ST. CHARLES", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("UNIVERSITY|COOK COUNTY|SMART CITY|CITY OF DETROIT|JANUARY 13|ST. CHARLES", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT,|HIS|STATUS UPDATE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("BEHALF OF CONSTITUENT|CONSTITUENT,|HIS|STATUS UPDATE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("POLIC|VOTING|MOTORCYCLIST ADVISORY|BUY AMERICA WAIVERS|LEGACY INFORMATION|INFRASTRUCTURE PACKAGE|REPORT TO CONGRESS|1664|EXPRESSING CONCERN|OPPOSE|ZERO EMISSIONS|URGING THE COMPLETION|DEPARTMENT REVERSE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("POLIC|VOTING|MOTORCYCLIST ADVISORY|BUY AMERICA WAIVERS|LEGACY INFORMATION|INFRASTRUCTURE PACKAGE|REPORT TO CONGRESS|1664|EXPRESSING CONCERN|OPPOSE|ZERO EMISSIONS|URGING THE COMPLETION|DEPARTMENT REVERSE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROJECT LABOR AGREEMENTS", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "1", ALT_TYPE)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("STATE OF WASHINGTON|PORT OF VIRGINIA|INTERSTATE 94 WEST|TEX WASH BRIDGE", SUBJECT, ignore.case = TRUE), "EARMARK", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("RTC", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("RTC", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("RTC", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("CARBON POLLUTION|URGING SECRETARY FOX|PROPOSED RULE", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
    mutate(POLICY_EVENT = ifelse (!grepl("[A-Z]", POLICY_EVENT) & grepl("REPORT TO CONGRESS", SUBJECT, ignore.case = TRUE), "REPORT", POLICY_EVENT)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "3", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
    mutate(EVENT_NAME = ifelse (!grepl("[A-Z]", EVENT_NAME) & grepl("OREGON DEPARTMENT|ARIZONA DEPARTMENT|CALIFORNIA DEPARTMENT", SUBJECT, ignore.case = TRUE), "EARMARK (I THINK?)", EVENT_NAME)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SPECTRUM", SUBJECT, ignore.case = TRUE), "4", ALT_TYPE)) 
  
  
  
  return(data)  
  
}
