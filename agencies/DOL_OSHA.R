# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


 #file.name <- "DOL_OSHA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create LetterID variable
  data$LetterID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  #data$DATE %<>% as.Date("%m/%d/%Y")
  
  # Format date, year, Congress, member name etc.
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))
  
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #String split
  data %<>%
    mutate(FROM = str_split(FROM, ";|&")) %>%
    unnest(FROM)
  
  #Create chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Cong\\)| \\(Rep\\)") & ! str_detect(FROM, "\\(Sen\\)|\\(Sen \\)"), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Sen\\)|\\(Sen \\)") & ! str_detect(FROM, "\\(Cong\\)"), "Senate", chamber))
  
  #Remove Sen and Cong
  data %<>%
    mutate(FROM = str_remove_all(FROM, "\\(Cong\\)|\\(Sen\\)|\\(Sen \\)| \\(Rep\\)"))
  
  # paste all relevent info into subject col
  data %<>% 
    mutate(SUBJECT = paste(Constituent, Organization, SUBJECT)) 
  
  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))

  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  #Delete extra space
  data %<>%
    mutate(FROM = str_replace(FROM, " , | ,", ", "))
  
  #Format typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Young, C.W. \\(Bill\\)", "YOUNG, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Butterfield, G. K.", "BUTTERFIELD, George")) %>%
    mutate(FROM = str_replace(FROM, "Butterfield, G.K.", "BUTTERFIELD, George")) %>%
    mutate(FROM = str_replace(FROM, "Brown-Wa te, Ginny", "BROWN-WAITE, Virginia")) %>%
    mutate(FROM = str_replace(FROM, "Conaway, K. Michael", "CONAWAY, Kenneth")) %>%
    mutate(FROM = str_replace(FROM, "Marco, Rubio", "Rubio, Marco")) %>%
    mutate(FROM = str_replace(FROM , "Shaw, E. Clay Jr.", "SHAW, Eugene")) %>%
    mutate(FROM = str_replace(FROM, "Ftzpatrick, Michael G.", "FITZPATRICK, Michael")) %>%
    mutate(FROM = str_replace(FROM, "Visdosky, Peter J.", "VISCLOSKY, Peter")) %>%
    mutate(FROM = str_replace(FROM, "Specter, Arien|Specte r, Arlen", "SPECTER, Arlen")) %>%
    mutate(FROM = str_replace(FROM, "Forbes, J. Randy", "FORBES, James")) %>%
    mutate(FROM = str_replace(FROM, "Lautenberg. Frank R", "LAUTENBERG, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Snow, Olympia J.", "SNOWE, Olympia")) %>%
    mutate(FROM = str_replace(FROM, "Barrett, J. Gresham", "BARRETT, James")) %>%
    mutate(FROM = str_replace(FROM, "Cassey, Robert P., Jr.", "CASEY, Robert")) %>%
    mutate(FROM = str_replace(FROM, "Hagan,Kay\n R.|Hagan.Kay\n R.", "HAGAN, Kay")) %>%
    mutate(FROM = str_replace(FROM, "Barton,Joe|Barton. Joe|Barton.Joe", "BARTON, Joseph")) %>%
    mutate(FROM = str_replace(FROM, "Royball-Allard, L.", "ROYBAL-ALLARD, Lucille")) %>%
    mutate(FROM = str_replace(FROM, "Caster, Kathy", "CASTOR, Kathy")) %>%
    mutate(FROM = str_replace(FROM, "Bra ley, Bruce", "BRALEY, Bruce")) %>%
    mutate(FROM = str_replace(FROM, "Westmorelan d, Lynn|WestmoreIan d, Lynn A.", "WESTMORELAND, Lynn")) %>%
    mutate(FROM = str_replace(FROM, "Lamar, Alexander|Lamar,\n Alexander", "Alexander, Lamar")) %>%
    mutate(FROM = str_replace(FROM, "Baird. Brian", "BAIRD, Brian")) %>%
    mutate(FROM = str_replace(FROM, "Blumenthal\n, Richard", "Blumenthal, Richard")) %>%
    mutate(FROM = str_replace(FROM, "Boehne r, John A.", "BOEHNER, John")) %>%
    mutate(FROM = str_replace(FROM, "Bonne r, Jo", "BONNER, Josiah")) %>%
    mutate(FROM = str_replace(FROM, "Bro wn, Sherrod", "Brown, Sherrod")) %>%
    mutate(FROM = str_replace(FROM, "Frelinghuyse n, Rodney P.|Frelinghuyse\n n, Rodney P.", "FRELINGHUYSEN, Rodney")) %>%
    mutate(FROM = str_replace(FROM, "Davis, Linco ln", "DAVIS, Lincoln")) %>%
    mutate(FROM = str_replace(FROM, "Collins\n, Susan M.", "COLLINS, Susan")) %>%
    mutate(FROM = str_replace(FROM, "Lugren,\n Daniel E.", "LUNGREN, Daniel")) %>%
    mutate(FROM = str_replace(FROM, "Davis, Geo ff", "DAVIS, Geoffrey")) %>%
    mutate(FROM = str_replace(FROM, "lnslee, Jay", "INSLEE, Jay")) %>%
    mutate(FROM = str_replace(FROM, "Warne r, Mark R.", "Warner, Mark")) %>%
    mutate(FROM = str_replace(FROM, "Mikulski, Barba ra A.", "Mikulski, Barbara")) %>%
    mutate(FROM = str_replace(FROM, "Sensenbrenn er, F. James", "SENSENBRENNER, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Rehberg, Danny", "REHBERG, Dennis")) %>%
    mutate(FROM = str_replace(FROM, "Jorda n, Jim", "JORDAN, James")) %>%
    mutate(FROM = str_replace(FROM, "Israe l, Steve", "ISRAEL, Steven")) %>%
    mutate(FROM = str_replace(FROM, "Ros - Lehtinen, Ileana|Ros- Lehtinen, Ileana|Ros- Lehtinen,", "ROS-LEHTINEN, Ileana")) %>%
    mutate(FROM = str_replace(FROM, "Stutzma n, Marlin", "STUTZMAN, Marlin")) %>%
    mutate(FROM = str_replace(FROM, "Owens, W illiam L.", "OWENS, William")) %>%
    mutate(FROM = str_replace(FROM, "Burton.Dan", "BURTON, Danny")) %>%
    mutate(FROM = str_replace(FROM, "Lummis, Cyntha M.", "LUMMIS, Cynthia")) %>%
    mutate(FROM = str_replace(FROM, "Schume,r Charles E.", "SCHUMER, Charles")) %>%
    mutate(FROM = str_replace(FROM, "McEachin,\n A. Donald", "MCEACHIN, Aston")) %>%
    mutate(FROM = str_replace(FROM, "Risc h, James E.", "RISCH, James")) %>%
    mutate(FROM = str_replace(FROM, "Cuellar, Hery", "Cuellar, Henry")) %>%
    mutate(FROM = str_replace(FROM, "Ruppersberg er,C.A.\n Dutch|Ruppersberg er, C. A.\n Dutch", "RUPPERSBERGER, C.")) %>%
    mutate(FROM = str_replace(FROM, "lskaon, Johnny", "ISAKSON, Johnny")) %>%
    mutate(FROM = str_replace(FROM, "Des antis, Ron", "DeSANTIS, Ron")) %>%
    mutate(FROM = str_replace(FROM, "Hurt. Robert", "HURT, Robert")) %>%
    mutate(FROM = str_replace(FROM, "Sa rbanes, John P.", "SARBANES, John")) %>%
    mutate(FROM = ifelse(FROM == "Diaz-Balart," & congress == 115, str_replace(FROM, "Diaz-Balart,", "DIAZ-BALART, Mario"), FROM)) %>%
    mutate(FROM = str_replace(FROM, "Grisham, Michelle Lujan", "LUJAN, Michelle"))
  
  #chamber typos
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Miller, George") & congress == 109, str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "McGovern, James") & congress == 109, "House", chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Udall, Tom") & congress == 109, "House", chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Kennedy, Edward") & congress == 109, "Senate", chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Alexander, Lamar") & congress == 112, str_replace(chamber, "House", "Senate"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Brown, Sherrod") & congress == 113, str_replace(chamber, "House", "Senate"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Dicks, Norm") & congress == 111, str_replace(chamber, "Senate", "House"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Davis, Tom \\(Chairman\\)") & congress == 109, "House", chamber))
    
  
  data %>%
    filter(LetterID == 2463) %>%
    select(FROM)
  
  # extract member names
  #data %<>%
   # getFirstLast.Comma("FROM")
  
  data <- extractMemberName(data, members, 'FROM')
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Others|Other"), "Multiple Unnamed Members", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Whelan, Jim|Wisniewski, John "), "State Politician", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Mau-Shimizu, Patricia|Sunning, Jim|Carlton, Maggie"), "Non Member", ERROR))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES)) 
  
  data %<>% arrange(DATE)
  
                                   
  
  return(data)
  
}
