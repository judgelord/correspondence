# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


# file.name <- "DOL_OWCP Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read()   
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #format DATE to multiple formats
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  #finding NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #create agency column
  data$agency <- file.name
  
  
  ###############
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, "/")) %>% 
    unnest(FROM)
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Foxx. Virginia", "Foxx, Virginia")) %>% 
    mutate(FROM = str_replace(FROM, "Schumer", "Schumer, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Young, C.W. Bill", "Young, Bill")) %>%
    mutate(FROM = str_replace(FROM, "Young, C.W.", "Young, Bill")) %>%
    mutate(FROM = str_replace(FROM, "Byrd. Robert C.", "Byrd, Robert C.")) %>% 
    mutate(FROM = str_replace(FROM, "Barrett, J. Gresham", "Barrett, James")) %>%
    mutate(FROM = str_replace(FROM, "Beutler, Jamie Herrera", "HERRERA BEUTLER, Jaime")) %>%
    mutate(FROM = str_replace(FROM, "Filemon, Vela", "VELA, Filemon")) %>%
    mutate(FROM = str_replace(FROM, "Pocan", "Pocan, Mark")) %>%
    mutate(FROM = str_replace(FROM, "Hochul, Kthleen C.", "Hochul, Kathleen C.")) %>%
    mutate(FROM = str_replace(FROM, "Conaway, K. Michael", "Conaway, Michael")) %>%
    mutate(FROM = str_replace(FROM, "Lugar Richard", "Lugar, Richard")) %>%
    mutate(FROM = str_replace(FROM, "Sensenbrenner, F. James Jr.", "SENSENBRENNER, Frank James, Jr.")) %>%
    mutate(FROM = str_replace(FROM, "Courtney", "Courtney, Joe")) %>%
    mutate(FROM = str_replace(FROM, "Clinton", "CLINTON, Hillary Rodham")) %>% 
    mutate(FROM = str_replace(FROM, "Polis", "POLIS, Jared")) %>%
    mutate(FROM = str_replace(FROM, "PERLMUTTER", "PERLMUTTER, Ed")) %>%
    mutate(FROM = str_replace(FROM, "Representative Ben Ray Lújan", "Lújan, Ben")) %>%
    mutate(FROM = str_replace(FROM, "Reed , Thomas W. II", "REED, Thomas W. II")) %>%
    mutate(FROM = str_replace(FROM, "Lee, Shelia Jackson", "JACKSON LEE, Sheila")) %>%
    #mutate(FROM = str_replace(FROM, "Alexander", "ALEXANDER, Lamar")) %>%
    #FIXME Alexander, Lamar was failing to match and Alexander, Rodney was failing to match (likely because of the above line, but these should be checked)
    #FIXME also, there are at least two cases where members appear after &:
    # & Corker, 
    # & Udall, T
    mutate(FROM = str_replace(FROM, "McGovern", "McGOVERN, James P.")) %>%
    mutate(FROM = str_replace(FROM, "Hill, J. French", "HILL, French")) 
  

  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("\\(Sen\\)|\\(Sen.\\)|Senate|Senator", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("\\(Cong\\)|\\(Cong.\\)|Representative", FROM), "House", chamber)) 
  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
 
  

  #ERRORS
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Mathias, James N.|McElwaine, James P.|Patterson, James H.|Haynes, Gregory L.|Martin, Todd|Jerison, Deb|Robinson, Johnnie E. III|Parsons, Stephanie|McLancon, Charlie|Rafferty, Dennis Michael|North, Lynn Fraley|Muirhead, James D.|Hulegaren, Marty|Ricks, Rosena A.|Hand, Donna|Fortuno, Luis G.|Evangelisti, John S.|Dillon, Stephaine|Deloach, Lawrence E.|Crawford, Kathryn G.|Connor, Jeffrey|SC First Congressional District Office|Shahan, Theresa|Smalls, Eugene C.|Coleman, Wayne A.|Churovich, Danial|Christensen, Donna M.|Washington, Pauletta D.|Bordallo, Madeleine Z|Norton, Eleanor Holmes|Avella, Tony|Bordallo, Madeleine|Bordallo, Madeleine .|Wilson, Ruth|Knox, Wayne|Storms, Ronda|Shapiro, Alan"), "Not Member", ERROR))
  
  #FOIA NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Wittman & 17 Others"), "Multiple members Unknown", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Maloney & 15 Others"), "Multiple members Unknown", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Scott, R. & 7 Other"), "Multiple members Unknown", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Voinovich & 12 Others"), "Multiple members Unknown", NOTES)) %>% 
    mutate(NOTES = ifelse(ID=="482623", "Multiple members Unknown", NOTES)) %>%
    mutate(NOTES = ifelse(ID=="520604", "Multiple members Unknown", NOTES))
    
    
 

 
  #REGULAR NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Rodriguez, Ciro"), "No longer in Congress", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Owens, William L. "), "No longer in Congress", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Sen\\)"), "Not sure if we are missing members or need to investigate why a name isn't attached to this", NOTES))
    
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, chamber,  FROM, SUBJECT, everything())
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR),
           is.na(NOTES))
  
  
  return(data)
}
