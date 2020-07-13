# file.name <- "DHHS_CMS Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  
# LetterIDs for these data will have ";;;" in them because most are duplicated
  
  # helper function to deal with duplicates
  combine_strings <- . %>% unique() %>% str_c(collapse = ";;;")
  
  if(F){ # Inspect duplicates 
  duplicates <- data %>% 
    group_by(`From Last Name`, `From First Name`, `DATE`, `SUBJECT`) %>% 
    summarise_all(combine_strings) %>% 
    add_count() %>% 
    filter(n>1) %>% 
    distinct()
  
  # duplicated subjects (short ones or NA may not be duplicates)
  duplicates %>% count(SUBJECT, sort = T) 
  
  duplicate_coding <- duplicates %>%  
    filter(str_detect(TYPE, ";;;")|str_detect(ALT_TYPE, ";;;") ) %>%
    select(DATE, agency, bioname, SUBJECT, TYPE, ALT_TYPE) %>% distinct()
  duplicate_coding
  }
  
  # Drop duplicates 
  data %<>% group_by(`From Last Name`, `From First Name`, `DATE`, `SUBJECT`) %>% 
    summarise_all(combine_strings) %>% 
    ungroup() %>% 
    distinct()

  


  
  #create agency column
  data$agency <- file.name

  data$DATE %<>% as.Date("%m/%d/%y")
  data$`Date to CMS` %<>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date to CMS`, DATE))
  
   noDate <- data %>%
     filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # # for testing
  #fullFROM <- data %>%
  #   filter(! is.na(FROM))
  
  nullName <- data %>%
    filter(str_detect(FROM, "NULL"))
  
  
  data %<>%
    mutate(FROM = ifelse(is.na(FROM), paste(`From Last Name`, `From First Name`, sep = ", "), FROM))
  
  #data <- data[sample(1:nrow(data), 20000, replace=FALSE),]
  
  nullName <- data %>%
    filter(str_detect(FROM, "NULL"))
  
  #Typos
   data %<>%
     mutate(FROM = str_replace(FROM, "Young, C\\. W\\. Bill|Young, C\\. W\\. \'Bill\'", "Young, Charles")) %>%
     mutate(FROM = str_replace(FROM, "Clay, Wm\\.", "CLAY, William")) %>%
  #   mutate(FROM = str_replace(FROM, "Robert P\\., Jr\\. Casey", "Robert Casey")) %>%
  #   mutate(FROM = str_replace(FROM, "Joe, III Manchin", "Joe Manchin")) %>%
     mutate(FROM = str_replace(FROM, "Rockerfeller, John", "Rockefeller, John")) %>%
  #   mutate(FROM = str_replace(FROM, "Benjamin E\\. Nelson", "Benjamin Nelson")) %>%
  #   mutate(FROM = str_replace(FROM, "H\\. Griffith", "H\\. Morgan GRIFFITH")) %>%
     mutate(FROM = str_replace(FROM, "Sensenbrenner, Jr\\., F\\. James", "Sensenbrenner, Frank")) %>%
     mutate(FROM = str_replace(FROM, "Butterfield, G\\.K\\.|Butterfield, G\\. K\\.", "Butterfield, George")) %>%
     mutate(FROM = str_replace(FROM, "Conaway, K\\. Michael", "CONAWAY, Kenneth")) %>%
     mutate(FROM = str_replace(FROM, "Himes, J\\. Marc", "Himes, James")) %>%
     mutate(FROM = str_replace(FROM, "Lindsey, Graham", "GRAHAM, Lindsey")) %>%
     mutate(FROM = str_replace(FROM, "Carter, E\\.L\\.", "CARTER, Buddy")) %>%
     mutate(FROM = str_replace(FROM, "Grijalva, Ral|Grijalva, Raúl|Grijalva, Raï¿½l", "GRIJALVA, Raul")) %>%
     mutate(FROM = str_replace(FROM, "Griffin, H\\. Morgan", "GRIFFITH, H\\.")) %>%
     mutate(FROM = str_replace(FROM, "Grassely, Charles E\\.", "GRASSLEY, Charles")) %>%
     mutate(FROM = str_replace(FROM, "Mack, Mary B\\.", "BONO, Mary")) %>%
     mutate(FROM = str_replace(FROM, "Barrett, J\\. Gresham", "BARRETT, James")) %>%
     mutate(FROM = str_replace(FROM, "Hinojosa, Rubï¿½n", "Hinojosa, Ruben")) %>%
     mutate(FROM = str_replace(FROM, "Capito Moore, Shelley", "CAPITO, Shelley Moore")) %>%
     mutate(FROM = str_replace(FROM, "Labrador, Ra?l|Labrador, Raï¿½l", "LABRADOR, Raul")) %>%
     mutate(FROM = str_replace(FROM, "Sï¿½nchez, Linda", "SANCHEZ, Linda")) %>%
     mutate(FROM = str_replace(FROM, "Tenney, Claudi", "Tenney, Claudia")) %>%
     mutate(FROM = str_replace(FROM, "Feingold, Russell", "FEINGOLD, Russell")) %>%
     mutate(FROM = str_replace(FROM, "Griffin, Tom", "Griffin, Tim")) %>%
     mutate(FROM = str_replace(FROM, "Hill, J\\. French", "Hill, French")) %>%
     mutate(FROM = str_replace(FROM, "Farenthold, Black", "Farenthold, Blake")) %>%
     mutate(FROM = str_replace(FROM, "Takai, K\\. Mark", "TAKAI, Mark")) %>%
     mutate(FROM = str_replace(FROM, "Lujï¿½n, Ben", "LUJAN, Ben")) %>%
     mutate(FROM = str_replace(FROM, "Labrador, Ra?l|Labrador, Ra?l|Labrador, Raúl", "LABRADOR, Raul")) %>%
     mutate(FROM = str_replace(FROM, "MICA  JOHN L, NULL", "MICA, JOHN")) %>%
     mutate(FROM = str_replace(FROM, "CORKER  BOB, NULL", "CORKER, BOB")) %>%
     mutate(FROM = str_replace(FROM, "Andrews, NULL", "ANDREWS, Robert")) %>%
     mutate(FROM = str_replace(FROM, "Murkowski, NULL", "MURKOWSKI, Lisa"))
   
   # check N
   dim(data)

  #Extract Member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")  
  
  # check N
  dim(data)
  
  subjectData <- data %>%
    filter(str_detect(NOTES, "member name in subject")) %>%
    extractMemberName(members = members, col_name = "SUBJECT") 
  
  data %<>%
    full_join(subjectData)
  
  #check N
  dim(data)
  
  
  data %>%
    filter(ID == 7) %>%
    select(FROM)
  
  #ERRORs
   data %<>%
     mutate(ERROR = ifelse(str_detect(FROM, "Gonzalez-Colon, Jenniffer|Pierluisi, Pedro") & is.na(ERROR), "Puerto Rico Legislators", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Colvin, Carolyn W\\.|Counihan, Kevin") & is.na(ERROR), "Agency staff", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Hassan, Margaret Wood") & congress %in% 114 & is.na(ERROR), "not yet in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "McCollum, Bill") & congress %in% 111 & is.na(ERROR), "no longer in congress", ERROR)) %>% #might be McCollum, Betty
     mutate(ERROR = ifelse(str_detect(FROM, "Kildee, Dale|Bingaman, Jeff") & congress %in% 113 & is.na(ERROR), "not in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Feingold, Russell") & congress %in% 112 & is.na(ERROR), "no longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Bachus, Spencer|Moran, James") & congress %in% 114 & is.na(ERROR), "no longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Schaefermeyer, Connie|Shepley, William L\\.|Griffin, Tom|Loubert, Michelle") & is.na(ERROR), "non member", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Limehouse III, Harry B\\. \'Chip\'|Lynch, John H\\.|Hansen, Alicia \'Chucky\'|Corbett, Tom|Markell, Jack A\\.|Avella, Tony|Otter, C\\. L\\. \'Butch\'|Ellis, Rodney|Gill, Nia H\\.|Swanson, Lori") & is.na(ERROR), "state politician", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Griffin, Tim") & congress %in% c(115, 114) & is.na(ERROR), "no longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "riffin, Timothy") & congress %in% c(111) & is.na(ERROR), "no longer in congress", ERROR))
  
  unfoundNamesSample2 <- data %>%
    filter(is.na(last_name)) %>%
    filter(! str_detect(FROM, "NA, NA")) %>%
    filter( is.na(ERROR))
  
  return(data)
  
}


