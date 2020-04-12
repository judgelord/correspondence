# file.name <- "DHHS_CMS Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name

  data$DATE %<>% as.Date("%m/%d/%y")
  
  noDate <- data %>%
    filter(is.na(DATE))
  
  fullFROM <- data %>%
    filter(! is.na(From))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
    
  data %<>%
    mutate(FROM = ifelse(is.na(From), paste(`From Last Name`, `From First Name`, sep = ", "), FROM))
  
  data <- data[sample(1:nrow(data), 20000, replace=FALSE),]
  
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
     mutate(FROM = str_replace(FROM, "Griffin, Tim", "Griffin, Tom"))

  #Extract Member names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")  
  
  data %<>%
    select(FROM, first_name, last_name, DATE, string, everything())
  
  
  
  
  data %>%
    filter(LetterID == 42738) %>%
    select(FROM)
  
  #ERRORs
   data %<>%
     mutate(ERROR = ifelse(str_detect(FROM, "Gonzalez-Colon, Jenniffer") & is.na(ERROR), "Commissioner of Puerto Rico", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Colvin, Carolyn W\\.") & is.na(ERROR), "Agency staff", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Hassan, Margaret Wood") & congress %in% 114 & is.na(ERROR), "not yet in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "McCollum, Bill") & congress %in% 111 & is.na(ERROR), "no longer in congress", ERROR)) %>% #might be McCollum, Betty
     mutate(ERROR = ifelse(str_detect(FROM, "Kildee, Dale") & congress %in% 113 & is.na(ERROR), "not in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Feingold, Russell") & congress %in% 112 & is.na(ERROR), "no longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Bachus, Spencer") & congress %in% 114 & is.na(ERROR), "no longer in congress", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Schaefermeyer, Connie|Shepley, William L\\.") & is.na(ERROR), "non member", ERROR)) %>%
     mutate(ERROR = ifelse(str_detect(FROM, "Limehouse III, Harry B\\. \'Chip\'|Lynch, John H\\.|Hansen, Alicia \'Chucky\'|Corbett, Tom|Markell, Jack A\\.|Avella, Tony") & is.na(ERROR), "State Legislator", ERROR))
  
  unfoundNamesSample <- data %>%
    filter(is.na(last_name)) %>%
    filter(! str_detect(FROM, "NA, NA")) %>%
    filter( is.na(ERROR))
  
  return(data)
  
}


