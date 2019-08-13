# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 # file.name <- "DOT_FAA Sam" # for testing
 

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  #create ID variable
  data$ID <- c(1:nrow(data))
  
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
  
  ################
  
  data$ID <- c(1:nrow(data))
  
  #chamber typos
  data %<>%
    mutate(FROM = ifelse(str_detect(FROM, "Flake, Jeff\\.|Flake, Jeff") & congress %in% c("114|115"), str_replace(FROM, "Flake, Jeff\\. \\\n \\\n R\\/AZ \\\n \\\n U\\.S\\. House of Representatives|Flake, Jeff\\.  Congressman  U\\.S\\. House of Representatives|Flake, Jeff\\. Congressman|Flake, Jeff\\.   U\\.S\\. House of Representatives|Flake, Jeff\\.  Congressman  U\\.S\\. House of Representatives|Flake, Jeff\\. R\\/AZ U\\.S\\. House of Representatives|Flake, Jeff Congressman U\\.S House of Representatives|Flake, Jeff\\. Congressman U\\.S\\. House of Representatives|Flake, Jeff\\. U\\.S\\. House of Representatives", "Flake, Jeff United States Senate"), FROM))
  
  #Create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (str_detect(FROM, "Senator|Senate"), "Senate", NA)) %>% 
    mutate(chamber = ifelse(str_detect(FROM, "Representative"), "House", chamber)) %>% 
    mutate(chamber = ifelse(str_detect(assigned, "Representative"), "House", chamber)) %>% 
    mutate(chamber = ifelse(str_detect(assigned, "Senate"), "Senate", chamber)) 
  
  
  #Format Typo
  data %<>%
    mutate(FROM = str_replace_all(FROM, " ,", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, " , ", ", ")) %>%
    mutate(FROM = str_remove_all(FROM, " Jr\\.| JR\\.| Jr\\.,")) %>%
    mutate(FROM = str_replace_all(FROM, ",, ", ", ")) %>%
    mutate(FROM = str_replace_all(FROM, "\\. ", " "))
  
  #Name Format Typo
  data %<>%
    mutate(FROM = str_replace(FROM, "Sensenbrenner, F James|Sensenbrenner,, F James|Sensenbrenner,  F James", "Frank SENSENBRENNER")) %>%
    mutate(FROM = str_replace(FROM, "FrR\\/AZ anks, Trent\\.|FrR\\/AZ  anks, Trent\\.", "Trent FRANKS \\/AZ")) %>%
    mutate(FROM = str_replace(FROM, "ClD\\/MO ay, Wm Lacy\\.|Clay, Wm Lacy  D\\/MO|ClD\\/MO  ay, Wm Lacy\\.", "William CLAY \\/MO")) %>%
    mutate(FROM = str_replace(FROM, "WD\\/VA arner, Mark\\.|WD\\/VA  arner, Mark\\.", "Mark WARNER \\/VA")) %>%
    mutate(FROM = str_replace(FROM, "Young, C\\.W Bill|Young, C W Bill|Young, CW Bill", "Charles YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "McKinley, P\\.E\\., David B", "David McKINLEY")) %>%
    mutate(FROM = str_replace(FROM, "Gosar, D\\.D\\.S\\., Paul A", "Paul GOSAR"))
    
    
  # data <- getFirstLast.Comma(data, 'FROM')
  
  ##extractmemberName takes longer and is worse than getFirstLast
  ## IS THIS TRUE ????
  
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
    mutate(ERROR = ifelse(str_detect(FROM, "Pierluisi, Pedro R|Bordallo, Madeleine Z|Holmes Norton, Eleanor|Norton, Eleanor Holmes|Holmes Norton,  Eleanor"), "Non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Local Government"), "State Politician", ERROR))

  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data %>%
    filter(ID == 11915) %>%
    select(FROM)
    

  return(data)
}



