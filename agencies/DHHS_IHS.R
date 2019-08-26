# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DHHS_IHS" # for testing


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
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #checking for Nodates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
data %<>% 
    mutate(FROM = str_split(FROM, "/")) %>% 
    unnest(FROM)
  ################
  
  
  #data <- getFirstLast.Comma(data, 'FROM')
  
  
  #Fixes name typo
  data$FROM %<>%
    str_replace("INHOFEJAMES", "INHOFE, JAMES") %>%
    str_replace("8AUCUS, MAX", "BAUCUS, MAX") %>%
    str_replace("BINGAMAN JEFF", "BINGAMAN, JEFF") %>%
    str_replace("KYL JON", "KYL, JON") %>%
    str_replace("MCCAIN JOHN", "MCCAIN, JOHN")

    
    
    
  
  data <- extractMemberName(data, members, 'FROM')
  
  
  #Membership Errors
  NonMembers <- data$FROM %>%
    str_detect("Norton, Eleanor Holmes|Ackerman, Greg T.|Ackerman, Joyce L.|Zawacki, Thomas O.|Wu, Portia|Winglass, Robert J.|
               Williams, Doug|Weprin, David I.|Washington, Pauletta D.|Washington, Willie C.|Alvarez, Robert|Cummings, Claude Jr.|
               Fuentes, Nathan D.|Cresci, Peter J.|Deloach, Lawrence E.|Muirhead, James D.|Drago, Tom|Pizzella, Patrick|
               Chao, Secretary|McCarthy, Devin|McNally, Cheryl L.|Chao, Secretary|Ching, Darwin L.D.|Chao, Elaine L.|
               Aumiller, Aaron B.|Williams, Doug|Stinson, Tamara|Hulse, Trevor M.|Smalls, Eugene C.|Simpson, James|
               North, Lynn Fraley|DeBruin, David W.|Coleman, Wayne A.|Miller, Lorraine C.|Friedel, Laura|
               Gonzalez-Colon, Jenniffer|Haley, Nikki R.|Hunt, Robert|Inos, Eloy S.|Knox, Wayne|McLaren, Ellen C.")

  StatePoliticians <- data$FROM %>%
    str_detect("Gordner, John R.|Avella, Tony|Young, Catharine M.|Uresti, Carlos I.|Schwarzenegger, Arnold|Cunningham, Don|
               Spitzer, Eliot|Lynch, John H.|Rell, M. Jodi|Lingle, Linda|Pawlenty, Tim|Goode, Virgil H. Jr.|Dayton, Mark|
               Brown, Edmund G. Jr.|De Leon, Kevin|Stack, Brian P.|Snyder, Rick")

  NonVotingMember <- data$FROM %>%
    str_detect("Pierluisi, Pedro R.|Fortuno, Luis|Bordallo, Madeleine Z.|Bordallo, Madeleine .|
               Christensen, Donna M.|Sablan, Gregorio Kilili Camacho")
  
  data %<>% 
    mutate(ERROR = ifelse(FROM %in% NonVotingMember, "Non-voting member", ERROR)) %>% 
    mutate(ERROR = ifelse(FROM %in% StatePoliticians, "State Politicians", ERROR)) %>% 
    mutate(ERROR = ifelse(FROM %in% NonVotingMember, "Non-Member", ERROR))
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, SUBJECT, FROM,  everything())
  
  return(data)
}
