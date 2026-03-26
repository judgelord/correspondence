# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "DOL_OASAM Rochelle" # for testing


clean <- function(file.name) {
  
  data <- gs_title(file.name) %>% gs_read() 
  
  # LetterID = sheet row number
  data$LetterID <- 2:(nrow(data)+1) 
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  #create agency column
  data$agency <- file.name 
  
  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #checking for Nodates
  NOdate <- data %>%
    filter(is.na(DATE))
  NOdate
  
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  ###############    
  data %<>% 
    mutate(FROM = str_replace(FROM, "Cong |Conq ", "Representative ") %>% 
             str_remove_all("\n"))
           
  # Creates duplicate rows for lines with multiple representatives
  data %<>% 
    mutate(FROM = str_split(FROM, "&|;")) %>% 
    unnest(FROM) %>%
    mutate(FROM = str_squish(FROM))
  ################
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  #Membership Errors
  NonMembers <- . %>%
    str_detect("Norton, Eleanor Holmes|Ackerman, Greg T.|Ackerman, Joyce L.|Zawacki, Thomas O.|Wu, Portia|Winglass, Robert J.|
               Williams, Doug|Weprin, David I.|Washington, Pauletta D.|Washington, Willie C.|Alvarez, Robert|Cummings, Claude Jr.|
               Fuentes, Nathan D.|Cresci, Peter J.|Deloach, Lawrence E.|Muirhead, James D.|Drago, Tom|Pizzella, Patrick|
               Chao, Secretary|McCarthy, Devin|McNally, Cheryl L.|Chao, Secretary|Ching, Darwin L.D.|Chao, Elaine L.|
               Aumiller, Aaron B.|Williams, Doug|Stinson, Tamara|Hulse, Trevor M.|Smalls, Eugene C.|Simpson, James|
               North, Lynn Fraley|DeBruin, David W.|Coleman, Wayne A.|Miller, Lorraine C.|Friedel, Laura|
               Gonzalez-Colon, Jenniffer|Haley, Nikki R.|Hunt, Robert|Inos, Eloy S.|Knox, Wayne|McLaren, Ellen C.|CAIN, ROBERT|SORBEL, TARYN|LINSKEY, KEVIN")
  
  StatePoliticians <- . %>%
    str_detect("Gordner, John R.|Avella, Tony|Young, Catharine M.|Uresti, Carlos I.|Schwarzenegger, Arnold|Cunningham, Don|
               Spitzer, Eliot|Lynch, John H.|Rell, M. Jodi|Lingle, Linda|Pawlenty, Tim|Goode, Virgil H. Jr.|Dayton, Mark|
               Brown, Edmund G. Jr.|De Leon, Kevin|Stack, Brian P.|Snyder, Rick")
  
  NonVotingMembers <- . %>%
    str_detect("Pierluisi, Pedro R.|Fortuno, Luis|Bordallo, Madeleine Z.|Bordallo, Madeleine .|Christensen, Donna M.|Sablan, Gregorio Kilili Camacho|CAIN, ROBERT")
  
  
  data %>% 
    mutate(ERROR = ifelse(NonVotingMembers(FROM), "Non-voting member", ERROR))  %>% 
    mutate(ERROR = ifelse(StatePoliticians(FROM), "State Politician", ERROR)) %>% 
    mutate(ERROR = ifelse(NonMembers(FROM), "Non-Member", ERROR)) %>% .$ERROR %>% unique()
  
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  Unfoundnames %>% select(congress, FROM) %>% distinct()  %>% kable
  
  return(data)
}
