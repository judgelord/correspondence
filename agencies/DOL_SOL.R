# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "DOL_SOL" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  names(data)[names(data) == 'SIMS ID'] <- 'LetterID'
  
  
   #create agency column
  data$agency <- file.name 

  # Format date, year, Congress
  data$DATE %<>% as.Date("%m/%d/%y")
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #checking for Nodates
  
  NOdate <- data %>%
    filter(is.na(DATE))

 
  ############### 

#Creates new variable chamber in full dataset
  data %<>%
     mutate(chamber = ifelse(str_detect(FROM, "\\(Sen|Sen\\.|Senator") & ! str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman"),
                             "Senate", NA))
 data %<>%
     mutate(chamber = ifelse(str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman") & ! str_detect(FROM, "\\(Sen|Sen\\.|Senator"),
                             "House", chamber)) 

  #Removes the chamber assigment from the variable FROM
 
 data %<>%
   mutate(FROM = str_remove_all(FROM, "\\(.*\\)")) %>%
   mutate(FROM = str_remove(FROM, "\\(.*")) %>%
   mutate(FROM = str_remove(FROM, "\\).*")) %>%
   mutate(FROM = str_remove_all(FROM, " Sen$|Sen\\.|Senator")) %>%
   mutate(FROM = str_remove(FROM, " Cong$|Member of Congress|Congressman")) %>%
   mutate(FROM = str_remove(FROM, "Chairman"))
   
 
 
    

  #Final Version with full data splits data on "/", "&", and ";" to account for multiple authors
 data %<>%
   mutate(FROM = str_replace(FROM, "Warner, Kaine, Manchin, Bennet,", "Warner; Kaine; Manchin; Bennet,"))
 
data %<>%
    mutate(FROM = str_split(FROM, "\\/|&|;| and")) %>%
    unnest(FROM)

#Removes the word "and" from the variable FROM
data %<>%
  mutate(FROM = str_remove(FROM, " and|;")) 

#Create ID
data %<>%
  mutate(ID = row_number())


#Replaces "." with "," where they separate a first from a last name
data %<>%
  mutate(FROM = ifelse(! str_detect(FROM, "\\,"), str_replace(FROM, "\\.", "\\,"), FROM))

data %<>%
  mutate(FROM = str_trim(FROM)) 

#Switches the order so last name comes first
#added names into nameMethods
data$FROM %<>% 
  str_replace("Elaine, Chao", "Chao, Elaine") %>%
  str_replace("George, Miller", "Miller, George")

#Fixes name typo
data$FROM %<>%
  str_replace("Davis, Arthur", "Davis, Artur") %>%
  str_replace("Gillibrand, Kirstein", "Gillibrand, Kirsten") %>%
  str_replace("Leahy, Ted", "Leahy, Patrick") %>%
  str_replace("Gerlah, Jim", "Gerlach, Jim") %>%
  str_replace("Obama, Brack", "Obama, Barack") %>%
  str_replace("Hooley, Darene", "Hooley, Darlene") %>%
  str_replace("Rubio, Mario", "Rubio, Marco") %>%
  str_replace("Alexander, Larmar", "Alexander, Lamar") %>%
  str_replace("Dingel, John D.", "DINGELL, John") %>%
  str_replace("Cuellar, Hery", "Cuellar, Henry") %>%
  str_replace("Kuchinich, Dennis J.", "KUCINICH, Dennis") %>%
  str_replace("Dent, Chares W.", "DENT, Charles") %>%
  str_replace("Snowe, Olympha J.", "SNOWE, Olympia") %>%
  str_replace("Specter, Alan", "SPECTER, Arlen") %>%
  str_replace("Harris, Cathy", "HARRIS, Katherine") %>%
  str_replace("Brown, Sherod", "Brown, Sherrod") %>%
  str_replace("Visclosky, Petr J.", "VISCLOSKY, Peter") %>%
  str_replace("Caster, Kathy", "CASTOR, Kathy") %>%
  str_replace("Hultgreeen, Randy|Hultgren|Hultgreen, Randy", "HULTGREN, Randy") %>%
  str_replace("Buchanan, Veern", "BUCHANAN, Vernon") %>%
  str_replace("Gillibrand, Kirstern E.", "Gillibrand, Kirsten")


  ################

 #Format Typos
 data$FROM %<>%
   str_replace("Forbes, J. Randy", "FORBES, James") %>%
   str_replace("Barrett, J. Gresham", "BARRETT, James") %>%
   str_replace("Sensenbrenner, F. James Jr.", "Sensenbrenner, James") %>%
   str_replace("Young, C.W. Bill|Young, C. W. Bill|Young, C. W.|Young, C.W.", "YOUNG, Charles") %>%
   str_replace("Butterfield, G.K.|Butterfield, G. K.", "BUTTERFIELD, George") %>%
   str_replace("Sensenbrenner, F. James", "SENSENBRENNER, Frank") %>%
   str_replace("McEachin, A. Donald", "MCEACHIN, Aston") %>%
   str_replace("Wilson,Joe", "Wilson, Joe") %>%
   str_replace("McIntyre,, Mike|McIntyre, Mile", "McINTYRE, Mike") %>%
   str_replace("Conaway, K. Michael", "CONAWAY, Kenneth") %>%
   str_replace("Cassey, Robert P., Jr.", "CASEY, Robert") %>%
   str_replace("Pete V, Domenici", "DOMENICI, Pete") %>%
   str_replace("Lynch, S. F|Lynch,\nS. F", "LYNCH, Stephen") %>%
   str_replace("Grisham, Michelle Lujan", "Michelle LUJAN") %>%
   str_replace("Hill, J. French", "Hill, French") %>%
   str_replace("Filemon, Vela", "VELA, Filemon")

 data %<>%
 mutate(FROM = ifelse(FROM == "DeLauro", str_replace(FROM, "DeLauro", "DeLauro, Rosa L."), FROM)) %>%
 mutate(FROM = ifelse(FROM == "Woolsey", str_replace(FROM, "Woolsey", "WOOLSEY, Lynn"), FROM))
 
 #Chamber typo
 data %<>%
   mutate(chamber = ifelse(str_detect(FROM, "Alexander, Lamar") & str_detect(chamber,"House"), str_replace(chamber,"House", "Senate"), chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Dent, Charles W.") & str_detect(chamber, "Senate"), str_replace(chamber, "Senate", "House"), chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Gillibrand, Kirsten") & ! str_detect(congress, "110|111"), "Senate", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Miller, George"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Scott, Tim") & str_detect(congress, "113"), "Senate", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "McGovern") & str_detect(congress, "110"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Michaud") & str_detect(congress, "110|111"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Udall") & str_detect(congress, "110"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Udall") & str_detect(congress, "111"), "Senate", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "KENNEDY, Edward|Kennedy, Edward") & str_detect(congress, "110"), "Senate", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Kennedy, Patrick") & str_detect(congress, "110"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Blunt, Roy") & str_detect(congress, "110"), "House", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Blunt, Roy") & str_detect(congress, "112|113|114|115|116"), "Senate", chamber)) %>%
   mutate(chamber = ifelse(str_detect(FROM, "Kline, John"), "House", chamber))
 
 #Match on Chamber
 data %<>%
  mutate(FROM = ifelse(FROM == "Kennedy" & str_detect(chamber, "Senate") & str_detect(congress, "110"), str_replace(FROM, "Kennedy", "KENNEDY, Edward"), FROM)) %>%
  mutate(FROM = ifelse(FROM == "Kennedy" & str_detect(chamber, "House") & str_detect(congress, "110"), str_replace(FROM, "Kennedy", "KENNEDY, Patrick"), FROM))
 
 #Match on Chamber_Last
 data %<>%
   mutate(FROM =ifelse( ! str_detect(FROM, " ") & str_detect(chamber, "House"), paste("Representative", FROM, sep = " "), FROM)) %>%
   mutate(FROM = ifelse( ! str_detect(FROM, " ") & str_detect(chamber, "Senate"), paste("Senator", FROM, sep = " "), FROM))
 
 #From Members Staff
 data %<>%
   mutate(FROM = str_replace(FROM, "Pellito, John", "Office of Louise Slaughter, Community Liaison Pellito, John")) %>%
   mutate(NOTES = ifelse(str_detect(FROM, "Office of Louise Slaughter, Community Liaison Pellito, John"), "FROM", NOTES))
 
 #sample
 #sampledata <- data[sample(1:nrow(data), 8000, replace=FALSE),]
 
 #data <- sampledata
 

#extractMemberName

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
  mutate(ERROR = ifelse(str_detect(FROM, "Trumka, Richard L."), "AFL-CIO President", ERROR)) %>%
  mutate(ERROR = ifelse(NonMembers, "Non Member", ERROR)) %>%
  mutate(ERROR = ifelse(StatePoliticians, "State Politician", ERROR)) %>%
  mutate(ERROR = ifelse(NonVotingMember, "Non Voting Member", ERROR))


#Puts all data without a comma into last name variable and 
#Format last name and put in last_name  

data %<>%
  mutate(last_name = ifelse(! str_detect(FROM, " ") & is.na(last_name), formatLastName(data, 'FROM'), last_name))

#Multiple unnamed members
data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Other|Others"), "Multiple Unnamed Members, FOIA", NOTES))

#Failing observations
Unfoundnames <- data %>%
  filter(is.na(last_name),
         str_detect(pattern, "404error"),
         is.na(ERROR),
         is.na(NOTES))  

#nonMembers
nonmem <- data %>%
  filter(! is.na(ERROR))

data %>%
  filter(ID == 712737) %>%
  select(FROM, chamber)

#Filter FOIA
FOIA <- data %>%
  filter(str_detect(FROM, "Representative Udall|Senator Udall"))

data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Representative Udall|Senator Udall") & is.na(last_name), "Multiple Udall's FOIA", NOTES))

#sample <- data %>%
#filter(is.na(last_name))  
#View(sample)

#Check after run through merge
#Unfoundnames2 <- d %>%
#filter(is.na(pattern))

##code for testing

  #arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  
  return(data)
}
