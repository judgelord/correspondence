# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

 #file.name <- "DOL_SOL" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  names(data)[names(data) == 'SIMS ID'] <- 'ID'
  
  
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
    mutate(FROM = str_split(FROM, "\\/|&|;| and")) %>%
    unnest(FROM)

#Removes the word "and" from the variable FROM
data %<>%
  mutate(FROM = str_remove(FROM, " and")) 

#Replaces "." with "," where they separate a first from a last name
data %<>%
  mutate(FROM = ifelse(! str_detect(FROM, "\\,"), str_replace(FROM, "\\.", "\\,"), FROM))



#Separates first and last name by comma
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
  str_replace("Young, C.W. Bill", "Young, C.W.")


  ################
  
#sample
#sampledata <- data[sample(1:nrow(data), 8000, replace=FALSE),]

 #data <- sampledata

 #Format Typos
 data %<>%
   mutate(FROM = str_replace(FROM, "Forbes, J. Randy", "FORBES, James")) %>%
   mutate(FROM = str_replace(FROM, "Barrett, J. Gresham", "BARRETT, James")) %>%
   mutate(FROM = ifelse(FROM == "DeLauro", str_replace(FROM, "DeLauro", "DeLauro, Rosa L."), FROM)) %>%
   mutate(FROM = str_replace(FROM, "Sensenbrenner, F. James Jr.", "Sensenbrenner, James")) %>%
   mutate(FROM = ifelse(FROM == "Woolsey", str_replace(FROM, "Woolsey", "WOOLSEY, Lynn"), FROM)) %>%
   mutate(FROM = str_replace(FROM, "Young, C. W.|Young, C. W. Bill", "YOUNG, Charles"))

#data <- getFirstLast.Comma(data, 'FROM')

#changed to extractMemberName because it is capturing more observations

data <- extractMemberName(data, members, 'FROM')

Nochamber <- data %>%
  filter(is.na(chamber))



#Membership Errors
data %<>%
  mutate(ERROR = ifelse(str_detect(FROM, "Fortuno, Luis"), "Puerto Rico Legislator", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Norton, Eleanor Holmes"), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Ackerman, Greg T."), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Ackerman, Joyce L."), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Avella, Tony"), "New York State Senate Member", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Zawacki, Thomas O."), "Not a member of Congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Young, Catharine M."), "New York State Senator", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Wu, Portia"), "Not a member of Congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Winglass, Robert J."), "Not a member of Congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Williams, Doug"), "Not a member of Congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Weprin, David I."), "Not a member of Congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Washington, Pauletta D."), "Not a member of Congress", ERROR))

#Puts all data without a comma into last name variable and 
#Format last name and put in last_name  

data %<>%
  mutate(last_name = ifelse(! str_detect(FROM, "\\,") & is.na(last_name), formatLastName(data, 'FROM'), last_name))

#Multiple unnamed members
data %<>%
  mutate(NOTES = ifelse(str_detect(FROM, "Other|Others"), "Multiple Unnamed Members, FOIA", NOTES))

#Failing observations
Unfoundnames <- data %>%
  filter(is.na(last_name),
         str_detect(pattern, "404error"),
         is.na(ERROR),
         is.na(NOTES))  

data %>%
  filter(ID == 655431) %>%
  select(FROM)
#sample <- data %>%
#filter(is.na(last_name))  
#View(sample)

#Check after run through merge
#Unfoundnames <- d %>%
#filter(is.na(bioname))

##code for testing

  #arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
  
  
  return(data)
}
