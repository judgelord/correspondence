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
  
#Filter FROM to find only the data that includes "\\/|&" and has multiple authors
#sampledata1<- data %>% 
#  filter(str_detect(FROM, "\\/|&|;"))

#Creates new variable chamber 
 # sampledata1 %<>%
  #  mutate(chamber = ifelse(str_detect(FROM, "\\(Sen") & ! str_detect(FROM, "\\(Cong"),
   #                         "Senate", NA))
  #sampledata1 %<>%
   # mutate(chamber = ifelse(str_detect(FROM, "\\(Cong") & ! str_detect(FROM, "\\(Sen"),
    #                        "House", chamber)) 

   #Filter FROM for / , & looking for observations with multiple authors 
   
   #Creates duplicate rows for line with multiple representatives to check code and to compare to original col
   #sampledata1 %<>% 
    # mutate(FROM = str_split(FROM, "\\/|&|;")) %>%
     #unnest(FROM) 
   

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
  mutate(FROM = str_trim(FROM)) %>%
  mutate(FROM = ifelse(! str_detect(FROM, "\\,"), str_replace(FROM, " ", "\\, "), FROM))

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
  str_replace("Hooley, Darene", "Hooley, Darlene")



  ################
  
#data <- getFirstLast.Comma(data, 'FROM')

#changed to extractMemberName because it is capturing more observations

data <- extractMemberName(data, members, 'FROM')



#Membership Errors
data %<>%
  mutate(ERROR = ifelse(str_detect(FROM, "Fortuno, Luis"), "Puerto Rico Legislator", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Norton, Eleanor Holmes"), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Ackerman, Greg T."), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Ackerman, Joyce L."), "Not a member of congress", ERROR)) %>%
  mutate(ERROR = ifelse(str_detect(FROM, "Avella, Tony"), "New York State Senate Member", ERROR))
  


#Puts all data without a comma into last name variable and 
#Format last name and put in last_name  

data %<>%
  mutate(last_name = ifelse(! str_detect(FROM, "\\,") & is.na(last_name), formatLastName(data, 'FROM'), last_name))

#data %<>%
 # mutate(last_name = ifelse(! str_detect(FROM, "\\,") & is.na(last_name), FROM, last_name))

#Failing observations
Unfoundnames <- data %>%
  filter(is.na(last_name),
         str_detect(pattern, "404error"),
         is.na(ERROR))  

#sample <- data %>%
#filter(is.na(last_name))  
#View(sample)

#Check after run through merge
#Unfoundnames <- d %>%
#filter(is.na(bioname))

##code for testing

  #arrange columns for hand coding
  data %<>% select(ID, DATE,  FROM, everything())
}
