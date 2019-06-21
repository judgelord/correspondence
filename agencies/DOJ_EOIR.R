# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOJ_EOIR" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data

# create agency column
  data$agency <- file.name
  
  #Format date
  data$DATE %<>% as.Date("%m/%d/%y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Con "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Sen "), "Senate", chamber))
  
  #Splits Rows with multiple authors
  data %<>%
    mutate(FROM = str_split(FROM, " and |&")) %>%
    unnest(FROM)

  data %<>%
    mutate(FROM = str_replace(FROM, "Con Jim Morgan", "Con Jim Moran"))
 
  #Remove middle initials for now
  
  
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  
  #Unmatched
  unmatched <- data %>%
    filter(is.na(last_name)) %>%
    select(DATE, FROM, first_name, last_name, SUBJECT, everything())
  
  #Subject catching non authors
  #Filters for names still unmatched
  #Unfoundnames <- data %>%
   # filter(is.na(last_name)) %>%
    #extractMemberName(members = members, col_name = "SUBJECT") %>% 
  
  
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Don Tripp, State Representative of New Mexico|Daniel Dromm, New York City Council Member|Eva Galambos, Mayor of Sandy Springs, Georgia|John M. Kefalas, State Representative of Colorado|Elaine Nekritz, Illinois State Representative - 57th District|
                                     John J. Gleason, State Senator of Michigan, 27th District|Rashida H. Tlaib, State Representative of Michigan|
                                     Sen Leticia Van de Putte, R.PH., State of Texas, District 26|Amanda Aguirre, Senator, District 24, Arizona State Senate|Willie Simmons, State Senator of Mississpi|
                                     Daphne Campell, RN, State Representative of Fl, District 108|Sen Noreen Evans, California State Senate, Second Senate District"), "State Legislator", ERROR))
  
  
  
  return(data)
  
}