# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


 #file.name <- "DOL_OSHA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  #data$DATE %<>% as.Date("%m/%d/%Y")
  
  # Format date, year, Congress, member name etc.
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate( c("%m-%d-%y","%m/%d/%y"))
  
  NOdate <- data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Create chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Cong\\)") & ! str_detect(FROM, "\\(Sen\\)"), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Sen\\)") & ! str_detect(FROM, "\\(Cong\\)"), "Senate", chamber))
  
  #Remove Sen and Cong
  data %<>%
    mutate(FROM = str_remove_all(FROM, "\\(Cong\\)|\\(Sen\\)"))
  
  # paste all relevent info into subject col
  data %<>% 
    mutate(SUBJECT = paste(Constituent, Organization, SUBJECT)) 
  
  #Trim White Space
  data %<>%
    mutate(FROM = str_trim(FROM))
  
  #String split
  data %<>%
    mutate(FROM = str_split(FROM, ";|&")) %>%
    unnest(FROM)
  
  #Format typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Young, C.W. \\(Bill\\)", "YOUNG, Charles")) %>%
    mutate(FROM = str_replace(FROM, "Butterfield, G. K.", "BUTTERFIELD, George")) %>%
    mutate(FROM = str_replace(FROM, "Butterfield, G.K.", "BUTTERFIELD, George")) %>%
    mutate(FROM = str_replace(FROM, "Brown-Wa te, Ginny", "BROWN-WAITE, Virginia")) %>%
    mutate(FROM = str_replace(FROM, "Conaway, K. Michael", "CONAWAY, Kenneth")) %>%
    mutate(FROM = str_replace(FROM, "Marco, Rubio", "Rubio, Marco")) %>%
    mutate(FROM = str_replace(FROM , "Shaw, E. Clay Jr.", "SHAW, Eugene")) %>%
    mutate(FROM = str_replace(FROM, "Ftzpatrick, Michael G.", "FITZPATRICK, Michael")) %>%
    mutate(FROM = str_replace(FROM, "Visdosky, Peter J.", "VISCLOSKY, Peter")) %>%
    mutate(FROM = str_replace(FROM, "Specter, Arien", "SPECTER, Arlen")) %>%
    mutate(FROM = str_replace(FROM, "Forbes, J. Randy", "FORBES, James")) %>%
    mutate(FROM = str_replace(FROM, "Lautenberg. Frank R", "LAUTENBERG, Frank")) %>%
    mutate(FROM = str_replace(FROM, "Snow, Olympia J.", "SNOWE, Olympia")) %>%
    mutate(FROM = str_replace(FROM, "Barrett, J. Gresham", "BARRETT, James")) %>%
    mutate(FROM = str_replace(FROM, "Cassey, Robert P., Jr.", "CASEY, Robert")) %>%
    mutate(FROM = str_replace(FROM, "Hagan,Kay\n R.|Hagan.Kay\n R.", "HAGAN, Kay")) %>%
    mutate(FROM = str_replace(FROM, "Barton,Joe|Barton. Joe|Barton.Joe", "BARTON, Joseph")) %>%
    mutate(FROM = str_replace(FROM, "Royball-Allard, L.", "ROYBAL-ALLARD, Lucille")) %>%
    mutate(FROM = str_replace(FROM, "Caster, Kathy", "CASTOR, Kathy")) %>%
    mutate(FROM = str_replace(FROM, "Bra ley, Bruce", "BRALEY, Bruce")) %>%
    mutate(FROM = str_replace(FROM, "Westmorelan d, Lynn", "WESTMORELAND, Lynn"))
  
  data %>%
    filter(ID == 1245) %>%
    select(FROM)
  
  # extract member names
  #data %<>%
   # getFirstLast.Comma("FROM")
  
  data <- extractMemberName(data, members, 'FROM')
  
  #Failing observations
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR)) 
  
  data %<>% arrange(DATE)
  
  
  
  return(data)
  
}
