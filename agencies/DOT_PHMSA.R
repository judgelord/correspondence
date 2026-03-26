# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "DOT_PHMSA" # for testing


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
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%B %d %Y")
  
  #Checking for missing dates
  NAdate<-data %>%
    filter(is.na(DATE))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
 
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Senate|Senator"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Representative|House"), "House", chamber))
  
  NoChamber <- data %>%
    filter(is.na(chamber))
  
  #Multiple members  string split
  data %<>%
    mutate(FROM = str_replace(FROM,  "Hill, Baron P. D/IN Member, U.S. House of Representative Polis, Jared. D/CO U.S. House of Representative Latta, Robert. R/OH U.S. House of Representative Shuster, Bill. R/PA U.S. House of Representative Carter, John. R/TX U.S. House of Representative Blackburn, Marsha. R/TN U.S. House of Representative Radanovich, George. U.S. House of Representative Thomberry, Mac. R/TX U.S. House of Representative Diaz-Balart, Mario. R-FL U.S. House of Representatives", 
                              "Hill, Baron P. D/IN Member, U.S. House of Representative; 
                              Polis, Jared. D/CO U.S. House of Representative;
                              Latta, Robert. R/OH U.S. House of Representative; 
                              Shuster, Bill. R/PA U.S. House of Representative; 
                              Carter, John. R/TX U.S. House of Representative; 
                              Blackburn, Marsha. R/TN U.S. House of Representative; 
                              Radanovich, George. U.S. House of Representative; 
                              Thomberry, Mac. R/TX U.S. House of Representative; 
                              Diaz-Balart, Mario. R-FL U.S. House of Representatives" ))

  
  data %<>%
    mutate(FROM = str_split(FROM, "Senate |Representatives |;")) %>%
    unnest(FROM)
  

  #Format for extract member names
  data %<>%
    mutate(FROM = str_replace(FROM, ". ", ", "))
  
 
  #Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Steve, Ted.", "Stevens, Ted")) %>%
    mutate(FROM = str_replace(FROM, "Langevin, Janies", "Langevin, James")) %>%
    mutate(FROM = str_replace(FROM, "Brown, SheiTod.", "BROWN, Sherrod"))
  
  #Format Typos
  data %<>%
    mutate(FROM = str_replace(FROM, "Yarmuth, John. D", "Yarmuth, John D.")) %>%
    mutate(FROM = str_replace(FROM, "Rahal, II, Nick J.", "Nick J RAHALL")) %>%
    mutate(FROM = str_replace(FROM, "Youn, , Don.", "Don E YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "John, Shimkus", "John Shimkus")) %>%
    mutate(FROM = str_replace(FROM, "Rya, , Paul.", "Ryan, Paul")) %>%
    mutate(FROM = str_replace(FROM, "Collin, , Susan M.", "Collins, Susan")) %>%
    mutate(FROM = str_replace(FROM, "Rockefeller, VI, John", "Rockefeller, John")) %>%
    mutate(FROM = str_replace(FROM, "Casey, Jr. , Robert P.", "Casey, Robert")) %>%
    mutate(FROM = str_replace(FROM, "Cantwel, , Maria.", "CANTWELL, Maria"))
  
  #chamber typos
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "Stevens, Ted R-AK U.S. House of Representatives"), str_replace(chamber, "House", "Senate"), chamber)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "Markey, Edward J.") & str_detect(congress, "111|112"), "House", chamber))
  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  
  #Not members
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Wheeler, Jim.|Fiore, Michele.|Klinger, Patricia|Estes, Mark|Willke, Theodore.|Pappas, Sherri.|Bettinelli, Andrew. |Quarterman, Cynthia. |Barill, Anthony|Szabo, Joseph C. |Wiese, Jeffrey D.|Keller, John.|Norton, James.|Poyer, Scott"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Rosenker, Mark V."), "Acting Federal Agencies Chairman", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Talbot, Kirk.|Guillory, Elbert L. |Clark-Reed, Gwyndolen.|Shumake, Lindell F.|Sypolt, Dave.|Merrill, John."), "State politician", ERROR))
  

  
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Price, Thomas. ") & icpsr == "20505", "Wrong Tom, Duplicate", ERROR))
  
           Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR))
  
  #Unmatched <- d %>%
    #filter(is.na(bioname))
  

  
  
  return(data)
  
}

    