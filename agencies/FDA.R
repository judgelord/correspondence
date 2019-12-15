# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FDA Rochelle" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  
  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% str_squish()
  data$DATE  <- gsub(" .*", "", data$DATE)
  
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
  data$DATE <- gsub("-201", "-1", data$DATE) 
  data$DATE <- gsub("-200", "-0", data$DATE)
  data$DATE %<>% multidate(c("%m-%d-%y","%m/%d/%y"))
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # state
  # data$state <- gsub(" ","", data$state)
 

 
 data %<>%
   mutate(FROM = str_remove_all(FROM, "\\[norg\\]|norgl|\\[no orgl|\\[no org\\]"))
 

  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
    mutate(FROM = str_split(FROM, ";")) %>%
    unnest(FROM)
  
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(str_detect(FROM,"Senate|SENATE|Senator|SENATOR|MAJORITY LEADER"), "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "House|HOUSE|Representative|REPRESENTATIVE|REPRESENTATIAVE|^REP |CONGRESSMAN"), "House", chamber))
  
  
  
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)

  
  data %<>%
    mutate(FROM = str_remove_all(FROM, "Honorable"))
 

  data %<>%
    mutate(FROM = str_remove_all(FROM, "JR,|Jr,|jr,|JR\\.|Jr\\.|Mr\\.|Ms\\.|Mrs\\.|Dr\\."))
  

  
  data %<>%
    mutate(FROM = str_replace(FROM, "Warner, Mark R US", "Warner, Mark R")) %>%
    mutate(FROM = str_replace(FROM, "BROWN, SHERROD \\.", "Brown, Sherrod")) %>%
    mutate(FROM = str_replace(FROM, "STOCKMAN, STEVE \\(R\\) TEXAS", "Stockman, Steve")) %>%
    mutate(FROM = str_replace(FROM, "Miller, Mke", "Miller, Mike")) %>%
    mutate(FROM = str_replace(FROM, "FRANKEN, ALS", "FRANKEN, AL S")) %>%
    mutate(FROM = str_replace(FROM, "BACHUS, SPENCERT", "Bachus, Spencer T")) %>%
    mutate(FROM = str_replace(FROM, "TIBERI, PATRICKJ", "Tiberi, Patrick J")) %>%
    mutate(FROM = str_replace(FROM, "ESHOO, ANNAG", "Eshoo, Anna G")) %>%
    mutate(FROM = str_replace(FROM, "KIRK, MARKS", "Kirk, Mark S")) %>%
    mutate(FROM = str_replace(FROM, "LAUTENBERG, FRANKR", "Lautenberg, Frank R")) %>%
    mutate(FROM = str_replace(FROM, "LEAHY, PATRICKJ", "Leahy, Patrick J")) %>%
    mutate(FROM = str_replace(FROM, "ROBACH, JOSEPH THE", "ROBACH, JOSEPH")) %>%
    mutate(FROM = str_replace(FROM, "HECK, JOESPH J", "HECK, JOSEPH J")) %>%
    mutate(FROM = str_replace(FROM, "SCOTT, DESJARLAIS", "Desjarlais, Scott")) %>%
    mutate(FROM = str_replace(FROM, "Cantwell, Ms Maria", "Cantwell, Maria")) %>%
    mutate(FROM = str_replace(FROM, "SENSENBRENNER, F\\. JAMES", "Frank SENSENBRENNER")) %>%
    mutate(FROM = str_replace(FROM, "BUTTERFIELD, G\\.K\\.", "George BUTTERFIELD")) %>%
    mutate(FROM = str_replace(FROM, "YOUNG, C\\.W\\. BILL|Young, C\\. W\\. Bill", "Bill YOUNG")) %>%
    mutate(FROM = str_replace(FROM, "McNerney, Jerr", "McNerney, Jerry")) %>%
    mutate(FROM = str_replace(FROM, "Drier, David", "Dreier, David")) %>%
    mutate(FROM = str_replace(FROM, "Schwartz, Ms Allyson Y", "Schwartz, Allyson Y")) %>%
    mutate(FROM = str_replace(FROM, "Alexander, Chairman Lamar", "Alexander, Lamar")) %>%
    mutate(FROM = str_replace(FROM, "Bryne, Bradley", "Byrne, Bradley")) %>%
    mutate(FROM = str_replace(FROM, "BUSCHON,  LARRY", "Larry BUCSHON")) %>%
    mutate(FROM = str_replace(FROM, "Bryne,  Bradley", "BYRNE, Bradley")) %>%
    mutate(FROM = str_replace(FROM, "Stefank, Elise", "STEFANIK, Elise")) %>%
    mutate(FROM = str_replace(FROM, "Torres-Small,  Xochitl", "Torres Small, Xochitl")) %>%
    mutate(FROM = str_replace(FROM, "Coleman Watson, Barbara", "Watson Coleman, Barbara")) %>%
    mutate(FROM = str_replace(FROM, "Enzi, Sen Mke", "Enzi, Mike")) %>%
    mutate(FROM = str_replace(FROM, "BOOKER,  COREY", "BOOKER, CORY")) %>%
    mutate(FROM = str_replace(FROM, "CONAWAY, K\\. MICHAEL", "CONAWAY, MICHAEL")) %>%
    mutate(FROM = str_replace(FROM, "PRICE, TOM", "Tom Edmunds PRICE"))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "\\.,", ","))
  
  
  # trim extra white space before or after name
  data$FROM %<>% trimws()
  

  ################


  # #extract member names
  data <-  extractMemberName(data,members,"FROM") 
  

  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "von Eschenbach, Andrew C"), "Commissioner of Food and Drugs", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HAMBURG, MARGARET"), "Commissioner U\\\\.S. Food and Drug Administration", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Dutcher, Michael Minneapolis District Office"), "Minneapolis District Office", ERROR)) %>%
    mutate(ERROR = ifelse(FROM %in% c("U.S.-China Economic, .", "Ireland, Jeanne", "ST.JOHN ST. JOHN MEDICAL CENTER", "UNIVERSITY OF ROCHESTER MEDICAL CENTER","INDIANA UNIVERSITY SCHOOL OF MEDICINE", "Hyde, Marleice", "SIPOS, TIBOR DIGESTIVE CARE, INC.", "Unknown, Unknown",
                                      "CONSTITUENT", "Constituent", "CTMG, NA", "FOOD AND DRUG ADMINISTRATION\\/CENTER FOR FOOD SAFETY AND APPLIED NUTRITION|CONSTITUENTS", "HOWARD, SALLY A FDA\\/OC\\/OPP\\/","HOWARD, SALLY A", "VITALE, JOSEPH", "BJORKLUND, CYBELE", "Boyd, Patrick", "Conrady-Brown, Michelle"), "Not Member of Congress", ERROR)) %>%
    mutate(NOTES = ifelse(FROM %in% c("Addtional", "E&C Committee, U\\. S\\. Congress","Additional", "CMTE ON HEALTH, EDUCATION, LABOR & PENSIONS", "Help Committee", "SPECIAL COMMITTEE ON AGING"), "Multiple unnamed Members of Congress", NOTES)) %>%
    mutate(ERROR = ifelse(FROM %in% c("Liston, Larry", "Jackson, Brent", "Nozzolio, Michael", "Hannon, Kemp", "Miller, Mike", "GRIFFO, JOSEPH A", "Brown, Kate"), "State Legislator", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "FALEOMAVAEGA, ENI F\\.H\\.|Sablan, Kilili"), "Non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "WARNER, CAITLIN"), "Agency staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "CLINTON, HILLARY RODHAM") & congress %in% 114, "No longer in congress", ERROR))
    
  
  
  unfoundnames<- data %>%
   filter(is.na(last_name) & is.na(ERROR))
  
  data %>%
    filter(LetterID == 000674)%>%
    select()
  
  unfoundnames %<>%
    select(ID, DATE, FROM, SUBJECT, last_name, everything())
  
    
  data %<>%
    arrange(-nchar(FROM))
  
  # Dropping this for now because it is incomplete and we get more observations without it
  # If we rewrite Clean.R, we may be able to add chamber back in
  # FIXME
  data %<>% select(-chamber)
  
  return(data)
  }

