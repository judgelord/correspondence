# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information


#file.name <- "FDA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # remove NA rows
  data <- data[-which(is.na(data$FROM)),]
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE  <- gsub(" .*", "", data$DATE)
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # paste all relevent info into subject col
  data %<>% 
    mutate(SUBJECT = paste(SUBJECT)) 
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl("Senate|SENATE|Senator|SENATOR|MAJORITY LEADER", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("House|HOUSE|Representative|REPRESENTATIVE|REPRESENTATIAVE|^REP ", FROM), "House", chamber))
  
  # state
  # data$state <- gsub(" ","", data$state)
 
 data %<>% select(ID, DATE,  FROM, everything())
  
 data %<>%
   mutate(FROM = str_replace(FROM, "Jackson, Brent MCINTYRE, MIKE", "Jackson, Brent; MCINTYRE, MIKE")) %>%
   mutate(FROM = str_replace(FROM, "16 Addtional BURR, RICHARD", "16 Addtional; BURR, RICHARD"))
 
  # Add semi colons in rows with multiple congressman
  data$FROM <- gsub("(.*?)(REPRESENTATIVES|SENATOR|OF THE UNITED STATES|UNITED STATES SENATE|SENATE|Congressman) (\\w+)",'\\1\\2; \\3',data$FROM, ignore.case = T)

 
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
    mutate(FROM = str_split(FROM, ";|\\[norg\\]|norgl")) %>%
    unnest(FROM)
  
  #for(i in 1:nrow(data)){
    #if(grepl(";|\\[norg\\]|norgl", data$FROM[i])) {
      
     # new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";|\\[norg\\]|norgl") + 1))
      #new$FROM <- unlist(str_split(data$FROM[i], ";|\\[norg\\]|norgl"))
      
      #data <- rbind(data, new)
      
    #}
  #}
  #data <- data[-grep(";|\\[norg\\]|norgl", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  data <- data[-grep("^(Originator/ Org|Constituent|\\[norg\\]|\\[norg\\] \\[norg\\]|Corr. Date:|Due Date:|Signature Level:|Source:|Status Date:)$",data$FROM),] # removes observations
  
  # clean from
  data %<>%
    mutate(FROM = (str_remove_all(FROM, " UNITED.*| SENATE.*| HOUSE.*|\\[no org\\] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et.al|et. al|Honorable|\\[No Org\\]|Mr.|\\[NO ORG\\]| House of Representatives| OFFICE.*| \\[no org\\]| \\[no orgl|
                              ASSOCIATE.*| SENATOR.*| HOUSE OF REPRESENTATIVES.*| ASSOCIATE COMMISSIONER.*| HOUSE OF REPRESENTATIVES.*| CONGRESS.*|fno orgl |Ino orgl |\\[no orgl | U.S. Senate|FDA\\/OC\\/OPP\\/| G FDA\\/OPPLA\\/OL\\/|Mr. |Ms. |
                              Dr. |Inc | Honorable| Sen| CONGRESSIONAL.*| Food & Drug Administration| LIBRARY OF| SUBCMTE.*| NEW MEXICO STATE| GenPak Solutions, LLC| Commissioner of Food and Drugs|United States.*| District 47, Florida|
                                  anonymous |anonymous, anonymous | Ino orgl")))
  data %<>%
    mutate(FROM = str_remove_all(FROM, "JR,|Jr,|jr,|JR.|Jr."))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "Warner, Mark R US", "Warner, Mark R")) %>%
    mutate(FROM = str_replace(FROM, "BROWN, SHERROD .", "Brown, Sherrod")) %>%
    mutate(FROM = str_replace(FROM, "STOCKMAN, STEVE (R) TEXAS", "Stockman, Steve")) %>%
    mutate(FROM = str_replace(FROM, "Miller, Mke", "Miller, Mike"))
  
  #data$FROM <- gsub(" UNITED.*| SENATE.*| SENATOR.*| HOUSE.*|[no org] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et. al|et.al","", data$FROM)
  
data$FROM <- gsub("^ ","", data$FROM)
  

  ################
  
  

  # extract member names
  data %<>%
    getFirstLast.Comma("FROM")

 
data %<>%
  filter( ! FROM %in% c("[no org]", "Rec/Create Date:", "Office:","[no person]", "CONSTITUENT", "BE3H","BE3^^H","Ino orgl", "UNITED STATES", "SENATE", "fno orgl" ))

  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "von Eschenbach, Andrew C"), "Commissioner of Food and Drugs", ERROR)) %>%
    mutate(ERROR = ifelse(FROM %in% c("U.S.-China Economic, .", "Ireland, Jeanne", "ST.JOHN ST. JOHN MEDICAL CENTER", "UNIVERSITY OF ROCHESTER MEDICAL CENTER", 
                                      "INDIANA UNIVERSITY SCHOOL OF MEDICINE", "Hyde, Marleice", "SIPOS, TIBOR DIGESTIVE CARE, INC.", "Unknown, Unknown"), "Not Member of Congress", ERROR)) %>%
    mutate(ERROR = ifelse(FROM %in% c("Addtional", "E&C Committee, U. S. Congress"), "Multiple unnamed Members of Congress", NOTES))
  
#Filter while working (Comment out) 
  #data %<>%
   #filter( ! FROM %in% c("U.S.-China Economic, .", "Ireland, Jeanne", "Addtional", "E&C Committee, U. S. Congress", "von Eschenbach, Andrew C", "ST.JOHN ST. JOHN MEDICAL CENTER", "[no orq]", "UNIVERSITY OF ROCHESTER MEDICAL CENTER",
  #"GINGREY, PHILLIP","INDIANA UNIVERSITY SCHOOL OF MEDICINE","Hyde, Marleice", "SIPOS, TIBOR DIGESTIVE CARE, INC.","Unknown, Unknown"))
 
  data <- data[!data$FROM == "",] # removes blank observations
  
  unfoundnames<- data %>%
   filter(is.na(last_name))
  
  unfoundnames %<>%
    select(ID, DATE, FROM, SUBJECT, last_name, everything())
  
  
  
}
