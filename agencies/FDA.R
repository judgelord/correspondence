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
  
 # Add semi colons in rows with multiple congressman
  data$FROM <- gsub("(.*?)(REPRESENTATIVES|SENATOR|OF THE UNITED STATES|UNITED STATES SENATE|SENATE|LLC|Inc\\.,|Inc\\.) (\\w+)",'\\1\\2; \\3',data$FROM, ignore.case = T)

  data %<>%
    mutate(FROM = str_replace(FROM, "Addtional", "Addtional;")) %>%
    mutate(FROM = str_replace(FROM, "[0-9]+", ";"))
 
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  data %<>%
    mutate(FROM = str_split(FROM, ";|\\[norg\\]|norgl|\\[no orgl|\\[no org\\]")) %>%
    unnest(FROM)
  
  #Filter while working
  #data %<>%
    #filter( ! FROM %in% c("HAMBURG, MARGARET Commissioner U.S. Food and Drug Administration", "HAMBURG, M.D., MARGARET Food and Drug Administration"))
  
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  data <- data[-grep("^(Originator/ Org|Constituent|\\[norg\\]|\\[norg\\] \\[norg\\]|Corr. Date:|Due Date:|Signature Level:|Source:|Status Date:)$",data$FROM),] # removes observations
  
  # clean from
  data %<>%
    mutate(FROM = (str_remove_all(FROM, " UNITED.*| SENATE.*| HOUSE.*|\\[no org\\] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et\\.al|et\\. al|Honorable|\\[No Org\\]|Mr.|\\[NO ORG\\]| House of Representatives| OFFICE.*| \\[no org\\]| \\[no orgl|
                              ASSOCIATE.*| SENATOR.*| HOUSE OF REPRESENTATIVES.*| ASSOCIATE COMMISSIONER.*| HOUSE OF REPRESENTATIVES.*| CONGRESS.*|fno orgl |Ino orgl |\\[no orgl | U.S. Senate|FDA\\/OC\\/OPP\\/| G FDA\\/OPPLA\\/OL\\/|Mr\\. |Ms\\. |
                              Dr\\. |Inc | Honorable| CONGRESSIONAL.*| Food & Drug Administration| LIBRARY OF| SUBCMTE.*| NEW MEXICO STATE| GenPak Solutions, LLC| Commissioner of Food and Drugs|United States.*| District 47, Florida|
                                  anonymous |anonymous, anonymous | Ino orgl|\\)| FDA\\/OO\\/OHR\\/DPPER\\/|FDA\\/OMPT\\/CDER\\/OND\\/OAP\\/DAIP\\/|\\(b \\(|FDA\\/OGROP\\/ORA\\/OEIO\\/DFDT\\/|Naturals|Dr. |Sen |Senate|Sen | District.*| State.*")))
  data %<>%
    mutate(FROM = str_remove_all(FROM, "JR,|Jr,|jr,|JR\\.|Jr\\."))
  
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
    mutate(FROM = str_replace(FROM, "BUSCHON,  LARRY", "Larry BUCSHON"))
  
  data %<>%
    mutate(FROM = str_replace(FROM, "\\.,", ","))
  
  #data$FROM <- gsub(" UNITED.*| SENATE.*| SENATOR.*| HOUSE.*|[no org] |OF THE UNITED STATES|\\(b\\) \\(6\\)| House.*|et. al|et.al","", data$FROM)
  
  # trim extra white space before or after name
  data$FROM %<>% trimws()
  

  ################
  
  

  # #extract member names
  data <-  extractMemberName(data,members,"FROM") 
  
  #Check for Duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, SUBJECT, DATE) %>%
    mutate(n = n(),
           last_name = str_c(last_name, collapse = "; "))

 
data %<>%
  filter( ! FROM %in% c("\\[no org\\]", "Rec\\/Create Date:", "Office:","\\[no person\\]", "BE3H","BE3^^H","Ino orgl", "UNITED STATES",
                        "SENATE", "fno orgl", "BS", "\\)", ".", "BE", "H", "^^H", "CONGRESS", "reiBi" ))

  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "von Eschenbach, Andrew C"), "Commissioner of Food and Drugs", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HAMBURG, MARGARET"), "Commissioner U.S. Food and Drug Administration", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Dutcher, Michael Minneapolis District Office"), "Minneapolis District Office", ERROR)) %>%
    mutate(ERROR = ifelse(FROM %in% c("U.S.-China Economic, .", "Ireland, Jeanne", "ST.JOHN ST. JOHN MEDICAL CENTER", "UNIVERSITY OF ROCHESTER MEDICAL CENTER", 
                                      "INDIANA UNIVERSITY SCHOOL OF MEDICINE", "Hyde, Marleice", "SIPOS, TIBOR DIGESTIVE CARE, INC.", "Unknown, Unknown",
                                      "CONSTITUENT", "Constituent", "CTMG, NA", "FOOD AND DRUG ADMINISTRATION/CENTER FOR FOOD SAFETY AND APPLIED NUTRITION|CONSTITUENTS"), "Not Member of Congress", ERROR)) %>%
    mutate(NOTES = ifelse(FROM %in% c("Addtional", "E&C Committee, U. S. Congress","Additional", "CMTE ON HEALTH, EDUCATION, LABOR & PENSIONS", "Help Committee", "SPECIAL COMMITTEE ON AGING"), "Multiple unnamed Members of Congress", NOTES)) %>%
    mutate(ERROR = ifelse(FROM %in% c("Liston, Larry", "Jackson, Brent", "Nozzolio, Michael", "Hannon, Kemp", "Miller, Mike", "GRIFFO, JOSEPH A"), "State Legislator", ERROR))
    
  
#Filter while working (Comment out) 
  data %<>%
   filter( ! FROM %in% c("U\\.S\\.-China Economic, \\.", "Ireland, Jeanne", "Addtional", "E&C Committee, U\\. S\\. Congress", "von Eschenbach, Andrew C", "ST\\.JOHN ST\\. JOHN MEDICAL CENTER", "[no orq]", "UNIVERSITY OF ROCHESTER MEDICAL CENTER",
  "GINGREY, PHILLIP","INDIANA UNIVERSITY SCHOOL OF MEDICINE","Hyde, Marleice", "SIPOS, TIBOR DIGESTIVE CARE, INC\\.","Unknown, Unknown", "Dutcher, Michael Minneapolis District Office", "CONSTITUENT", "Additional",
  "CMTE ON HEALTH, EDUCATION, LABOR & PENSIONS", "Help Committee", "CTMG, NA","FOOD AND DRUG ADMINISTRATION/CENTER FOR FOOD SAFETY AND APPLIED NUTRITION", "Liston, Larry", "Jackson, Brent, Constituent"))
 data %<>%
   filter(! str_detect(FROM, "Addtional|Constituent|BJORKLUND, CYBELE"))

  
 data <- data[!data$FROM == "",] # removes blank observations
  
  unfoundnames<- data %>%
   filter(is.na(last_name) & is.na(ERROR))
  
  unfoundnames %<>%
    select(ID, DATE, FROM, SUBJECT, last_name, everything())
  
  # Dropping this for now because it is incomplete and we get more observations without it
  # If we rewrite Clean.R, we may be able to add chamber back in
  # FIXME
  data %<>% select(-chamber)
  
  return(data)
  }

