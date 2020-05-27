#This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# file.name <- "VA Rochelle" # for testing

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

  #Format Date
  # FIXME, if read as character, DATE will be string "NA" not real NA and this will fail:
  data %<>%
    mutate(DATE = if_else(is.na(DATE), `Date Inquiry Assigned`, DATE))
  
  data$DATE %<>% as.Date("%Y-%m-%d")
  
  
  
  #Check for NA Dates # FIXME
  NoDATE <- data %>%
    filter(is.na(DATE))
  
  data %<>% anti_join(NoDATE)

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

  
  #Filter out rows without data
  data %<>% 
    mutate(FROM = ifelse(FROM %in% c("", "N/A","n/a","N/a", "n/a/","m/a","`N/A") | is.na(FROM),
                         person,
                         FROM))
  
  data %<>%
    mutate(FROM = str_remove(FROM, " N/A"))
  
  #sample
  #data <- data[sample(1:nrow(data), 10000, replace=FALSE),]

  #data <- sampledata
  
  data %<>%
    mutate(FROM = str_replace(FROM, "Sinema, K\\. Kirkpatrick, A\\. Barber, R\\.", "Sinema, K\\. \\/Kirkpatrick, A\\. \\/Barber, R\\.")) %>%
    mutate(FROM = str_replace(FROM, "Graves, S\\. Collins, C\\. Hanna, R\\.", "Graves, S\\. \\/Collins, C\\. \\/Hanna, R\\.")) %>%
    mutate(FROM = str_replace(FROM, "Rothfus, K\\. Miller, J\\. Coffman, M\\.", "Rothfus, K\\. \\/Miller, J\\. \\/Coffman, M\\.")) %>%
    mutate(FROM = str_replace(FROM, "Graves, S\\. Collins, C\\. Hanna, R\\.", "Graves, S\\. \\/Collins, C\\. \\/Hanna, R\\."))

  #Trim White Space
  data %<>%
    mutate(FROM = str_squish(FROM))
 
#Typo  
data %<>%
  mutate(FROM = str_replace(FROM, "VanHollen, C", "Van Hollen, C")) %>%
  mutate(FROM = str_replace(FROM, "^Balart, M\\.", "Diaz-Balart, M\\.")) %>%
  mutate(FROM = str_replace(FROM, "Diaz Balart, M\\.", "Diaz-Balart, M\\.")) %>%
  mutate(FROM = str_replace(FROM, "Johnson, E\\. B\\.", "Johnson, E\\.")) %>%
  mutate(FROM = str_replace(FROM, "Rashia, Jamie", "Raskin, Jamie")) %>%
  mutate(FROM = str_replace(FROM, "Carter, E\\.L\\.", "Carter, Earl")) %>%
  mutate(FROM = str_replace(FROM, "Capito, S\\.M\\.|Capito, S\\. M\\.", "Moore Capito, Shelley")) %>%
  mutate(FROM = str_replace(FROM, "Lujan Grisham, M\\.", "LUJAN GRISHAM, Michelle")) %>%
  mutate(FROM = str_replace(FROM, "Flemming,J,", "FLEMING, John")) %>%
  mutate(FROM = str_replace(FROM, "Scott, R\\. C\\.", "SCOTT, Robert")) %>%
  mutate(FROM = str_replace(FROM, "Forbes, R\\.", "FORBES, James")) %>%
  mutate(FROM = str_replace(FROM, "Butterfield, G\\.K\\.|Butterfield, G\\. K\\.", "BUTTERFIELD, George")) %>%
  mutate(FROM = str_replace(FROM, "Rodgers, C", "McMORRIS RODGERS, Cathy")) %>%
  mutate(FROM = str_replace(FROM, "Barrett, J\\. G\\.|Barrett, J\\.G\\.", "BARRETT, James")) %>%
  mutate(FROM = str_replace(FROM, "Chabliss, S\\.", "CHAMBLISS, Saxby")) %>%
  mutate(FROM = str_replace(FROM, "Mikulski B|Mikulski, B,|Milkulski, B|Mukulski, B\\.", "MIKULSKI, Barbara")) %>%
  mutate(FROM = str_replace(FROM, "Mack, M|Bono Mack, M\\.|Bono-Mack, M", "BONO, Mary")) %>%
  mutate(FROM = str_replace(FROM, "Brownbeck, S", "BROWNBACK, Sam Dale")) %>%
  mutate(FROM = str_replace(FROM, "Young, C\\. W\\. B\\.|Young, C\\.W\\.|Young, C\\.W\\.B\\.", "YOUNG, Charles William")) %>%
  mutate(FROM = str_replace(FROM, "Klobuchan, A", "KLOBUCHAR, Amy")) %>%
  mutate(FROM = str_replace(FROM, "Herseth, S|Sandlin, S|Herseth-Sandlin, S", "HERSETH SANDLIN, Stephanie")) %>%
  mutate(FROM = str_replace(FROM, "Sanford, B", "BISHOP, Sanford")) %>%
  mutate(FROM = str_replace(FROM, "Blunt-Rochester, L\\.", "BLUNT ROCHESTER, Lisa")) %>%
  mutate(FROM = str_replace(FROM, "Carter, E\\.L", "Earl Leroy CARTER")) %>%
  #mutate(FROM = str_replace(FROM, "Davis, A\\.", "DAVIS, Artur")) %>% # might be Davis, Susan A. as well
  mutate(FROM = str_replace(FROM, "Kilroy, M\\. J\\.", "KILROY, Mary Jo")) %>%
  mutate(FROM = str_replace(FROM, "Enzi, M\\.", "ENZI, Michael")) %>%
  mutate(FROM = str_replace(FROM, "Conaway, K, M\\.|Conaway, K\\. Michael|Conaway, K\\.M\\.|Conaway, K\\. M\\.", "CONAWAY, Kenneth")) %>%
  mutate(FROM = str_replace(FROM, "Clay, L\\.|Clay, Wm\\.|Clay, W\\. L\\.", "CLAY, William")) %>%
  mutate(FROM = str_replace(FROM, "Udall, T\\.", "UDALL, Thomas")) %>%
  mutate(FROM = str_replace(FROM, "Sensenbrenner, F\\. J\\.", "SENSENBRENNER, Frank")) %>%
  mutate(FROM = str_replace(FROM, "Kuster, A\\. M\\.", "KUSTER, Ann")) %>%
  mutate(FROM = str_replace(FROM, "Herrera-Beutler|Beutler, J\\. H\\.|Herrera Beutler, J\\.", "HERRERA BEUTLER, Jaime")) %>%
  mutate(FROM = str_replace(FROM, "Mccury, J\\.", "McCRERY, James")) %>%
  mutate(FROM = str_replace(FROM, "Kilder, D\\.", "KILDEE, Dale")) %>%
  mutate(FROM = str_replace(FROM, "Bryd, R", "BYRD, Robert")) %>%
  mutate(FROM = str_replace(FROM, "Aderhot, R\\.", "ADERHOLT, Robert")) %>%
  mutate(FROM = str_replace(FROM, "Buchson, L\\.", "BUCSHON, Larry")) %>%
  mutate(FROM = str_replace(FROM, "Gringrey, P\\.", "GINGREY, Phil")) %>%
  mutate(FROM = str_replace(FROM, "Joyce, D\\. P\\.", "JOYCE, David")) %>%
  mutate(FROM = str_replace(FROM, "Hanabusa, C\\. W\\.", "HANABUSA, Colleen")) %>%
  mutate(FROM = str_replace(FROM, "Nelson, E\\. B\\.", "NELSON, Earl Benjamin")) %>%
  mutate(FROM = str_replace(FROM, "Gallegley, E\\.", "GALLEGLY, Elton")) %>%
  mutate(FROM = str_replace(FROM, "Hutchinson, K", "HUTCHISON, Kathryn")) %>%
  mutate(FROM = str_replace(FROM, "Jackson-Lee|Jackson-Lee, S", "JACKSON LEE, Sheila")) %>%
  mutate(FROM = str_replace(FROM, "Kissel, L\\.", "KISSELL, Larry")) %>%
  mutate(FROM = str_replace(FROM, "Bond, C\\. \\(Kit\\)", "BOND, Christopher")) %>%
  mutate(FROM = str_replace(FROM, "Casey, B\\.", "CASEY, Robert")) %>%
  mutate(FROM = str_replace(FROM, "Owens, B\\.", "OWENS, William")) %>%
  mutate(FROM = str_replace(FROM, "Coleman, B\\. W\\.|Coleman, B\\.", "WATSON COLEMAN, Bonnie")) %>%
  mutate(FROM = str_replace(FROM, "Grotham, G\\.", "GROTHMAN, Glenn")) %>%
  mutate(FROM = str_replace(FROM, "Hartler, V\\.", "HARTZLER, Vicky")) %>%
  mutate(FROM = str_replace(FROM, "Roskan, P", "Roskam, P")) %>%
  mutate(FROM = str_replace(FROM, "Balwdin, T", "BALDWIN, Tammy")) %>%
  mutate(FROM = str_replace(FROM, "Haynes, R", "HAYES, Robert")) %>%
  mutate(FROM = str_replace(FROM, "McCaskell, C", "McCaskill, C")) %>%
  mutate(FROM = str_replace(FROM, "Scott, R\\.C\\.", "SCOTT, Robert")) %>%
  mutate(FROM = str_replace(FROM, "Gosar, P\\. A\\.", "GOSAR, Paul")) %>%
  mutate(FROM = str_replace(FROM, "Lowery, N\\.", "LOWEY, Nita")) %>%
  mutate(FROM = str_replace(FROM, "Fleishmann, C\\.", "FLEISCHMANN, Chuck")) %>%
  mutate(FROM = str_replace(FROM, "Keating W\\.", "KEATING, William")) %>%
  mutate(FROM = str_replace(FROM, "Heller, G\\.", "Heller, D\\.")) %>%
  mutate(FROM = str_replace(FROM, "Maloney, S\\. P\\.", "MALONEY, Sean")) %>%
  mutate(FROM = str_replace(FROM, "Barlett, R", "BARTLETT, Roscoe")) %>%
  mutate(FROM = str_replace(FROM, "Fox, V\\.", "Foxx, V\\.")) %>%
  mutate(FROM = str_replace(FROM, "Myrack, S", "MYRICK, Sue")) %>%
  mutate(FROM = str_replace(FROM, "Tubbs Jones, S", "Stephanie Tubbs JONES")) %>%
  mutate(FROM = str_replace(FROM, "Shelby R", "SHELBY, Richard")) %>%
  mutate(FROM = str_replace(FROM, "Wyden R\\.", "WYDEN, Ronald")) %>%
  mutate(FROM = str_replace(FROM, "Kird, M\\.", "KIRK, Mark")) %>%
  mutate(FROM = str_replace(FROM, "Wilson, F\\.S\\.", "WILSON, Frederica")) %>%
  mutate(FROM = str_replace(FROM, "Tipton, S\\. R\\.", "TIPTON, Scott")) %>%
  mutate(FROM = str_replace(FROM, "Busos, Cheri", "BUSTOS, Cheri")) %>%
  mutate(FROM = str_replace(FROM, "Blumenaurer, E", "BLUMENAUER, Earl")) %>%
  mutate(FROM = str_replace(FROM, "Boustany, C\\.W\\.", "BOUSTANY, Charles")) %>%
  mutate(FROM = str_replace(FROM, "Steube, G\\.", "STEUBE, William")) %>%
  mutate(FROM = str_replace(FROM, "Johnson, E\\.B\\.", "JOHNSON, Eddie Bernice")) %>%
  mutate(FROM = str_replace(FROM, "Johnson, H,", "JOHNSON, Hank")) %>%
  mutate(FROM = str_replace(FROM, "Franks\\. T", "FRANKS, Trent")) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Lee, S\\.") & congress %in% c(115), str_replace(FROM, "Lee, S\\.", "JACKSON LEE, Sheila"), FROM))
  
  

#Wrong chambers
data %<>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress %in% c(115) & str_detect(chamber, "Senate"), str_replace(FROM, "Kennedy, J.", "John Neely KENNEDY"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Kennedy, J.") & congress %in% c(115) & str_detect(chamber, "House"), str_replace(FROM, "Kennedy, J.", "Joseph P KENNEDY"), FROM)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Risch"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Carter, Earl"), str_replace(chamber, "Senate", "House"), chamber)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Moran, J.") & congress %in% c(112,113) & str_detect(chamber, "Senate"), str_replace(FROM, "Moran, J.", "MORAN, Jerry"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Moran, J.") & congress %in% c(112,113) & str_detect(chamber, "House"), str_replace(FROM, "Moran, J.", "MORAN, James"), FROM)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Murkowski") & congress %in% c(111) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Johnson, T\\.") & congress %in% c(111,112) & str_detect(chamber, "House"), str_replace(FROM, "Johnson, T\\.", "Timothy V JOHNSON"), FROM)) %>%
  mutate(FROM = ifelse(str_detect(FROM, "Johnson, T\\.") & congress %in% c(111,112) & str_detect(chamber, "Senate"), str_replace(FROM, "Johnson, T\\.", "Tim Peter JOHNSON"), FROM)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Chambliss, S\\.|Chambliss, S") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Cardin, B") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Isakson, J") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Smith, G") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Brown, S") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Thune, J") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Schumer, C") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber)) %>%
  mutate(chamber = ifelse(str_detect(FROM, "Sessions, J") & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber))


data %<>%
  mutate(FROM = str_replace(FROM, "McNerney, J\\. Denham, J\\.", "McNerney, J. \\/ Denham, J."))

#string split on "\"
  data %<>%
    mutate(FROM = str_split(FROM, "\\/")) %>%
    unnest(FROM)
  
  # # create ID 
  # data$ID <- 1:nrow(data)

  data %<>%
    mutate(FROM = str_remove(FROM, "\\/"))
  
  #Paste Chamber into FROM
  data %<>%
    mutate(FROM =ifelse(str_detect(chamber, "House") & !str_detect(FROM, ","), paste("Representative", FROM, sep = " "), FROM)) %>%
    mutate(FROM = ifelse(str_detect(chamber, "Senate") & !str_detect(FROM, ","), paste("Senator", FROM, sep = " "), FROM))
  
  #Extract Member Names
  data %<>%
    extractMemberName(members = members, col_name = "FROM")
  
  
  
  
  
   #Check for duplicates
   sample2data <- data %>%
     group_by(ID, SUBJECT, DATE) %>%
     mutate(n = n(),
            last_name = str_c(last_name, collapse = "; "))
  

  
  
  #Membership Errors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "SVAC|Rogerson, Dale|Blood, Richard"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative Hall R\\."), "state legislator", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "non-cong"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Non-Congressional"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative Pellito, John"), "House Staff", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Gonzalez Colon, J\\.|Gonzalez, J\\.|Faleomavaega, E\\.|Norton, E\\.|Pierluisi, P\\.|Gonzalez-Colon, J\\.|Pierluisi|Bordallo|Norton|Faleomavaega|Christensen|Representative Sablan, Gregorio|Representative Radewagen, A|Representative Sablan, G.|Representative Sablan, G|Radewagen, A\\.|Radewagen, A\\.|Sablan, G\\."), "non voting member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Plaskett, S\\.|Representative non Congressional|Representative Non-Congressional|Representative NonCongressional|Representative NY-25"), "non member", ERROR))
  
  #FOIA NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Young, D."), "Multiple Young's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Senator Scott"), "Multiple Senator Scott's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Miller, G."), "Multiple Miller's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Rogers, M."), "Multiple Rogers' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Davis"), "Multiple Davis' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Rose"), "Multiple Rose's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Smith"), "Multiple Smith's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Johnson"), "Multiple Johnson's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Davis, A\\."), "Multiple Davis, A's FOIA", NOTES)) #Davis, Artur/Davis, Susan A.
 

 
  Unfoundnames <- data %>%
    filter(is.na(last_name),
           is.na(ERROR), 
           is.na(NOTES),
           str_detect(pattern, "404error"),
           ! str_detect(FROM, "Senator NA|Representative NA"))
  
# Unfoundnames2 %<>% extractMemberName(members, "FROM")
  
   #Check after run through merge
   #Unmatched <- d %>%
   #filter(is.na(bioname))
  
  

  
 return(data)
  
}

  