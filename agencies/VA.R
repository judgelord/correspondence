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
  #data <- data[sample(1:nrow(data), 5000, replace=FALSE),]

  #data <- sampledata

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
  mutate(FROM = str_replace(FROM, "Butterfield, G\\. K\\.", "BUTTERFIELD, George")) %>%
  mutate(FROM = str_replace(FROM, "Rodgers, C", "McMORRIS RODGERS, Cathy")) %>%
  mutate(FROM = str_replace(FROM, "Barrett, J\\. G\\.|Barrett, J\\.G\\.", "BARRETT, James")) %>%
  mutate(FROM = str_replace(FROM, "Chabliss, S\\.", "CHAMBLISS, Saxby")) %>%
  mutate(FROM = str_replace(FROM, "Mikulski, B,|Milkulski, B|Mukulski, B\\.", "MIKULSKI, Barbara")) %>%
  mutate(FROM = str_replace(FROM, "Mack, M|Bono Mack, M\\.|Bono-Mack, M", "BONO, Mary")) %>%
  mutate(FROM = str_replace(FROM, "Brownbeck, S", "BROWNBACK, Sam Dale")) %>%
  mutate(FROM = str_replace(FROM, "Young, C\\.W\\.B\\.", "YOUNG, Charles William")) %>%
  mutate(FROM = str_replace(FROM, "Klobuchan, A", "KLOBUCHAR, Amy")) %>%
  mutate(FROM = str_replace(FROM, "Herseth, S|Sandlin, S|Herseth-Sandlin, S", "HERSETH SANDLIN, Stephanie")) %>%
  mutate(FROM = str_replace(FROM, "Sanford, B", "BISHOP, Sanford")) %>%
  mutate(FROM = str_replace(FROM, "Blunt-Rochester, L\\.", "BLUNT ROCHESTER, Lisa")) %>%
  mutate(FROM = str_replace(FROM, "Carter, E\\.L", "Earl Leroy CARTER")) %>%
  #mutate(FROM = str_replace(FROM, "Davis, A\\.", "DAVIS, Artur")) %>% # might be Davis, Susan A. as well
  mutate(FROM = str_replace(FROM, "Kilroy, M\\. J\\.", "KILROY, Mary Jo")) %>%
  mutate(FROM = str_replace(FROM, "Enzi, M\\.", "ENZI, Michael")) %>%
  mutate(FROM = str_replace(FROM, "Conaway, K\\.M\\.|Conaway, K\\. M\\.", "CONAWAY, Kenneth")) %>%
  mutate(FROM = str_replace(FROM, "Clay, L\\.|Clay, Wm\\.|Clay, W\\. L\\.", "CLAY, William")) %>%
  mutate(FROM = str_replace(FROM, "Udall, T\\.", "UDALL, Thomas")) %>%
  mutate(FROM = str_replace(FROM, "Sensenbrenner, F\\. J\\.", "SENSENBRENNER, Frank")) %>%
  mutate(FROM = str_replace(FROM, "Kuster, A\\. M\\.", "KUSTER, Ann")) %>%
  mutate(FROM = str_replace(FROM, "Beutler, J\\. H\\.|Herrera Beutler, J\\.", "HERRERA BEUTLER, Jaime")) %>%
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
  mutate(FROM = str_replace(FROM, "Jackson-Lee, S", "JACKSON LEE, Sheila")) %>%
  mutate(FROM = str_replace(FROM, "Kissel, L\\.", "KISSELL, Larry")) %>%
  mutate(FROM = str_replace(FROM, "Bond, C\\. \\(Kit\\)", "BOND, Christopher")) %>%
  mutate(FROM = str_replace(FROM, "Casey, B\\.", "CASEY, Robert"))
  
  

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
  mutate(chamber = ifelse(str_detect(FROM, "Isakson, J") & congress %in% c(110) & str_detect(chamber, "House"), str_replace(chamber, "House", "Senate"), chamber))


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
  
  #Filter for Unfoundnames
  Unfoundnames <- data %>%
    filter(is.na(last_name), !is.na(string)) %>%
    select(-last_name, -first_name)
  
  
  #Membership Errors
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "SVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "non-cong"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Non-Congressional"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "HVAC"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative Pellito, John"), "House Staff", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Gonzalez-Colon, J\\.|Pierluisi|Bordallo|Norton|Faleomavaega|Christensen|Representative Sablan, Gregorio|Representative Radewagen, A|Representative Sablan, G.|Representative Sablan, G|Radewagen, A\\.|Radewagen, A\\.|Sablan, G\\."), "non voting member", NOTES)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Representative non Congressional|Representative Non-Congressional|Representative NonCongressional|Representative NY-25"), "non member", ERROR))
  
  #FOIA NOTES
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "Young, D."), "Multiple Young's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Miller, G."), "Multiple Miller's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Rogers, M."), "Multiple Rogers' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Davis"), "Multiple Davis' FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Rose"), "Multiple Rose's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Smith"), "Multiple Smith's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Representative Johnson"), "Multiple Johnson's FOIA", NOTES)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Davis, A\\."), "Multiple Davis, A's FOIA", NOTES)) #Davis, Artur/Davis, Susan A.
  

  
 
  Unfoundnames2 <- data %>%
    filter(is.na(last_name),
           is.na(ERROR), 
           is.na(NOTES),
           str_detect(pattern, "404error"))
  
# Unfoundnames2 %<>% extractMemberName(members, "FROM")
  
   #Check after run through merge
   #Unmatched <- d %>%
   #filter(is.na(bioname))
  
  

  
 return(data)
  
}

  