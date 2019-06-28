##########################################################################################################
# This script defines functions for cleaning and extracting names of members of congress
#' cleanFROMcolumn() preprocesses text to make matching more likely
#' formatFirstName() and formatLastName()formats names to look like those provided by voteview. These are used by other functions
#' getFirstLast.Comma() looks for names in the format Last, First only 
#' extractMemberNames looks for names in many formats
#' addFirst() adds first names given last names, but only to last names that are unique in congress. This should be used with caution.
##########################################################################################################


# This function cleans up text from which member names will be extracted.
# SUCH CODE SHOULD BE CONSOLIDATED HERE
# It is used in extractMemberNames etc. to preprocess text.
cleanFROMcolumn <- function(FROM){

  # remove 
  FROM <- gsub('\\+', "", FROM)
  
  # remove common names in quotes 
  FROM <- gsub('\\"(Bobby|Buddy|GT|Buck|Chuck|Rick)\\"', "", FROM, ignore.case = TRUE)
  
  # remove paragraph breaks and trailing white space 
  FROM <- gsub("\n", " ", FROM)
  FROM <- trimws(FROM)
  
  # trim down extra spaces
  #FROM <- gsub(" +", " ", FROM) # extra spaces
  
  # remove 
  FROM <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", FROM)
  FROM <- gsub("(^ |^  |^   |\n)", "", FROM)
  FROM <- gsub("(REP|SEN)(\\.|- | - |\\. )|(^S(-| ))|Congressman|Congresswoman|Sen\\.|(^(R|C)(-| ))|Repres|Rep |Sen ", "", FROM)
  
  # replace with comma
  FROM <- gsub(pattern = ", CPA,|, M.D.|, M.D.,|, MD,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\),Rep\\.| \\(Il\\),Sen\\.",
               replacement = ",", FROM)
  
  # replace with "U.S."
  FROM <- gsub(pattern = "Member, U.S", "U.S", FROM)
  
  # remove periods
  FROM <- gsub(pattern= "\\.\\.", replacement = " ", FROM) 
  FROM <- gsub(pattern= "\\.", replacement = " ", FROM) 

  # replace spaces with a single space
  FROM <- gsub(" +", " ", FROM) # extra spaces
  FROM <- trimws(FROM)
  
  return(FROM)
}

###################################################################################################################

# Formats col_name (usually last_name) to similiar format as members$last_name
# Capitalizes letters and fixes common errors 
formatLastName <- function(data, col_name){
  
  data$last_name <- data[[col_name]]
  
  # trim white space and paragraph breaks
  data$last_name %<>% trimws()
  
  # THIS WILL STAY IN THE FUNCTION formatLastName
  data %<>%
    # correct capitalization to match last names in voteview data 
    # Last names in voteview are upper case
    mutate(last_name = str_to_upper(last_name)) %>% 
    #case corrections, not touching at the moment
    mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
    mutate(last_name = gsub("McEACHIN", replacement = "MCEACHIN", last_name, ignore.case = TRUE)) %>% 
    mutate(last_name = gsub("DEFAZIO", replacement = "DeFAZIO", last_name, ignore.case = TRUE)) %>% 
    mutate(last_name = gsub("DELAURO", replacement = "DeLAURO", last_name)) %>% 
    mutate(last_name = gsub("DEMINT", replacement = "DeMINT", last_name)) %>% 
    mutate(last_name = gsub("LOBIONDO", replacement = "LoBIONDO", last_name)) %>% 
    mutate(last_name = gsub("LATOURETTE", replacement = "LaTOURETTE", last_name)) %>% 
    mutate(last_name = gsub("LAHOOD", replacement = "LaHOOD", last_name)) %>% 
    mutate(last_name = gsub("DEGETTE", replacement = "DeGETTE", last_name)) %>% 
    mutate(last_name = gsub("DELBENE", replacement = "DelBENE", last_name)) %>% 
    mutate(last_name = gsub("DESANTIS", replacement = "DeSANTIS", last_name)) %>% 
    mutate(last_name = gsub("MACARTHUR", replacement = "MacARTHUR", last_name)) %>% 
    mutate(last_name = gsub("LAMALFA", replacement = "LaMALFA", last_name)) %>% 

    # Commented this out because we modified the members file instead, but maybe it would better to modify just the search pattern to be Luj.n as a typo
    # FIXED
    # mutate(last_name = ifelse(grepl("Lujan", FROM,ignore.case=TRUE)&grepl("Ben", FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>% 
    # mutate(last_name = ifelse( grepl("Lujan",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>%
    

    ##############################################################################################################################
    # FIXED
    # All of the below should be corrected with the typos tables (if a typo) or in nameCongress.R (if a name that needs expanding)
    # Spelling and specific corrections
  
    # FIXED and added names to typo tables
    mutate(last_name = gsub("DENIS", replacement = "DENNIS", last_name)) %>% #fixed
    mutate(last_name = gsub("DUNCAN JOHN.*", replacement = "DUNCAN", last_name)) %>% #fixed
    mutate(last_name = gsub("JOHNSON HENRY.*", replacement = "JOHNSON", last_name)) %>% #fixed
    mutate(last_name = gsub("BONO MACK.*", replacement = "BONO", last_name)) %>% #this should be Mary not Mack #fixed 
    mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% #fixed
    mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name)) %>%  #fixed


    mutate(last_name = gsub("MOORE CAPITO.*", replacement = "CAPITO", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Milkulski, Barbara", FROM), "MIKULSKI", last_name)) %>% #fixed 
    mutate(last_name = ifelse(grepl("GRESHAM BARRETT", last_name,ignore.case=TRUE), "BARRETT", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Shelley Moore", FROM,ignore.case=TRUE), "CAPITO", last_name)) %>% #fixed
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) %>% #will be fixed below
    mutate(last_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM), "JACKSON LEE", last_name)) %>% #fixed
    mutate(last_name = ifelse( (grepl("McMorris|Rodgers",FROM,ignore.case=TRUE))&grepl("Cathy|McMorris",FROM), "McMORRIS RODGERS", last_name)) %>% #fixed below
    mutate(last_name = ifelse( grepl("Michael|(^| )K",FROM,ignore.case=TRUE)&grepl("Conaway",FROM,ignore.case=TRUE), "CONAWAY", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Ben",FROM)&grepl("Nelson",FROM), "NELSON", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Gillbrand",FROM,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Hillary|Hilary",FROM,ignore.case=TRUE)&grepl("Rodham",FROM), "CLINTON", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Sandlin", FROM,ignore.case=TRUE), "HERSETH SANDLIN", last_name)) %>%  #fixed
    mutate(last_name = ifelse(grepl("Murhpy", FROM,ignore.case=TRUE), "MURPHY", last_name)) %>% #fixed
    #mutate(last_name = ifelse( grepl("Linda",FROM,ignore.case=TRUE)&grepl("Sanchez",FROM,ignore.case=TRUE), "SÁNCHEZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Wasserman", FROM,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("McMorris", FROM,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("ROS-LEHTINEN", FROM ,ignore.case=TRUE), "ROS-LEHTINEN", last_name)) %>% #not an error
    mutate(last_name = ifelse(grepl(".ISCLOSKY", FROM,ignore.case=TRUE), "VISCLOSKY", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Guitierrez", FROM,ignore.case=TRUE), "GUTIERREZ", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Harmon", FROM,ignore.case=TRUE), "HARMAN", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Hollen", FROM,ignore.case=TRUE), "VAN HOLLEN", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Masto", FROM,ignore.case=TRUE), "CORTEZ MASTO", last_name)) %>%  #fixed
    
    mutate(last_name = ifelse(grepl("Roybal", last_name,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("RAHALL", last_name,ignore.case=TRUE), "RAHALL", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("BEUTLER", last_name,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Inholfe|Imhofe|Imholfe|Inhoffe", last_name,ignore.case=TRUE), "INHOFE", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Barrat|Barret", last_name,ignore.case=TRUE), "BARRETT", last_name)) %>%  #fixed
    mutate(last_name = ifelse(grepl("Stebenow", last_name,ignore.case=TRUE), "STABENOW", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("C.rdenas", last_name,ignore.case=TRUE), "CARDENAS", last_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Vel.zquez", last_name,ignore.case=TRUE), "VELAZQUEZ", last_name)) %>% #fixed
   
    
    mutate(last_name = gsub("GONZALES", replacement = "GONZALEZ", last_name)) #fixed
  
  data$last_name %<>% trimws()
  

  return(data$last_name)
  
}

##################################################################################################################################

# Formats col_name (usually first_name) to similiar format as members$first_name
# Capitalizes letters appropriately and fixes common errors
formatFirstName <- function(data, col_name){
  
  data$first_name <- data[[col_name]]
  #data %<>% mutate(FROM = ifelse("FROM2" %in% names(data), FROM2, FROM))
  
  data %<>%
    # In voteview, first names are title case
    mutate(first_name = stri_trans_totitle(first_name))
    
  ##############################################################################################################################
  # FIXED
  # All of the below should be corrected with the typos tables (if a typo) or in nameCongress.R (if a name that needs expanding)
  # Spelling and specific corrections
  data %<>% 
    mutate(first_name = ifelse( grepl("Don",FROM,ignore.case=TRUE)&grepl("Young",FROM,ignore.case=TRUE), "Donald", first_name)) %>% #fixed in namecongress
    #mutate(first_name = ifelse( grepl("Andr",FROM,ignore.case=TRUE)&grepl("Carson",FROM,ignore.case=TRUE), "André", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Thune",FROM,ignore.case=TRUE), "John", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Rockefeller",FROM,ignore.case=TRUE), "John", first_name)) %>%#no error
    mutate(first_name = ifelse( grepl("Harold",FROM,ignore.case=TRUE)&grepl("Rogers",FROM,ignore.case=TRUE), "Harold", first_name)) %>% #no error
    
    mutate(first_name = ifelse( grepl("James",FROM,ignore.case=TRUE)&grepl("Sensenbrenner",FROM,ignore.case=TRUE), "James", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Richard",FROM,ignore.case=TRUE)&grepl("Blumenthal",FROM,ignore.case=TRUE), "Richard", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Bill",FROM,ignore.case=TRUE)&grepl("Nelson",FROM,ignore.case=TRUE), "Clarence", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Fred",FROM,ignore.case=TRUE)&grepl("Upton",FROM,ignore.case=TRUE), "Frederick", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Thad",FROM,ignore.case=TRUE)&grepl("Cochran",FROM,ignore.case=TRUE), "William", first_name)) %>% #fixed in namecongress
    mutate(first_name = ifelse( grepl("Kristen",FROM,ignore.case=TRUE)&grepl("Gillibrand",FROM,ignore.case=TRUE), "Kirsten", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("C",FROM,ignore.case=TRUE)&grepl("Ruppersberger",FROM,ignore.case=TRUE), "Dutch", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Paul",FROM,ignore.case=TRUE)&grepl("Gosar",FROM,ignore.case=TRUE), "Paul", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Ros-Lehtinen",FROM,ignore.case=TRUE), "Ileana", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "Jaime", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Will|Bill",FROM,ignore.case=TRUE)&grepl("Owens",FROM,ignore.case=TRUE), "William", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Butterfield",FROM,ignore.case=TRUE)&grepl("G",FROM,ignore.case=TRUE), "George", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("G. K.",FROM,ignore.case=TRUE), "G.K.", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Nelson",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "Earl", first_name)) %>% #fixed in namecongress
  #mutate(first_name = ifelse( grepl("Carson",FROM,ignore.case=TRUE)&grepl("Andr",FROM,ignore.case=TRUE), "André", first_name)) %>% #fixed
   #mutate(first_name = ifelse( grepl("Griv",FROM,ignore.case=TRUE)&grepl("Raul",FROM,ignore.case=TRUE), "Raúl", first_name)) %>%
    mutate(first_name = ifelse( grepl("Scott",FROM,ignore.case=TRUE)&grepl("Bobby",FROM,ignore.case=TRUE), "Bob", first_name)) %>% #fixed
    
    
    mutate(first_name = ifelse( grepl("Young",FROM,ignore.case=TRUE)&grepl("C.W|C. W|CW",FROM,ignore.case=TRUE), "Charles", first_name)) %>%  #fixed
    mutate(first_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM,ignore.case=TRUE), "Sheila", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Gresham",FROM,ignore.case=TRUE)&grepl("Barrett",FROM,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Putnam",FROM,ignore.case=TRUE)&grepl("Ad",FROM,ignore.case=TRUE), "Adam", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Lind",FROM,ignore.case=TRUE)&grepl("Graham",FROM,ignore.case=TRUE), "Lindsey", first_name)) %>% #no error
    mutate(first_name = ifelse( grepl("SERRANO",FROM,ignore.case=TRUE)&grepl("Jos",FROM,ignore.case=TRUE), "Jose", first_name)) %>% #fixed
    
    mutate(first_name = gsub(pattern = "Christoher", replacement = "Christopher", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Hilllary|Hilary|Fillary", replacement = "Hillary", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Babara", replacement = "Barbara", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Colin", replacement = "Collin", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Melisssa", replacement = "Melissa", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub(pattern = "Denis", replacement = "Dennis", first_name,ignore.case=TRUE)) %>% #fixed
    mutate(first_name = gsub("Eliott", replacement = "Eliot", first_name)) %>% #fixed
    mutate(first_name = gsub("Brain", replacement = "Brian", first_name)) %>% #fixed
    
    mutate(first_name = gsub("Duncan John.*", replacement = "John", first_name)) %>% #fixed
    mutate(first_name = gsub("Johnson Henry.*", replacement = "Henry", first_name)) #fixed
  
  data$first_name %<>% trimws()
  data$first_name <- gsub("(^ |^  |^   |\n)", "", data$first_name)
  
  return(data$first_name)
  
}









###########################################################################################################
# This function will extract names found in the members dataset
# typical call:   
# data %<>% extractMemberName(members, 'FROM') 
# NOTE: A VAR NAMED "members" IN DATA CAN CAUSE PROBLEMS AND A VAR NAMED SUMMARY WILL BE OVERWRITTEN 

extractMemberName <- function(data, members, col_name){
  
  data %<>% mutate(Summary = data[[col_name]])

  # clean up text
  data$Summary %<>% cleanFROMcolumn()
  
  
  # correct common OCR errors
  data$Summary <- ocr.errors(data$Summary)
  
  
  data %<>% 
    # find common typos
    mutate(typos = Summary %>% map_chr(findTypos)) %>% 
    # add in corrections
    left_join(typos) %>%
    # replace typos with corrections
    mutate(Summary = str_replace_all(Summary, regex(typos, ignore_case = T), correct))
  
  
  #####################
  # Match names in different formats
  ###################

  # create FROM2 varible extracting name from data$Summary
  
  # WARNING THIS OVERWRITES WITH THE MOST RECENT NAME MATCHED
  
  # extract common_last name formats
  data$FROM2 <- gsub(pattern = paste(c('.*(', paste(members$common_last[1:850], collapse = '|'), ').*'), collapse = ""),
                    replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # extracts  first_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # first_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # first_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
  gsub(pattern = paste(c('.*(', paste(members$common_initial_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
       replacement = "\\1", data$Summary, ignore.case = TRUE) 
  
  
  
  
  
  
  
  # Assume first name is first word and last name appears last? 
  # THIS SEEMS LIKE A BAD ASSSUMPTION 
  data$first_name <- gsub("^(\\w+) .*", replacement = "\\1", data$FROM2)
  data$last_name <- gsub(".* (\\w+)$", replacement = '\\1', data$FROM2)
  
  
  # apply formating functions from above
  data$first_name <- formatFirstName(data, 'first_name')
  data$last_name <- formatLastName(data, 'last_name')
  
  # keep if first name matches a first name in the members data, otherwise, NA
  data %<>%
    mutate(first_name = ifelse(   grepl(paste(members$first_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE),
                                  first_name, NA) ) 
  
  # keep if last name matches a last name in the members file, otherwise, NA
  data %<>% 
    mutate(last_name = ifelse(
      grepl(paste(members$first_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE), 
      last_name, NA)) 
  
  # if both first name and last name are NA, FROM2 is NA 
data %>% 
    mutate(FROM2 = ifelse( is.na(first_name) & is.na(last_name), NA, FROM2))
  
  # corrections to first and last names 
 data %<>%
   mutate(last_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "Catherine", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Luj.n", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Luj.n", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Luj.n", Summary,ignore.case = TRUE)&grepl("Ben", Summary,ignore.case=TRUE), "LUJAN", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Luj.n", Summary,ignore.case = TRUE)&grepl("Ben", Summary,ignore.case=TRUE), "Ben", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Deb", Summary,ignore.case = TRUE)&grepl("Wasserman|Schultz", Summary,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Deb",Summary,ignore.case=TRUE)&grepl("Wasserman|Schultz",Summary,ignore.case=TRUE), "Debbie", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Mario|Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Mario", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Mario", first_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Lincoln", first_name)) #fixed
 
 # Fix specific common errors
 data %<>%
   mutate(last_name = ifelse(grepl("HERSETH", Summary, ignore.case = TRUE )|grepl('SANDLIN', Summary, ignore.case = TRUE), "HERSETH SANDLIN", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("HERSETH", Summary,ignore.case = TRUE)|grepl('SANDLIN', Summary,ignore.case = TRUE), "Stephanie", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("PAULSEN", Summary, ignore.case = TRUE )&grepl('Erik', Summary, ignore.case = TRUE), "PAULSEN", last_name)) %>% #no error
   mutate(first_name = ifelse(grepl("PAULSEN", Summary,ignore.case = TRUE)&grepl('Erik', Summary,ignore.case = TRUE), "Erik", first_name)) %>% #no error 
   mutate(last_name = ifelse(grepl("CONYERS", Summary, ignore.case = TRUE )&grepl('John', Summary, ignore.case = TRUE), "CONYERS", last_name)) %>% #no error
   mutate(first_name = ifelse(grepl("CONYERS", Summary,ignore.case = TRUE)&grepl('John', Summary,ignore.case = TRUE), "John", first_name)) %>% #no error
   mutate(last_name = ifelse(grepl("Ben|E.B|E B", Summary,ignore.case = TRUE)& grepl('NELSON', Summary,ignore.case = TRUE), "NELSON", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Ben|E.B|E B", Summary,ignore.case = TRUE)& grepl('NELSON', Summary,ignore.case = TRUE), "Ben", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Casey", Summary,ignore.case = TRUE)& grepl('Rob|Bob|Jr', Summary,ignore.case = TRUE), "CASEY", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Casey", Summary,ignore.case = TRUE)& grepl('Rob|Bob|Jr', Summary,ignore.case = TRUE), "Robert", first_name)) %>% 
   mutate(last_name = ifelse(grepl("RUPPERSBERGER", Summary,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% 
   mutate(first_name = ifelse(grepl("RUPPERSBERGER", Summary,ignore.case = TRUE), "Dutch", first_name)) %>% 
   mutate(last_name = ifelse(grepl("KRATOVIL", Summary,ignore.case = TRUE), "KRATOVIL", last_name)) %>% #no error
   mutate(first_name = ifelse(grepl("KRATOVIL", Summary,ignore.case = TRUE), "Frank", first_name)) %>% #no error
   mutate(last_name = ifelse(grepl("Sheila", Summary,ignore.case = TRUE)&grepl("JACKSON|Lee", Summary,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Sheila", Summary,ignore.case = TRUE)&grepl("JACKSONLee", Summary,ignore.case = TRUE), "Sheila", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Paul", Summary,ignore.case = TRUE)&grepl("KIRK", Summary,ignore.case = TRUE), "KIRK", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Paul", Summary,ignore.case = TRUE)&grepl("KIRK", Summary,ignore.case = TRUE), "Paul", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE), "BARRETT", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE)&grepl("James|Barret", Summary,ignore.case = TRUE), "James", first_name)) %>% 
   mutate(last_name = ifelse(grepl("John", Summary,ignore.case = TRUE)&grepl("Hall", Summary,ignore.case = TRUE), "HALL", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("John", Summary,ignore.case = TRUE)&grepl("Hall", Summary,ignore.case = TRUE), "John", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("ENBRENNER", Summary,ignore.case = TRUE), "SENSENBRENNER", last_name)) %>% 
   mutate(first_name = ifelse(grepl("ENBRENNER", Summary,ignore.case = TRUE), "Frank", first_name)) %>% 
   mutate(last_name = ifelse(grepl("FRELINGHUY", Summary,ignore.case = TRUE), "FRELINGHUYSEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("FRELINGHUY", Summary,ignore.case = TRUE), "Rodney", first_name)) %>% 
   mutate(last_name = ifelse(grepl("LARRICK", Summary,ignore.case = TRUE), "LARSEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("LARRICK", Summary,ignore.case = TRUE), "Rick", first_name)) %>% 
   mutate(last_name = ifelse(grepl("BOUSTANY", Summary,ignore.case = TRUE), "BOUSTANY", last_name)) %>% 
   mutate(first_name = ifelse(grepl("BOUSTANY", Summary,ignore.case = TRUE), "Charles", first_name)) %>% 
   mutate(last_name = ifelse(grepl("HINOJOSA", Summary,ignore.case = TRUE), "HINOJOSA", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("HINOJOSA", Summary,ignore.case = TRUE), "Ruben", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Payne", Summary,ignore.case = TRUE)&grepl("Don", Summary,ignore.case = TRUE), "PAYNE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Payne", Summary,ignore.case = TRUE)&grepl("Don", Summary,ignore.case = TRUE), "Donald", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Pascrell", Summary,ignore.case = TRUE)&grepl("Bill", Summary,ignore.case = TRUE), "PASCRELL", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Pascrell", Summary,ignore.case = TRUE)&grepl("Bill", Summary,ignore.case = TRUE), "Bill", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Tony", Summary,ignore.case = TRUE)&grepl("C.rdenas", Summary,ignore.case = TRUE), "CARDENAS", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Tony", Summary,ignore.case = TRUE)&grepl("C.rdenas", Summary,ignore.case = TRUE), "Tony", first_name)) %>%  
   mutate(last_name = ifelse(grepl("Kay", Summary,ignore.case = TRUE)&grepl("Hutch", Summary,ignore.case = TRUE), "HUTCHISON", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Kay", Summary,ignore.case = TRUE)&grepl("Hutch", Summary,ignore.case = TRUE), "Kay", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Mack|Mary", Summary,ignore.case = TRUE)&grepl("Bono", Summary,ignore.case = TRUE), "BONO", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Mack|mary", Summary,ignore.case = TRUE)&grepl("Bono", Summary,ignore.case = TRUE), "Mary", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Arlen", Summary,ignore.case = TRUE)&grepl("Spec", Summary,ignore.case = TRUE), "SPECTER", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Arlen", Summary,ignore.case = TRUE)&grepl("Spec", Summary,ignore.case = TRUE), "Arlen", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Chris", Summary,ignore.case = TRUE)&grepl("Van", Summary,ignore.case = TRUE)&grepl("Hollen", Summary,ignore.case = TRUE), "VAN HOLLEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Chris", Summary,ignore.case = TRUE)&grepl("Van", Summary,ignore.case = TRUE)&grepl("Hollen", Summary,ignore.case = TRUE), "Chris", first_name))  %>% 
   mutate(last_name = ifelse(grepl("Bonnie", Summary,ignore.case = TRUE)&grepl("Coleman", Summary,ignore.case = TRUE), "WATSON COLEMAN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Bonnie", Summary,ignore.case = TRUE)&grepl("Coleman", Summary,ignore.case = TRUE), "Bonnie", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Beto", Summary,ignore.case = TRUE)&grepl("ROURKE", Summary,ignore.case = TRUE), "O'ROURKE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Beto", Summary,ignore.case = TRUE)&grepl("ROURKE", Summary,ignore.case = TRUE), "Beto", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Gloria", Summary,ignore.case = TRUE)&grepl("McLeod|Negrete", Summary,ignore.case = TRUE), "NEGRETE McLEOD", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Gloria", Summary,ignore.case = TRUE)&grepl("McLeod", Summary,ignore.case = TRUE), "Gloria", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Frank", Summary,ignore.case = TRUE)&grepl("Pallone", Summary,ignore.case = TRUE), "PALLONE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Frank", Summary,ignore.case = TRUE)&grepl("Pallone", Summary,ignore.case = TRUE), "Frank", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Sanford", Summary,ignore.case = TRUE)&grepl("Bishop", Summary,ignore.case = TRUE), "BISHOP", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Sanford", Summary,ignore.case = TRUE)&grepl("Bishop", Summary,ignore.case = TRUE), "Sanford", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "Catherine", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Luj.n|Michelle", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Luj.n|Michelle", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Donovan|Donavan", Summary,ignore.case = TRUE)&grepl("Dan", Summary,ignore.case=FALSE), "DONOVAN", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Donovan|Donavan", Summary,ignore.case = TRUE)&grepl("Dan", Summary,ignore.case=FALSE), "Daniel", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Randy| j.|james|j ", Summary,ignore.case = TRUE)&grepl("Forbes", Summary,ignore.case=TRUE), "FORBES", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Randy| j.|james|j ", Summary,ignore.case = TRUE)&grepl("Forbes", Summary,ignore.case=TRUE), "James", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Mario|Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Mario", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Mario", first_name)) %>% #fixed
   mutate(first_name = ifelse(grepl("Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Lincoln", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE)&grepl("Bar", Summary,ignore.case=TRUE), "BARRETT", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Gresham",Summary,ignore.case=TRUE)&grepl("Barret",Summary,ignore.case=TRUE), "James", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Deb", Summary,ignore.case = TRUE)&grepl("Wasserman|Schultz", Summary,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Deb",Summary,ignore.case=TRUE)&grepl("Wasserman|Schultz",Summary,ignore.case=TRUE), "Debbie", first_name)) %>% #fixed
   mutate(last_name = ifelse(grepl("Kristen|Kirsten", Summary,ignore.case = TRUE)&grepl("Gil", Summary,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Kristen|Kirsten",Summary,ignore.case=TRUE)&grepl("Gil",Summary,ignore.case=TRUE), "Kirsten", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Jo ",Summary,ignore.case=TRUE)&grepl("Davis",Summary,ignore.case=TRUE), "DAVIS", last_name)) %>% #no error
   mutate(first_name = ifelse( grepl("Jo ",Summary,ignore.case=TRUE)&grepl("Davis",Summary,ignore.case=TRUE), "Jo", first_name)) %>% #no error
   mutate(last_name = ifelse( grepl("Waite|Brown",Summary,ignore.case=TRUE)&grepl("Ginny|Virginia",Summary,ignore.case=TRUE), "BROWN-WAITE", last_name)) %>% #no error
   mutate(first_name = ifelse( grepl("Waite|Brown",Summary,ignore.case=TRUE)&grepl("Ginny|Virginia",Summary,ignore.case=TRUE), "Virginia", first_name)) %>% #no error
   mutate(last_name = ifelse( grepl("Jo",Summary,ignore.case=TRUE)&grepl("Emerson",Summary,ignore.case=TRUE), "EMERSON", last_name)) %>% #no error
   mutate(first_name = ifelse( grepl("Jo",Summary,ignore.case=TRUE)&grepl("Emerson",Summary,ignore.case=TRUE), "Jo", first_name)) %>% #no error
   mutate(last_name = ifelse( grepl("Shelley|Moore",Summary,ignore.case=TRUE)&grepl("Capito",Summary,ignore.case=TRUE), "CAPITO", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Shelley|Moore",Summary,ignore.case=TRUE)&grepl("Capito",Summary,ignore.case=TRUE), "Shelley", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("McMorris|Rodgers",Summary,ignore.case=TRUE)&grepl("Cathy|McMorris",Summary,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("McMorris|Rodgers",Summary,ignore.case=TRUE)&grepl("Cathy|McMorris",Summary,ignore.case=TRUE), "Cathy", first_name)) %>% #fixed

   mutate(last_name = ifelse( grepl("Rounds",Summary,ignore.case=TRUE)&grepl("Marion|Mike|Michael",Summary,ignore.case=TRUE), "ROUNDS", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Rounds",Summary,ignore.case=TRUE)&grepl("Marion|Mike|Michael",Summary,ignore.case=TRUE), "Marion", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("Panetta",Summary,ignore.case=TRUE)&grepl("Jim|James",Summary,ignore.case=TRUE), "PANETTA", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Panetta",Summary,ignore.case=TRUE)&grepl("Jim|James",Summary,ignore.case=TRUE), "James", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("Roybal",Summary,ignore.case=TRUE)&grepl("Allard",Summary,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Roybal",Summary,ignore.case=TRUE)&grepl("Allard",Summary,ignore.case=TRUE), "Lucille", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("Clay",Summary,ignore.case=TRUE)&grepl("Lacy|William|Bill",Summary,ignore.case=TRUE), "CLAY", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Clay",Summary,ignore.case=TRUE)&grepl("Lacy|William|Bill",Summary,ignore.case=TRUE), "William", first_name))%>% #fixed
   #mutate(last_name = ifelse( grepl("Eleanor",Summary,ignore.case=TRUE)&grepl("Holmes|Norton",Summary,ignore.case=TRUE), "NORTON", last_name)) %>% 
   #mutate(first_name = ifelse( grepl("Eleanor",Summary,ignore.case=TRUE)&grepl("Holmes|Norton",Summary,ignore.case=TRUE), "Eleanor", first_name))%>% 
   #mutate(last_name = ifelse( grepl("Gregorio",Summary,ignore.case=TRUE)&grepl("Sablan",Summary,ignore.case=TRUE), "SABLAN", last_name)) %>% 
   #mutate(first_name = ifelse( grepl("Gregorio",Summary,ignore.case=TRUE)&grepl("Sablan",Summary,ignore.case=TRUE), "Gregorio", first_name))%>% 
   mutate(last_name = ifelse( grepl("Shea",Summary,ignore.case=TRUE)&grepl("Porter",Summary,ignore.case=TRUE), "SHEA-PORTER", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Shea",Summary,ignore.case=TRUE)&grepl("Porter",Summary,ignore.case=TRUE), "Carol", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("Jane",Summary,ignore.case=TRUE)&grepl("Harmon",Summary,ignore.case=TRUE), "HARMAN", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Jane",Summary,ignore.case=TRUE)&grepl("Harmon",Summary,ignore.case=TRUE), "Jane", first_name))%>% #fixed
   mutate(last_name = ifelse( grepl("Butterfield",Summary,ignore.case=TRUE)&grepl("G",Summary,ignore.case=TRUE), "BUTTERFIELD", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Butterfied",Summary,ignore.case=TRUE)&grepl("G",Summary,ignore.case=TRUE), "George", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Jon",Summary,ignore.case=TRUE)&grepl("Kyi",Summary,ignore.case=TRUE), "KYL", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Jon",Summary,ignore.case=TRUE)&grepl("Kyi",Summary,ignore.case=TRUE), "Jon", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Lou",Summary,ignore.case=TRUE)&grepl("Gohmert",Summary,ignore.case=TRUE), "GOHMERT", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Lou",Summary,ignore.case=TRUE)&grepl("Gohmert",Summary,ignore.case=TRUE), "Louie", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Jaime|Jamie|Jaimie",Summary,ignore.case=TRUE)&grepl("Herrera|Beutler",Summary,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Jaime|Jamie|Jaimie",Summary,ignore.case=TRUE)&grepl("Herrera|Beutler",Summary,ignore.case=TRUE), "Jaime", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Dian",Summary,ignore.case=TRUE)&grepl("Feinstein|Feinstien|Fenstein",Summary,ignore.case=TRUE), "FEINSTEIN", last_name)) %>% #fixed
   #mutate(first_name = ifelse( grepl("Dian",Summary,ignore.case=TRUE)&grepl("Herrera|Beutler",Summary,ignore.case=TRUE), "Dianne", first_name)) %>% #incorrect name
   mutate(last_name = ifelse( grepl("Issa",Summary,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",Summary,ignore.case=TRUE), "ISSA", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Issa",Summary,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",Summary,ignore.case=TRUE), "Darrell", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("Whitfield|Whitefield",Summary,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",Summary,ignore.case=TRUE), "WHITFIELD", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Whitfield|Whitefield",Summary,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",Summary,ignore.case=TRUE), "Wayne", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("(^| )Dana( |$)",Summary,ignore.case=TRUE)&grepl("(^| )RO.*HER( |$)",Summary,ignore.case=TRUE), "ROHRABACHER", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )Dana( |$)",Summary,ignore.case=TRUE)&grepl("(^| )RO.*HER( |$)",Summary,ignore.case=TRUE), "Dana", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("(^| )Womack( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )St",Summary,ignore.case=TRUE), "WOMACK", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )Womack( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )St",Summary,ignore.case=TRUE), "Steve", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("(^| )Mary( |$)",Summary,ignore.case=TRUE)&grepl("Mack|Bono",Summary,ignore.case=TRUE), "BONO", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )Mary( |$)",Summary,ignore.case=TRUE)&grepl("Mack|Bono",Summary,ignore.case=TRUE), "Mary", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",Summary,ignore.case=TRUE)&grepl("Morgan| H | H\\.",Summary,ignore.case=TRUE), "GRIFFITH", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",Summary,ignore.case=TRUE)&grepl("Morgan| H | H\\.",Summary,ignore.case=TRUE), "Morgan", first_name)) %>% #fixed
   mutate(last_name = ifelse( grepl("(^| )Lindsay( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )Graham",Summary,ignore.case=TRUE), "GRAHAM", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )Lindsay( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )Graham",Summary,ignore.case=TRUE), "Lindsey", first_name)) %>% #fixed
   
   mutate(last_name = ifelse( grepl("(^| )Conaway( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )Mi.",Summary,ignore.case=TRUE), "CONAWAY", last_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("(^| )Conaway( |$|,)",Summary,ignore.case=TRUE)&grepl("(^| )Mi.",Summary,ignore.case=TRUE), "Michael", first_name)) %>% #fixed
   mutate(first_name = ifelse( grepl("Anna A. ",Summary,ignore.case=TRUE),"Anna", first_name)) #fixed
 
  # i<- which(members$last_name == "GRAMM")[1] 
    ## clean up in future
  
  for (i in 1:length(members$id)) {
   data %<>% mutate(first_name = ifelse( !is.na(members$common_name[i]) & data$first_name == members$common_name[i] & data$last_name == members$last_name[i],
                                         members$first_name[i], data$first_name))
   
    }
  
 return(data)
 
 
}

# Function may need small add ons or adjustments for new/different datasets
# Function will take comma separated names (e.g. Johnson, Ralph) from a specified column (usually FROM) 
# and create first_name and last_name columns in the dataframe. Typical call: getFirstLast.Comma(data,'FROM')

# THIS FUNCTION IS NOT PART OF extractMemberNames, so these corrections will not help that function all corrections should appear in the same place and be called by each method
# this function should be able to take in different members (or we rewrite to be cogress-specific and use the full members list)
getFirstLast.Comma <- function(data, col_name){
  
  data$FROM <- data[[col_name]]

  # create duplicate FROM column and preprocess
  #data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, Jr..|, III| III| II|, II| ll| IV|VI", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, MD,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$FROM2)
  data$FROM2 <- gsub(pattern = "Member, U.S", "U.S", data$FROM2)
  data$FROM2 <- gsub(pattern= "\\.\\.", replacement = ".", data$FROM2)
  data$FROM2 <- gsub("(REP|SEN)(\\.|- | - |\\. )", "", data$FROM2)
  data$FROM2 <- gsub("(^S(-| ))|Senator|Sen\\.", "", data$FROM2)
  data$FROM2 <- gsub("(^(R|C)(-| ))|Repres|Congress|Rep", "", data$FROM2)
  data$FROM2 <- gsub("  |   |    ", " ", data$FROM2)
  data$FROM2 <- gsub("  |   |    ", " ", data$FROM2)
  data$FROM2 <- gsub("(^ |^  |^   |\n)", "", data$FROM2)
  
  
  data$FROM2 %<>% ocr.errors()
  
  
  #create variable for last name of the Sen/Rep
  data %<>%
    mutate(last = gsub(pattern = "^(\\w+|\\w+ \\w+|\\w+-\\w+)( ,|,).*", 
                       replacement = "\\1", x=FROM2)) %>% 
    mutate(last = gsub(pattern= "^(\\w')(\\w+)-(\\w+)( ,|,).*", replacement = "\\1\\2-\\3", last)) %>% 
    mutate(last = gsub(pattern= "^(\\w')(\\w+)( ,|,).*", replacement = "\\1\\2", last))
 
   # create variable for first name of Sen/Rep
  data %<>%
    mutate(first = gsub(pattern = ".*?(,|, |,  |,\\w |,\\w. |,, \\w |, \\w. |, \\w.|, \\w+|,\\w+)(\\w+)( |.|).*",
                        replacement = "\\1\\2", x=FROM2)) %>% 
    mutate(first = gsub("(,|, )", "", first))
  
  
  # format first and last variables to the same convention as the members dataset
  data$last <- formatLastName(data, 'last')
  data$first <- formatFirstName(data, 'first')
  
  # create a variable for their full name by combining first and last
  data %<>%
    mutate(first_last = paste(first, last, sep = " "))
  
  # if their full name is in the members dataset, assign 'first' to new variable 'first_name'. Otherwise  assign NA
  data %<>%
    mutate(first_name = ifelse(  
      grepl(paste(members$first_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE),
      
      first, NA) ) %>% 
    
    # if their full name is in the members dataset, assign 'last' to new variable 'last_name'. Otherwise assign NA
    mutate(last_name = ifelse(
      grepl(paste(members$first_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$first_last, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$first_last, ignore.case = TRUE), 
      last, NA)) 
  
  
  
  
  
  data$first_name <- formatFirstName(data, 'first_name')
  data$last_name <- formatLastName(data, 'last_name')
  
  # Fix specific common errors
  data %<>%
    mutate(last_name = ifelse(grepl("HERSETH", FROM2, ignore.case = TRUE )|grepl('SANDLIN', FROM2, ignore.case = TRUE), "HERSETH SANDLIN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("HERSETH", FROM2,ignore.case = TRUE)|grepl('SANDLIN', FROM2,ignore.case = TRUE), "Stephanie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("PAULSEN", FROM2, ignore.case = TRUE )&grepl('Erik', FROM2, ignore.case = TRUE), "PAULSEN", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("PAULSEN", FROM2,ignore.case = TRUE)&grepl('Erik', FROM2,ignore.case = TRUE), "Erik", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("CONYERS", FROM2, ignore.case = TRUE )&grepl('John', FROM2, ignore.case = TRUE), "CONYERS", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("CONYERS", FROM2,ignore.case = TRUE)&grepl('John', FROM2,ignore.case = TRUE), "John", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Ben|E.B|E B", FROM2,ignore.case = TRUE)& grepl('NELSON', FROM2,ignore.case = TRUE), "NELSON", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Ben|E.B|E B", first_last,ignore.case = TRUE)& grepl('NELSON', first_last,ignore.case = TRUE), "Ben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "CASEY", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "Robert", first_name)) %>% 
    mutate(last_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% 
    mutate(first_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "Dutch", first_name)) %>% 
    mutate(last_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "KRATOVIL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON|Lee", FROM2,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON|Lee", FROM2,ignore.case = TRUE), "Sheila", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Paul", FROM2,ignore.case = TRUE)&grepl("KIRK", FROM2,ignore.case = TRUE), "KIRK", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Paul", FROM2,ignore.case = TRUE)&grepl("KIRK", FROM2,ignore.case = TRUE), "Paul", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE), "GRESHAM", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE)&grepl("James", FROM2,ignore.case = TRUE), "Paul", first_name)) %>% 
    mutate(last_name = ifelse(grepl("John", FROM2,ignore.case = TRUE)&grepl("Hall", FROM2,ignore.case = TRUE), "HALL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("John", FROM2,ignore.case = TRUE)&grepl("Hall", FROM2,ignore.case = TRUE), "John", first_name)) %>% 
    mutate(last_name = ifelse(grepl("ENBRENNER", FROM2,ignore.case = TRUE), "SENSENBRENNER", last_name)) %>% 
    mutate(first_name = ifelse(grepl("ENBRENNER", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% 
    mutate(last_name = ifelse(grepl("FRELINGHUY", FROM2,ignore.case = TRUE), "FRELINGHUYSEN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("FRELINGHUY", FROM2,ignore.case = TRUE), "Rodney", first_name)) %>% 
    mutate(last_name = ifelse(grepl("LARRICK", FROM2,ignore.case = TRUE), "LARSEN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("LARRICK", FROM2,ignore.case = TRUE), "Rick", first_name)) %>% 
    mutate(last_name = ifelse(grepl("BOUSTANY", FROM2,ignore.case = TRUE), "BOUSTANY", last_name)) %>% 
    mutate(first_name = ifelse(grepl("BOUSTANY", FROM2,ignore.case = TRUE), "Charles", first_name)) %>% 
    mutate(last_name = ifelse(grepl("King", FROM2,ignore.case = TRUE)&grepl("Jr", FROM2,ignore.case = TRUE), "KING", last_name)) %>% 
    mutate(first_name = ifelse(grepl("King", FROM2,ignore.case = TRUE)&grepl("jr", FROM2,ignore.case = TRUE), "Angus", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Andr", FROM2,ignore.case = TRUE)&grepl("Carson", FROM2,ignore.case = TRUE), "CARSON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Andr", FROM2,ignore.case = TRUE)&grepl("Carson", FROM2,ignore.case = TRUE), "Andre", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE),"SERRANO", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE), "Jose", first_name)) %>% 
    mutate(last_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "HINOJOSA", last_name)) %>% 
    mutate(first_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "Ruben", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "PAYNE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "Donald", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "PASCRELL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "Bill", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Kay", FROM2,ignore.case = TRUE)&grepl("Hutch", FROM2,ignore.case = TRUE), "HUTCHISON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Kay", FROM2,ignore.case = TRUE)&grepl("Hutch", FROM2,ignore.case = TRUE), "Kay", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Mack|Mary", FROM2,ignore.case = TRUE)&grepl("Bono", FROM2,ignore.case = TRUE), "BONO", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Mack|Mary", FROM2,ignore.case = TRUE)&grepl("Bono", FROM2,ignore.case = TRUE), "Mary", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Arlen", FROM2,ignore.case = TRUE)&grepl("Spec", FROM2,ignore.case = TRUE), "SPECTER", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Arlen", FROM2,ignore.case = TRUE)&grepl("Spec", FROM2,ignore.case = TRUE), "Arlen", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Chris", FROM2,ignore.case = TRUE)&grepl("Van", FROM2,ignore.case = TRUE)&grepl("Hollen", FROM2,ignore.case = TRUE), "VAN HOLLEN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Chris", FROM2,ignore.case = TRUE)&grepl("Van", FROM2,ignore.case = TRUE)&grepl("Hollen", FROM2,ignore.case = TRUE), "Chris", first_name))  %>% 
    mutate(last_name = ifelse(grepl("Bonnie", FROM2,ignore.case = TRUE)&grepl("Coleman", FROM2,ignore.case = TRUE), "WATSON COLEMAN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Bonnie", FROM2,ignore.case = TRUE)&grepl("Coleman", FROM2,ignore.case = TRUE), "Bonnie", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Beto", FROM2,ignore.case = TRUE)&grepl("ROURKE", FROM2,ignore.case = TRUE), "O'ROURKE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Beto", FROM2,ignore.case = TRUE)&grepl("ROURKE", FROM2,ignore.case = TRUE), "Beto", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Gloria", FROM2,ignore.case = TRUE)&grepl("McLeod|Negrete", FROM2,ignore.case = TRUE), "NEGRETE McLEOD", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Gloria", FROM2,ignore.case = TRUE)&grepl("McLeod", FROM2,ignore.case = TRUE), "Gloria", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Frank", FROM2,ignore.case = TRUE)&grepl("Pallone", FROM2,ignore.case = TRUE), "PALLONE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Frank", FROM2,ignore.case = TRUE)&grepl("Pallone", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Sanford", FROM2,ignore.case = TRUE)&grepl("Bishop", FROM2,ignore.case = TRUE), "BISHOP", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Sanford", FROM2,ignore.case = TRUE)&grepl("Bishop", FROM2,ignore.case = TRUE), "Sanford", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Matso|Masto", FROM2,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Matso|Masto", FROM2,ignore.case = TRUE), "Catherine", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Luj.n|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n", FROM2,ignore.case = TRUE)&grepl("Ben", FROM2,ignore.case=TRUE), "LUJAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n", FROM2,ignore.case = TRUE)&grepl("Ben", FROM2,ignore.case=TRUE), "Ben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Donovan|Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "DONOVAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Donovan|Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "Daniel", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Randy| j.|james|j ", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "FORBES", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Randy| j.|james|j ", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "James", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Mario|Lincoln", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Mario", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "Mario", first_name)) %>% 
    mutate(first_name = ifelse(grepl("Lincoln", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "Lincoln", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE)&grepl("Barret", FROM2,ignore.case=TRUE), "BARRETT", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Gresham",FROM2,ignore.case=TRUE)&grepl("Barret",FROM2,ignore.case=TRUE), "James", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Deb", FROM2,ignore.case = TRUE)&grepl("Wasserman|Schultz", FROM2,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Deb",FROM2,ignore.case=TRUE)&grepl("Wasserman|Schultz",FROM2,ignore.case=TRUE), "Debbie", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Kristen|Kirsten", FROM2,ignore.case = TRUE)&grepl("Gil", FROM2,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Kristen|Kirsten",FROM2,ignore.case=TRUE)&grepl("Gil",FROM2,ignore.case=TRUE), "Kirsten", first_name)) %>% 
    mutate(last_name = ifelse( grepl("Jo ",FROM2,ignore.case=TRUE)&grepl("Davis",FROM2,ignore.case=TRUE), "DAVIS", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Jo ",FROM2,ignore.case=TRUE)&grepl("Davis",FROM2,ignore.case=TRUE), "Jo", first_name)) %>% 
    mutate(last_name = ifelse( grepl("Waite|Brown",FROM2,ignore.case=TRUE)&grepl("Ginny|Virginia",FROM2,ignore.case=TRUE), "BROWN-WAITE", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Waite|Brown",FROM2,ignore.case=TRUE)&grepl("Ginny|Virginia",FROM2,ignore.case=TRUE), "Virginia", first_name)) %>% 
    mutate(last_name = ifelse( grepl("Jo",FROM2,ignore.case=TRUE)&grepl("Emerson",FROM2,ignore.case=TRUE), "EMERSON", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Jo",FROM2,ignore.case=TRUE)&grepl("Emerson",FROM2,ignore.case=TRUE), "Jo", first_name)) %>% 
    mutate(last_name = ifelse( grepl("Shelley|Moore",FROM2,ignore.case=TRUE)&grepl("Capito",FROM2,ignore.case=TRUE), "CAPITO", last_name)) %>% #fixed

    mutate(last_name = ifelse( grepl("McMorris|Rodgers",FROM2,ignore.case=TRUE)&grepl("Cathy|McMorris",FROM2,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("McMorris|Rodgers",FROM2,ignore.case=TRUE)&grepl("Cathy|McMorris",FROM2,ignore.case=TRUE), "Cathy", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Rounds",FROM2,ignore.case=TRUE)&grepl("Marion|Mike|Michael",FROM2,ignore.case=TRUE), "ROUNDS", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Rounds",FROM2,ignore.case=TRUE)&grepl("Marion|Mike|Michael",FROM2,ignore.case=TRUE), "Marion", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Panetta",FROM2,ignore.case=TRUE)&grepl("Jim|James",FROM2,ignore.case=TRUE), "PANETTA", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Panetta",FROM2,ignore.case=TRUE)&grepl("Jim|James",FROM2,ignore.case=TRUE), "James", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Roybal",FROM2,ignore.case=TRUE)&grepl("Allard",FROM2,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Roybal",FROM2,ignore.case=TRUE)&grepl("Allard",FROM2,ignore.case=TRUE), "Lucille", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Clay",FROM2,ignore.case=TRUE)&grepl("Lacy|William|Bill",FROM2,ignore.case=TRUE), "CLAY", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Clay",FROM2,ignore.case=TRUE)&grepl("Lacy|William|Bill",FROM2,ignore.case=TRUE), "William", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Todd",FROM2,ignore.case=TRUE)&grepl("Akin",FROM2,ignore.case=TRUE), "AKIN", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Todd",FROM2,ignore.case=TRUE)&grepl("Akin",FROM2,ignore.case=TRUE), "Todd", first_name))%>% #fixed
    #mutate(last_name = ifelse( grepl("Eleanor",FROM2,ignore.case=TRUE)&grepl("Holmes|Norton",FROM2,ignore.case=TRUE), "NORTON", last_name)) %>% 
    #mutate(first_name = ifelse( grepl("Eleanor",FROM2,ignore.case=TRUE)&grepl("Holmes|Norton",FROM2,ignore.case=TRUE), "Eleanor", first_name))%>% 
    #mutate(last_name = ifelse( grepl("Gregorio",FROM2,ignore.case=TRUE)&grepl("Sablan",FROM2,ignore.case=TRUE), "SABLAN", last_name)) %>% 
    #mutate(first_name = ifelse( grepl("Gregorio",FROM2,ignore.case=TRUE)&grepl("Sablan",FROM2,ignore.case=TRUE), "Gregorio", first_name))%>% 
    mutate(last_name = ifelse( grepl("Shea",FROM2,ignore.case=TRUE)&grepl("Porter",FROM2,ignore.case=TRUE), "SHEA-PORTER", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Shea",FROM2,ignore.case=TRUE)&grepl("Porter",FROM2,ignore.case=TRUE), "Carol", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Jane",FROM2,ignore.case=TRUE)&grepl("Harmon",FROM2,ignore.case=TRUE), "HARMAN", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jane",FROM2,ignore.case=TRUE)&grepl("Harmon",FROM2,ignore.case=TRUE), "Jane", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Butterfield",FROM2,ignore.case=TRUE)&grepl("G",FROM2,ignore.case=TRUE), "BUTTERFIELD", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Butterfied",FROM2,ignore.case=TRUE)&grepl("G",FROM2,ignore.case=TRUE), "George", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Lou",FROM2,ignore.case=TRUE)&grepl("Gohmert",FROM2,ignore.case=TRUE), "GOHMERT", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Lou",FROM2,ignore.case=TRUE)&grepl("Gohmert",FROM2,ignore.case=TRUE), "Louie", first_name)) %>% #fixed
    
    mutate(last_name = ifelse( grepl("Jaime|Jamie|Jaimie",FROM2,ignore.case=TRUE)&grepl("Herrera|Beutler",FROM2,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jaime|Jamie|Jaimie",FROM2,ignore.case=TRUE)&grepl("Herrera|Beutler",FROM2,ignore.case=TRUE), "Jaime", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Dian",FROM2,ignore.case=TRUE)&grepl("Feinstein|Feinstien|Fenstein",FROM2,ignore.case=TRUE), "FEINSTEIN", last_name)) %>% #fixed
    #mutate(first_name = ifelse( grepl("Dian",FROM2,ignore.case=TRUE)&grepl("Herrera|Beutler",FROM2,ignore.case=TRUE), "Dianne", first_name)) %>% #this very much seems incorrect
    mutate(last_name = ifelse( grepl("Issa",FROM2,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",FROM2,ignore.case=TRUE), "ISSA", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Issa",FROM2,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",FROM2,ignore.case=TRUE), "Darrell", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Whitfield|Whitefield",FROM2,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",FROM2,ignore.case=TRUE), "WHITFIELD", last_name)) %>% #fixed 
    mutate(first_name = ifelse( grepl("Whitfield|Whitefield",FROM2,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",FROM2,ignore.case=TRUE), "Wayne", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Dana( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )RO.*HER( |,|$)",FROM2,ignore.case=TRUE), "ROHRABACHER", last_name)) %>% #fixed but there may be errors that haven't been caught
    mutate(first_name = ifelse( grepl("(^| )Dana( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )RO.*HER( |,|$)",FROM2,ignore.case=TRUE), "Dana", first_name)) %>% #fixed but there may be some errors still 
    mutate(last_name = ifelse( grepl("(^| )Womack( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )St",FROM2,ignore.case=TRUE), "WOMACK", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Womack( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )St",FROM2,ignore.case=TRUE), "Steve", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Mary( |,|$)",FROM2,ignore.case=TRUE)&grepl("Mack|Bono",FROM2,ignore.case=TRUE), "BONO", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Mary( |,|$)",FROM2,ignore.case=TRUE)&grepl("Mack|Bono",FROM2,ignore.case=TRUE), "Mary", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",FROM2,ignore.case=TRUE)&grepl("Morgan| H | H\\.",FROM2,ignore.case=TRUE), "GRIFFITH", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",FROM2,ignore.case=TRUE)&grepl("Morgan| H | H\\.",FROM2,ignore.case=TRUE), "Morgan", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Conaway( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )Mi.",FROM2,ignore.case=TRUE), "CONAWAY", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Conaway( |,|$)",FROM2,ignore.case=TRUE)&grepl("(^| )Mi.",FROM2,ignore.case=TRUE), "Michael", first_name)) #fixed
  

    
  for (i in 1:length(members$id)) {
    data %<>% mutate(first_name = ifelse( !is.na(members$common_name[i]) & data$first_name == members$common_name[i] & data$last_name == members$last_name[i],
                                          members$first_name[i], data$first_name))

  }
  
   #Remove colums. Comment out for debugging
 # data <- subset(data, select = -c(first, last, first_last, FROM2))
  
  #Drop first_last column
  data %<>%
    select(-first_last)
  
  return(data)
}


# Fixes common errors when names read in from OCR
# May need adjustments for different agencies/formats


ocr.errors <- function(FROM){
  
  # adds deleted "ll" to last names
  #CAN EDIT HERE
  FROM <- gsub("Hiary", "Hillary", FROM)
  FROM <- gsub("(^| )Dinge($| )", "\\1Dingell\\2", FROM)
  FROM <- gsub("Connoy", "Connolly", FROM)
  FROM <- gsub("(^| )Russe($| )", "\\1Russell\\2", FROM)
  FROM <- gsub("(^| )Swalwe($| )", "\\1Swalwell\\2", FROM)
  FROM <- gsub("Cheie", "Chellie", FROM)
  FROM <- gsub("(^| )Uda($| )", "\\1Udall\\2", FROM)
  FROM <- gsub("(^| )Wooda($| )", "\\1Woodall\\2", FROM)
 # FROM <- gsub("Marsha$", "Marshall", FROM)
  FROM <- gsub("Wiiam", "William", FROM)
  FROM <- gsub("(^| )Cantwe($| )", "\\1Cantwell\\2", FROM)
  FROM <- gsub("Mier$", "Miller", FROM)
  FROM <- gsub("Way$", "Wally", FROM)
  FROM <- gsub("Aen$", "Allen", FROM)
  FROM <- gsub("(^| )Bi($| )", "\\1Bill\\2", FROM)
  FROM <- gsub("Coins", "Collins", FROM)
  FROM <- gsub("Paone|Pal lone", "Pallone", FROM)
  FROM <- gsub("(^| )Campbe($| )", "\\1Campell\\2", FROM)
  FROM <- gsub("Hoen", "Hollen", FROM)
  FROM <- gsub("(^| )Darre($| )", "\\1Darrell\\2", FROM)
  FROM <- gsub("Gaegly", "Gallegly", FROM)
  FROM <- gsub("Giibrand", "Gillibrand", FROM)
  FROM <- gsub("(^| )McConne( |$)", "\\1McConnell\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Ayson", "\\1Allyson", FROM)
  FROM <- gsub("Keer($| |,)", "Keller\\1", FROM)
  FROM <- gsub("(^| )Een($| )", "\\1Ellen\\2", FROM)
  FROM <- gsub("Roybal-Aard", "Roybal-Allard", FROM)
  FROM <- gsub("Cuear($| |,)", "Cuellar\\1", FROM)
  FROM <- gsub("Pascre($| |,)", "Pascrell\\1", FROM)
  FROM <- gsub("Pa one", "Pallone", FROM)
  FROM <- gsub("Gabriee", "Gabrielle", FROM)
  FROM <- gsub("(^| )Aard", "\\1Allard", FROM)
  FROM <- gsub("McCoum", "McCollum", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Eison( |$)", "\\1Ellison\\2", FROM)
  FROM <- gsub("(^| )Weer( |$)", "\\1Weller\\2", FROM)
  FROM <- gsub("Rockefeer", "Rockefeller", FROM)
  FROM <- gsub("(^| )Suivan( |$)", "\\1Sullivan\\2", FROM)
  FROM <- gsub("(^| )Tiis( |$)", "\\1Tillis\\2", FROM)
  FROM <- gsub("(^| )McCaski( |$)", "\\1McCaskill\\2", FROM)
  FROM <- gsub("(^| )Espaiat( |$)", "\\1Espalliat\\2", FROM)
  FROM <- gsub("(^| )Costeo( |$)", "\\1Costello\\2", FROM)
  FROM <- gsub("(^| )Hi( |$)", "\\1Hill\\2", FROM)
  FROM <- gsub("(^| )Gaego( |$)", "\\1Gallego\\2", FROM)
  FROM <- gsub("(^| )McSay( |$)", "\\1McSally\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Muin( |$)", "\\1Mullin\\2", FROM)
  FROM <- gsub("(^| )Ciciin.( |$)", "\\1Cicilline\\2", FROM)
  FROM <- gsub("(^| )Sewe( |$)", "\\1Sewell\\2", FROM)
  FROM <- gsub("(^| )Heer( |$)", "\\1Heller\\2", FROM)
  FROM <- gsub("(^| )Rige( |$)", "\\1Rigell\\2", FROM)
  FROM <- gsub("(^| )Emers( |$)", "\\1Ellmers\\2", FROM)
  FROM <- gsub("(^| )Mier( |$)", "\\1Miller\\2", FROM)
  FROM <- gsub("(^| )Ha( |$)", "\\1Hall\\2", FROM)
  FROM <- gsub("(^| )McAISTER( |$)", "\\1McAllister\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Boswe( |$)", "\\1Boswell\\2", FROM)
  FROM <- gsub("(^| )Manzuo( |$)", "\\1Manzullo\\2", FROM)
  FROM <- gsub("(^| )Schiing( |$)", "\\1Shilling\\2", FROM)
  FROM <- gsub("(^| )Kisse( |$)", "\\1Kissell\\2", FROM)
  FROM <- gsub("(^| )Hoingsworth( |$)", "\\1Hollingsworth\\2", FROM)
  FROM <- gsub("(^| )Gaagher( |$)", "\\1Gallagher\\2", FROM)
  FROM <- gsub("(^| )Joy( |$)", "\\1Jolly\\2", FROM)
  FROM <- gsub("(^| )Raha( |$)", "\\1Rahall\\2", FROM)
  FROM <- gsub("(^| )Boswe( |$)", "\\1Boswell\\2", FROM)
  FROM <- gsub("(^| )Perrieo( |$)", "\\1Perriello\\2", FROM)
  FROM <- gsub("(^| )Fain( |$)", "\\1Fallin\\2", FROM)
  FROM <- gsub("(^| )Esworth( |$)", "\\1Ellsworth\\2", FROM)
  FROM <- gsub("(^| )Moohan( |$)", "\\1Mollohan\\2", FROM)
  FROM <- gsub("(^| )Fossea( |$)", "\\1Fossella\\2", FROM)
  FROM <- gsub("(^| )Miender-McDonald( |$)", "\\1Millender-McDonald\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Knoenberg( |$)", "\\1Knollenberg\\2", FROM)
  FROM <- gsub("(^| )Gimor( |$)", "\\1Gillmor\\2", FROM)
  FROM <- gsub("(^| )Jewe( |$)", "\\1Jewell\\2", FROM)
  # add 'll' to first names
  FROM <- gsub("(^| )oyd( |$)", "\\1Lloyd\\2", FROM)
  FROM <- gsub("(^| )Lucie( |$)", "\\1Lucille\\2", FROM)
  FROM <- gsub("(^| )Michee( |$)", "\\1Michelle\\2", FROM)
  FROM <- gsub("(^| )Bi( |$)", "\\1Bill\\2", FROM)
  FROM <- gsub("(^| )Biy( |$)", "\\1Billy\\2", FROM)
  FROM <- gsub("(^| )Coeen( |$)", "\\1Colleen\\2", FROM)
  FROM <- gsub("(^| )Cheie( |$)", "\\1Chellie\\2", FROM)
  FROM <- gsub("(^| )Sheey( |$)", "\\1Shelley\\2", FROM)
  FROM <- gsub("(^| )Darre( |$)", "\\1Darrell\\2", FROM)
  FROM <- gsub("(^| )Ayson( |$)", "\\1Allyson\\2", FROM)
  FROM <- gsub("(^| )Aen( |$)", "\\1JAllen\\2", FROM)
  FROM <- gsub("(^| )Say( |$)", "\\1Sally\\2", FROM)
  FROM <- gsub("(^| )Wi( |$)", "\\1Will\\2", FROM)
  FROM <- gsub("(^| )Way( |$)", "\\1Wally\\2", FROM)
  FROM <- gsub("(^| )Key( |$)", "\\1Kelly\\2", FROM)
  FROM <- gsub("([A-Z])(55)([A-Z])", "\\1SS\\2",FROM)
  FROM <- gsub("([A-Z])(5)([A-Z])", "\\1S\\2",FROM)
  FROM <- gsub("A1 ", "Al ", FROM)
  FROM <- gsub(" 1. ", " L. ", FROM)
  FROM <- gsub("Hany", "Harry", FROM) 
  
  
  # other errors
  #FIXED
  FROM <- gsub(".1.", "", FROM)
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("J.*?n","Jason", FROM), FROM) #not sure if we should make this correction
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("Ch.*?z","Chaffetz", FROM), FROM) #also not sure if we should make this correction
  FROM <- ifelse(grepl("Tom", FROM)&grepl("Cobum|Co bum", FROM), gsub("Cobum|Co bum", "Coburn", FROM), FROM) #fixed
  FROM <- ifelse(grepl("DarrellIssa", FROM), 'Darrell Issa', FROM) #fixed
  FROM <- ifelse(grepl("Trent|Robin|Mike", FROM)&grepl("Key", FROM), gsub("(Trent|Robin|Mike) Key", "\\1 Kelly",FROM), FROM) #this goes in OCR errors/last name errors
  FROM <- ifelse(grepl("Comyn|Com yn|Cobum|Corvyn", FROM)&grepl("John", FROM), gsub("Comyn|Com yn|Corvyn","Cornyn", FROM), FROM) #fixed
  FROM <- ifelse(grepl("Jon", FROM)&grepl("(^| )Kyi( |$)", FROM), gsub("Kyi","Kyl", FROM), FROM) #fixed
  FROM <- ifelse(grepl("Diane", FROM)&grepl("(^| )Feinstein( |$|,)", FROM), gsub("Diane","Dianne", FROM), FROM) #fixed
  
 # FROM <- ifelse(grepl("Cliff", FROM)&grepl("Steams", FROM), gsub("Steams","Stearns", FROM), FROM) #fixed
  
 #FIXED
  
  FROM <- gsub("Cwnmings", 'Cummings', FROM) # fixed
  FROM <- gsub("Tnhofe", "Inhofe", FROM) # fixed
  FROM <- gsub("Ellrners","Ellmers", FROM) # fixed 
  FROM <- gsub("TONKA", "TONKO", FROM) #fixed
  FROM <- gsub("Mcarthur|Mccarthur", "MacArthur", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Coryn( |$|,)", "\\1Cornyn\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Connelly( |$|,)", "\\1Connolly\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Heitkmap( |$|,)", "\\1Heitkamp\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Micahel( |$)", "\\1Michael\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Farenhold( |$|,)", "\\1Farenthold\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Eschoo( |$|,)", "\\1Eshoo\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Lary( |$)", "\\1Larry\\3", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("Christophers", "Christopher", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("Courntey", "Courtney", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Martrin( |$)", "\\1Martin\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Machin( |$|,)", "\\1Manchin\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Marry( |$|,)", "\\1Mary\\2", FROM, ignore.case = TRUE) # might not need this code because it is Harry not Marry
  FROM <- gsub("(^| )T MOTHY( |$|,)", "\\1Timothy\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )L NCOLN( |$|,)", "\\1Lincoln\\2", FROM, ignore.case = TRUE) #fixed 
  FROM <- gsub("(^| )Wydon( |$|,)", "\\1Wyden\\2", FROM, ignore.case = TRUE) #fixed
  FROM <- gsub("(^| )Klobachur( |$|,)", "\\Klobuchar\\2", FROM, ignore.case = TRUE) #fixed
  
  
  
  return(FROM)
}

# *** Use this function with caution. Will add first name information based on ONLY last names that are unique, has 
#     potential to add a members first name to a non member creating false positives (e.g. If only last name 
#     Grassley is provided, it will assume it's Chuck Grassley, even if it was actually a 
#     random person named Jim Joe Grassley)
#
#
#  Typical use of function looks as follows:
# data$first_name <- addFirst(data$first_name,data$last_name)

#     *** last_name paramter must be in same all caps format of the members file
#     This function call may be necessary:   data$last_name <- formatLastName(data, 'last_name')

# Useful code for creating last_name when only last name provided:
#data %<>%
#  mutate(last_name = ifelse(grepl("^(\\w+)$",FROM), gsub("^(\\w+)$", '\\1',FROM),last_name))
addFirst <- function(first_name, last_name){
  
  twolastnames  <- members %>% group_by(last_name, congress) %>% tally() %>% filter(n>1) %>% select(-congress, -n) %>% distinct()
  membersOneLastName <- members[!(members$last_name %in% twolastnames$last_name),]

  i <- 1
  for(i in 1:length(membersOneLastName$id)){
    first_name = ifelse(last_name == membersOneLastName$last_name[i] & is.na(first_name), membersOneLastName$first_name[i],first_name)
    
  }
  return(first_name)
}



#########################

# FREQUENT TYPOS WHERE WE CAN JUST REPLACE THEM REGARDLESS OF THE WORDS BEFORE AND AFTER (i.e. we are very confident that this is what they should be)
# THERE IS NOTHING ELSE IT COULD POSSIBLY BE
typos_clear <- tribble(
  ~correct, ~typos,
  "Cummings", "Cwnmings",
  "Inhofe", "Tnhofe",
  "Ellmers", "Ellrners",
  "TONKO", "TONKA",
  "Darrell Issa", "DarrellIssa",
  "Lujan", "Luj.n",
  "Phil ", "Phill ", # I added a space because it seems risky to match Phill...
  "LaMalfa", "LaMalfn",
  "Courtey", "Courntney",
  "Kirsten", "Kirstein",
  "Gerlach", "Gerlah",
  "Darlene", "Darene",
  "Rodham", "Redham",
  "Elizabeth", "Elezabeth",
  "Jeffrey", "Jeflrey",
  "Barbara", "(Babara|Barabara)",
  "Velazquez", "Vel.zquez",
  "Lincoln", "L.ncoln",
  "Timothy", "T.mothy",
  "MacArthur","(Mcarthur|Mccarthur)", # there is no McArthur in members file
  "Michael", "(Midlael|Michaell|Micahel)",
  "Martin","Martrin", 
  "Cardenas", "C.rdenas",
  "VISCLOSKY", ".isclosky",
  "Murphy", "Murhpy",
  "Sanchez", "S.nchez",
  "Gutierrez", "Gut.errez",
  "Melissa", "Melisssa",
  "Brian", "Brain",
  "Christopher", "Christoher",
  "Lujan", "Luj.n",
  "Raul", "R.ul",
  

  
  ###############################
  
  # Reversing order of first name and last name
  # Added the missing commas so they match the last, first pattern
  "John Duncan", "Duncan, John",
  "Henry Johnson", "Johnson, Henry",
  "Mary Bono", "Bono, Mary",
  "Nick Rahall", "Rahall, Nick",
  "Jackson Lee", "Lee, Jackson",
  "Michael Conaway", "Conaway, Michael",
  "Morgan Griffith", "Griffith, Morgan",
  "Steve Womack", "Womack, Steve",
  "Dana ROHRABACHER", "ROHRABACHER, Dana",
  "Ben Nelson", "Nelson, Ben"
  

)

# FREQUENT LAST NAME TYPOS WHERE WE ALSO WANT TO SEE THE FIRST NAME 
typos_last <- tribble(
  ~first_name, ~last_name, ~last_name_typos,
  "Patty", "Murray", "Muray",
  "Lois", "Capps", "Crapps",
  "Rick", "Boucher", "Bocuher",
  "Robert", "Andrews", "Andrew",
  "Rodney", "Frelinghuysen", "Frelinhuysen",
  "Shelly", "Berkley", "Barkley",
  "Steny", "Hoyer", "Royer",
  "Vito", "Fossella", "Fosella",
  "John", "Barrasso", "Barasso",
  "Larry", "Bucshon", "Bueston",
  "Matt", "Cartwright", "Cartwrite",
  "Rosa", "DeLauro", "De Lauro",
  "Mike", "DeWine", "De Wine",
  "Michael", "Bilirakis", "Bilikaris",
  "Jack", "Reed", "Red",
  "David", "Schweikert", "Schweikerl",
  "Peter", "DeFazio", "DiFazio",
  "Roy", "Blunt", "Blur",
  "Steve", "Scalise", "Scallise",
  "Russ", "Carnahan", "Camahan",
  "Zoe", "Lofgren", "Lufgren",
  "Thomas", "Holden", "Holen",
  "Olympia","Snowe", "Showe",
  "Robert","Byrd", "Bryd",
  "Michael", "Honda",  "Honds",
  "Lois", "Capps", "Caps",
  "Joseph", "Crowley", "(Crowly|Cowley)",
  "Joseph", "Lieberman", "Liberman",
  "Juanita", "Millender-McDonald", "Millender-McDonal",
  "John", "Shimkus", "(Shimku|Slimkus)",
  "John", "Sullivan", "Sulivan",
  "Jon", "Porter", "(Poster|Parter)",
  "David", "Cicilline", "Cicillin",
  "David", "Vitter", "Vilter",
  "Debbie", "Stabenow", "(Stebenow|Stabeno)",
  "Dianne", "Feinstein", "(Feinsten|Feinstein|Feinstien|Fenstein)",
  "Don", "Nickles", "Nickels",
  "Doug", "Lamborn", "(Lamborg|Lambon)",
  'Edward', "Markey", "Marley",
  "John", "Moolenaar", "Molinar",
  "Jery", 'Costello', "Costelo",
  "Jerry", "Kleczka", "Kyleczka",
  "John", "Barrow", "Barroy",
  "Jeff", "Merkley", "(Merkly|Merkeley)",
  "James", "Jeffords", "(Jefford|Jeffers)",
  "Jack", "Kingston", "Kington",
  "Greg", "Walden", "Wilden",
  "George","Radanovich", "Radavich",
  "George", "Nethercutt", "Nethecutt",
  "Frank", "Murkowski", "Mukowski",
  "Marcy", "Kaptur", "Kaptor",
  "Bob", "Goodlatte", "Goodlat",
  "Blaine", "Luetkemeyer", "Leautkemeyer",
  "Dutch", "Ruppersberger", "Rupperberger",
  "Carolyn", "Maloney", "Malony",
  "Catherine", "Cortez Masto", "Cortez Mastro", 
  "Charles", "Rangel", "Ranger",
  "Christopher", "Van Hollen", "Van Kollen", 
  "Gary", "Ackerman", "Acherman",
  "Conrad", "Burns", "Bums",
  "John", "Hostettler", "Hostetler",
  "Amy", "Klobuchar", "Klobachur",
  "Raja", "Krishnamoorthi", "Krishnamoothi",
  "Barbara", "Mikulski", "Milkulski",
  "Ruben", "Hinojosa", "Hinohosa",
  "George", "Lemieux", "Lemieuz",
  "Tom", "Periello", "Perielo",
  "Stephanie", "(Herseth|Sandlin)", "Herseth Sandlin",
  "John", "ROCKEFELLER", "ROCKFELLER",
  "Ron", "Wyden", "Wydon",
  "James", "Cornyn", "(Comyn|Com yn|Corvyn|Coryn)",
  "Joe", "Manchin", "Machin",
  "Blake", "Farenthold", "Farenhold",
  "Anna", "Eshoo", "Eschoo",
  "Heidi", "Heitkamp", "Heitkmap",
  "Gerry", "Connolly", "Connelly",
  "Anthony", "Gonzalez", "Gonzales",
  "Shelley", "Capito", "Moore Capito",
  "James", "Inhofe", "(Inholfe|Imhofe|Imholfe|Inhoffe)",
  "Yvette", "Clarke", "Clark",
  "Maurice", "Hinchey", "Henchey",
  "Chaka", "Fattah", "Chakka",
  "Kirsten", "Gillibrand", "Gillbrand",
  "James","Barrett", "(Barrat|Barret)",
  "Jaime", "HERRERA BEUTLER", "(HERRERA|BEUTLER)",
  "Lucille", "ROYBAL-ALLARD", "(Allard|Roybal)",
  "James", "Barrett", "(GRESHAM|BARRETT)",
  "Catherine", "CORTEZ MASTO", "(CORTEZ|MASTO|MATSO)",
  "Thomas","Coburn","(Cobum|Co bum)",
  "Trent", "Kelly", "Key",
  "Robin", "Kelly", "Key",
  "Mike", "Kelly", "Key",
  "Jon","Kyl", "Kyi",
  "Cliff", "Stearns","Steams",
  "Wayne", "Whitfield", "Whitefield",
  "Jane", "Harman", "Harmon",
  "Christopher", "Van Hollen", "(Van|Hollen)",
  "Luis", "GUTIERREZ", "(Guitierrez|GUTI.RREZ)",
  "Debbie", "WASSERMAN SCHULTZ", "(Wasserman|Schultz)",
  "Cathy", "McMORRIS RODGERS", "(McMorris|Rodgers)",
  "James", "Barrett", "(Baret|Barett|Barret)",
  "Mario", "Diaz-Balart", "(Diaz|Balart)",
  "Lincoln", "Diaz-Balart", "(Diaz|Balart)",
  "Stephanie", "Lujan Grishman", "Luj.n Grishman",
  "Carol", "Shea-Porter", "Shea|Porter",
  "Daniel", "Donovan", "Donavan"

  
  
  
) %>% 
  mutate(typos = str_c(str_c(first_name, "( | [A-z]* )", last_name_typos), 
                       str_c(last_name_typos, ", ", first_name), sep = "|") ) %>% 
  select(first_name, last_name, typos)


# FREQUENT FIRST NAME TYPOS 
typos_first <- tribble(
  ~first_name, ~last_name, ~first_name_typos,
  "Stephen", "Lynch", "Steven", 
  "Zoe", "Lofgren", "Toe", 
  "Jeanne", "Shaheen", "Teanne", 
  "Ron", "Wyden", "Roy", 
  "Russell","Feingold", "(Russel|Rusell)",
  "Ric", "Keller", "Rick", 
  "Orrin", "Hatch", "Orring",
  "Olympia","Snowe", "Olymia",
  "Shelly","Capito", "Shelley", 
  "Charles", "Schumer", "(Charls|Charls E)", # FIXME just adding middle initial for now, but eventually, it should be added to the typo pattern by merging with members data
  "Julia", "Carson", "Julie", 
  "Tom", "Barrett", "Mark", 
  "Matt", "Cartwright", "Mark", 
  "Katherine", "Clark", "Kathrine", 
  "Randy", "Weber", "Randay", 
  "Nancy", "Pelosi", "Nanci", 
  "Michael", "Honda", "(Midlael|Michaell)", 
  "Patrick", "Leahy", "Partrick", 
  "Ralph", "Regula", "Raplh", 
  "Chris", "Gibson", "Cris", 
  "Ron", "Estes", "(John|Jon)",
  "Dean", "Heller", "Den",
  "Dennis", "Cardoza", "Dinnes",
  "Harry", "Reid", "Hary",
  "George", "Voinovich", "Geaorge",
  "Candice", "Miller", "Candance",
  "Cedric", "Richmond", "Cedic",
  "Christopher", "Smith", "Christoper",
  "Christopher", "Bond", "(Chritoper|Christoper)",
  "Chris", "Coons", "Cris",
  "Chris", "Stewart", "Cris",
  "Hillary", "Clinton", "(Hilllary|Hilary|Fillary)",
  "Filemon", "Vela", "Filimon",
  "Arthur", "Davis", "Artur",
  "Patrick", "Leahy", "Ted",
  "Mary", "Bono", "Mack",
  "Harry", "Reid", "Marry",
  "Anna A.", "Eshoo", "Anna",
  "Darrell", "Issa", "(Darryl|Daryl|Darrel|Darel)",
  "Dianne", 'Feinstein', "Diane",
  "Eliot", "Engel", "Eliott",
  "Colin", "Allred", "Collin",
  "Collin", "Clark", "Colin",
  "Denise", "Majette", "Denis",
  "Dennis", "Ross", "Denis",
  "Dennis", "Cardoza", "Denis",
  "Dennis", "Moore", "Denis",
  "Dennis", "KUCINICH", "Denis",
  "Sheila", "Jackson", "Shelee",
  "Jose", "Serrano", "Jos.",
  "Lindsey", "Graham", "Lindsay",
  "Andre", "Carson", "Andr."
   
)   %>% 
  mutate(typos = str_c(str_c(first_name_typos, "( | [A-z]* )", last_name), # match any middle initial
                       str_c(last_name, ", ", first_name_typos), sep = "|" ) ) %>% 
  select(first_name, last_name, typos)

  # FREQUENT MIDDLE NAME TYPOS 
typos_middle <-  tribble(
    ~first_name, ~middle_name, ~last_name, ~middle_name_typos, 
    "Hillary", "Rodham", "Clinton", "Redham",
    "Benjamin", "Nighthorse", "Campbell", "Nighhorse",
    "Shelley", "Moore", 'Capito', "Moore Capito",
    "John", "Dennis", "Hastert", "Denis",
    "Mike", "Dennis", "Rehberg", "Denis"
  ) %>% 
  mutate(typos = str_c(paste(first_name, middle_name_typos, last_name),
                       str_c(last_name, ", ", first_name, " ", middle_name_typos), sep = "|") ) %>% 
  select(first_name, middle_name, last_name, typos)
  
# FREQUENT COMMON NAME TYPOS 
typos_common_name <-  tribble(
  ~common_name, ~last_name, ~common_name_typos, 
  "Jon", "Corize", "John",
  "Joe", "Barton", "Joel",
  "Ben", "Cardin", "Bin",
  "Don", "Nickles", "Dob",
  "Don", "Young", "Dob",
  "Dennis", "Heck", "Denis",
  "Dennis", "Rehberg", "Denis"
) %>% 
  mutate(typos = str_c(str_c(common_name_typos, "( | [A-z]* )", last_name),
                       str_c(last_name, ", ", common_name_typos), sep = "|") ) %>% 
  select(common_name, last_name, typos)

  # FREQUENT MIDDLE INITIAL TYPOS 
 typos_middle_initial <- tribble(
    ~first_name, ~middle_initial, ~last_name, ~middle_initial_typos, 
    "Richard", "G", "Lugar", "D", 
    "Roger", "F", "Wicker", "W", 
    "Lindsey", "O", "Graham", "D", 
    "Michael","E", "Capuano", "M", 
    "Nita", "M", "Lowey","L", 
    "Rosa", "L", "DeLauro", "I",
    "Joseph", "I", "Lieberman", "(J|L)" ,
    "John", "F", "Kerry", "P",
    "Jon", "S", "Corize", "C",
    "David", "N", "Cicilline", "(R|L|P)",
    "David", "B", "McKinley", "P",
    "James", "R", "Langevin", "P",
    "James", "M", "Inhofe", "N",
    "Charles", "H", "Taylor", "F",
    "Eliot", "L", "Engel", "E",
    "Bobby", "L", "Rush", "E",
    "Gary", "L", "Ackerman", "J"
  )%>% 
   mutate(typos = str_c(paste(first_name, middle_initial_typos, last_name),
                        str_c(last_name, ", ", first_name, " ", middle_initial_typos), sep = "|") ) %>% 
   select(first_name, middle_initial, last_name, typos)
 
 
 
 
 
 # #Fixes name typo (from DOL_SOL)
 
 #FIXED and added names into miscellaneous tables above
 
 # data$FROM %<>%
 #   str_replace("Davis, Arthur", "Davis, Artur") %>%
 #   str_replace("Gillibrand, Kirstein", "Gillibrand, Kirsten") %>%
 #   str_replace("Leahy, Ted", "Leahy, Patrick") %>%
 #   str_replace("Gerlah, Jim", "Gerlach, Jim") %>%
 #   str_replace("Obama, Brack", "Obama, Barack") %>%
 #   str_replace("Hooley, Darene", "Hooley, Darlene")
 # 
 # 
 # 
 
  
# combine typos 
typos <- full_join(typos_first, typos_last) %>% 
  full_join(typos_middle) %>% 
  full_join(typos_middle_initial) %>% 
  mutate(correct = paste(first_name, last_name)) %>% 
  group_by(correct) %>%
  summarise(typos = typos %>% str_c(collapse = "|") ) %>%
    full_join(typos_clear)


# A helper function to return the full regex pattern string (so that we can join on pattern) where it finds a match
str_detect_replace <- function(string, pattern){
  out <- ifelse(str_detect(string, pattern), pattern, "404error")
}


findTypos <- function(from){
  purrr::map(.x = typos$typos, 
             .f= str_detect_replace,
             string = from) %>% 
    unlist() %>%
    unique() %>% 
    # seperate pattrns found with OR 
    str_c(collapse = "|") %>%
    # remove 404error when it appears along side a found pattern
    str_remove("\\|404error|404error\\|")
}






  
  ######################################################################################
# This function will replace extractMemberNames after it has been tested and vetted
# It does not use format first and last name columns. 
# It does correct ocr.errors and then corrects typos using the typos tables
# It then uses the pattern variable in the members data to match names 
  extractMemberName2 <- function(data, members, col_name){

    data %<>% mutate(Summary = data[[col_name]])
    
    # clean up text
    data$Summary %<>% cleanFROMcolumn()

    # correct common OCR errors
    data$Summary %<>% ocr.errors()
    
    # Fix name typos
    data %<>% 
      # find common typos
      mutate(typos = Summary %>% map_chr(findTypos)) %>% 
      # add in corrections
      left_join(typos) %>%
      # replace typos with corrections
      mutate(Summary = str_replace_all(Summary, regex(typos, ignore_case = T), correct)) %>% 
      mutate(typos = str_replace(typos, "404error", "none"))
    
    
    # A helper function to return the full regex pattern string (so that we can join on pattern) where it finds a match
    str_detect_replace <- function(string, pattern){
      out <- ifelse(str_detect(string, pattern), pattern, "404error")
    }
    
    # A function to map over members 
    # (assumes that memmbers object contains congress and pattern)
    # (assumes that data data contains congress and from)
    extractName <- function(from){
      purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern), 
                 .f= str_detect_replace,
                 string = from) %>% 
        unlist() %>%
        unique() %>% 
        str_c(collapse = ";") %>%
        str_remove(";404error|404error;")
    }
    
    
    # match patters from members file and merge with member names
    data %<>%
      ungroup() %>%
      # map function to detect members over lower case version of FROM 
      mutate(from = tolower(Summary),
             pattern = map_chr(from, extractName) ) %>% # select(from,matches)
      # split out multiple members into separate rows 
      mutate(pattern = str_split(pattern, ";")  ) %>% 
      unnest() %>% 
      # join in members data by pattern 
      left_join(members %>% select(pattern, first_name, last_name, congress)) %>% 
      select(-from) # %>% select(FROM, pattern, first_name, last_name)
    
    return(data)
  }
  
