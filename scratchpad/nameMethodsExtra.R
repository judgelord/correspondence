# Function may need small add ons or adjustments for new/different datasets
# Function will take comma separated names (e.g. Johnson, Ralph) from a specified column (usually FROM) 
# and create first_name and last_name columns in the dataframe. Typical call: getFirstLast.Comma(data,'FROM')

# THIS FUNCTION IS NOT PART OF extractMemberNames, so these corrections will not help that function all corrections should appear in the same place and be called by each method
# this function should be able to take in different members (or we rewrite to be cogress-specific and use the full members list)
getFirstLast.Comma <- function(data, col_name){
  
  # FIXME # due to memory subsetting members 
  members %<>% filter(congress > 109)
  
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
    mutate(last_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "CASEY", last_name)) %>% #fixed in name congress
    mutate(first_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "Robert", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "Dutch", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "KRATOVIL", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% # no error
    mutate(last_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON|Lee", FROM2,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON|Lee", FROM2,ignore.case = TRUE), "Sheila", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Paul", FROM2,ignore.case = TRUE)&grepl("KIRK", FROM2,ignore.case = TRUE), "KIRK", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Paul", FROM2,ignore.case = TRUE)&grepl("KIRK", FROM2,ignore.case = TRUE), "Paul", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE), "GRESHAM", last_name)) %>% #no error
    #mutate(first_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE)&grepl("James", FROM2,ignore.case = TRUE), "Paul", first_name)) %>% #incorrect error
    mutate(last_name = ifelse(grepl("John", FROM2,ignore.case = TRUE)&grepl("Hall", FROM2,ignore.case = TRUE), "HALL", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("John", FROM2,ignore.case = TRUE)&grepl("Hall", FROM2,ignore.case = TRUE), "John", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("ENBRENNER", FROM2,ignore.case = TRUE), "SENSENBRENNER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("ENBRENNER", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("FRELINGHUY", FROM2,ignore.case = TRUE), "FRELINGHUYSEN", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("FRELINGHUY", FROM2,ignore.case = TRUE), "Rodney", first_name)) %>% #no error
    #mutate(last_name = ifelse(grepl("LARRICK", FROM2,ignore.case = TRUE), "LARSEN", last_name)) %>% #may need to fix later
    #mutate(first_name = ifelse(grepl("LARRICK", FROM2,ignore.case = TRUE), "Rick", first_name)) %>% 
    mutate(last_name = ifelse(grepl("BOUSTANY", FROM2,ignore.case = TRUE), "BOUSTANY", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("BOUSTANY", FROM2,ignore.case = TRUE), "Charles", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("King", FROM2,ignore.case = TRUE)&grepl("Jr", FROM2,ignore.case = TRUE), "KING", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("King", FROM2,ignore.case = TRUE)&grepl("jr", FROM2,ignore.case = TRUE), "Angus", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Andr", FROM2,ignore.case = TRUE)&grepl("Carson", FROM2,ignore.case = TRUE), "CARSON", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Andr", FROM2,ignore.case = TRUE)&grepl("Carson", FROM2,ignore.case = TRUE), "Andre", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE),"SERRANO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE), "Jose", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "HINOJOSA", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "Ruben", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "PAYNE", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "Donald", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "PASCRELL", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "Bill", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Kay", FROM2,ignore.case = TRUE)&grepl("Hutch", FROM2,ignore.case = TRUE), "HUTCHISON", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Kay", FROM2,ignore.case = TRUE)&grepl("Hutch", FROM2,ignore.case = TRUE), "Kay", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Mack|Mary", FROM2,ignore.case = TRUE)&grepl("Bono", FROM2,ignore.case = TRUE), "BONO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Mack|Mary", FROM2,ignore.case = TRUE)&grepl("Bono", FROM2,ignore.case = TRUE), "Mary", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Arlen", FROM2,ignore.case = TRUE)&grepl("Spec", FROM2,ignore.case = TRUE), "SPECTER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Arlen", FROM2,ignore.case = TRUE)&grepl("Spec", FROM2,ignore.case = TRUE), "Arlen", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Chris", FROM2,ignore.case = TRUE)&grepl("Van", FROM2,ignore.case = TRUE)&grepl("Hollen", FROM2,ignore.case = TRUE), "VAN HOLLEN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Chris", FROM2,ignore.case = TRUE)&grepl("Van", FROM2,ignore.case = TRUE)&grepl("Hollen", FROM2,ignore.case = TRUE), "Chris", first_name))  %>% #fixed
    mutate(last_name = ifelse(grepl("Bonnie", FROM2,ignore.case = TRUE)&grepl("Coleman", FROM2,ignore.case = TRUE), "WATSON COLEMAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Bonnie", FROM2,ignore.case = TRUE)&grepl("Coleman", FROM2,ignore.case = TRUE), "Bonnie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Beto", FROM2,ignore.case = TRUE)&grepl("ROURKE", FROM2,ignore.case = TRUE), "O'ROURKE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Beto", FROM2,ignore.case = TRUE)&grepl("ROURKE", FROM2,ignore.case = TRUE), "Beto", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Gloria", FROM2,ignore.case = TRUE)&grepl("McLeod|Negrete", FROM2,ignore.case = TRUE), "NEGRETE McLEOD", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Gloria", FROM2,ignore.case = TRUE)&grepl("McLeod", FROM2,ignore.case = TRUE), "Gloria", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Frank", FROM2,ignore.case = TRUE)&grepl("Pallone", FROM2,ignore.case = TRUE), "PALLONE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Frank", FROM2,ignore.case = TRUE)&grepl("Pallone", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Sanford", FROM2,ignore.case = TRUE)&grepl("Bishop", FROM2,ignore.case = TRUE), "BISHOP", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Sanford", FROM2,ignore.case = TRUE)&grepl("Bishop", FROM2,ignore.case = TRUE), "Sanford", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Matso|Masto", FROM2,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Matso|Masto", FROM2,ignore.case = TRUE), "Catherine", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n", FROM2,ignore.case = TRUE)&grepl("Ben", FROM2,ignore.case=TRUE), "LUJAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n", FROM2,ignore.case = TRUE)&grepl("Ben", FROM2,ignore.case=TRUE), "Ben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Donovan|Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "DONOVAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Donovan|Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "Daniel", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Randy| j.|james|j ", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "FORBES", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Randy| j.|james|j ", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Mario|Lincoln", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Mario", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "Mario", first_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Lincoln", FROM2,ignore.case = TRUE)&grepl("Diaz|Balart", FROM2,ignore.case=TRUE), "Lincoln", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Gresham", FROM2,ignore.case = TRUE)&grepl("Barret", FROM2,ignore.case=TRUE), "BARRETT", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Gresham",FROM2,ignore.case=TRUE)&grepl("Barret",FROM2,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Deb", FROM2,ignore.case = TRUE)&grepl("Wasserman|Schultz", FROM2,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Deb",FROM2,ignore.case=TRUE)&grepl("Wasserman|Schultz",FROM2,ignore.case=TRUE), "Debbie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Kristen|Kirsten", FROM2,ignore.case = TRUE)&grepl("Gil", FROM2,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Kristen|Kirsten",FROM2,ignore.case=TRUE)&grepl("Gil",FROM2,ignore.case=TRUE), "Kirsten", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Jo ",FROM2,ignore.case=TRUE)&grepl("Davis",FROM2,ignore.case=TRUE), "DAVIS", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jo ",FROM2,ignore.case=TRUE)&grepl("Davis",FROM2,ignore.case=TRUE), "Jo", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Waite|Brown",FROM2,ignore.case=TRUE)&grepl("Ginny|Virginia",FROM2,ignore.case=TRUE), "BROWN-WAITE", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Waite|Brown",FROM2,ignore.case=TRUE)&grepl("Ginny|Virginia",FROM2,ignore.case=TRUE), "Virginia", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Jo",FROM2,ignore.case=TRUE)&grepl("Emerson",FROM2,ignore.case=TRUE), "EMERSON", last_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Jo",FROM2,ignore.case=TRUE)&grepl("Emerson",FROM2,ignore.case=TRUE), "Jo", first_name)) %>% #no error
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
# FIXME 
# REWRITE WITH purrr


# a helper function to concat unique strings
unique_string <- . %>% 
  str_split(";") %>% 
  # select unique ones
  unlist() %>% 
  unique() %>% 
  trimws() %>% 
  # paste them back together to retrun a single value 
  paste(collapse = ";") %>%
  str_remove_all("na;|;na$|NA;|;NA$")

# Just the PAC names and IDs from the contributions matrix (will merge in relevent contributions later)
m <- members %>% 
  select(congress, bioname, pattern) %>% distinct()

# A function to get matching names
get_bioname <- function(FROM, pattern){
  ifelse(str_detect(tolower(FROM), tolower(pattern)), bioname, NA)
}

# A function to get IDs where names match 
get_bioname <- function(FROM, pattern, bioname){
  ifelse(str_detect(FROM, pattern), bioname, NA) 
}


get_bionames <- function(FROM){
  members %>% 
    mutate(bioname = get_bioname(FROM, pattern, bioname)) %>% 
    ungroup()
  return(members)
}

crosswalk <- map_dfr(data$FROM, get_bionames)

crosswalk %<>% distinct()








# git one 
extractName <- function(data){
  ifelse(str_detect(data, str_c(members$common_last, members$first_last, sep = "|")), 
         members$bioname, 
         NA)
}

# data %>% mutate(FROM2 = map_chr(FROM,  extractName))
str_extract_1 <- . %>% {str_extract_all(string = string, pattern = .) %>% 
    unlist() %>% 
    str_c(collapse = ";")}

getbioname <- function(members, person){
  m <- as.tibble(members)
  
  ifelse(str_detect(person, m$pattern),
         m$bioname,
         NA) %>% 
    unique()
}

map(members, .f = getbioname, person = "Peter Welch") 

str_extract_ignore <- function(string, pattern){
  str_extract(string, pattern)
}

str_extract_1 <- function(.x, string){
  str_extract(string = string, pattern = .x)
}

# git all 
extractMembers <- function(data){
  
  data %<>% 
    mutate(from = tolower(FROM)) %>%
    mutate(names = purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern) %>% as.list(), 
                              .f= str_extract_1,
                              string = from) %>% 
             unlist() %>%
             na.omit() %>% 
             unique()  %>%
             str_c(collapse = ";") )
  
  return(data)
}

data %>% extractMembers() %>% select(FROM, names)





















# 6-19

str_extract_1 <- function(.x, string){
  str_extract(string = string, pattern = .x)
}


extractName <- function(from){
purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern), 
           .f= str_extract_1,
           string = from) %>% 
  unlist() %>%
  na.omit() %>% 
  unique()  %>%
  str_c(collapse = ";") 
}

extractName(from = "foo peter welch bar ")

data %<>% 
  mutate(from = tolower(FROM))

data <- head(data)
data$FROM
data$from

map_chr(data$from, extractName)

data %>% 
  mutate(from = tolower(FROM),
         name = map(from, extractName) ) %>% 
  select(FROM, from, name)

# Rewrite with join? 
data %<>% 
  mutate(first_last = str_extract(str_c(members$f)))
  
# data %>% mutate(FROM2 = map2(FROM, members, extractNames))


















#####################################################
# THIS ONE WORKS

data$Summary <- data$FROM
data %>% mutate(na = is.na(last_name)) %>% count(na)

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

data %<>%
  ungroup() %>%
  #top_n(10) %>% 
  # map function to detect members over lower case version of FROM 
  mutate(from = tolower(Summary),
         pattern = map_chr(from, extractName) ) %>% # select(from,matches)
  # split out multiple members into separate rows 
  mutate(pattern = str_split(pattern, ";")  ) %>% 
  unnest() %>% 
  # join in members data by pattern 
  left_join(members %>% select(pattern, first_name, last_name, congress)) %>% 
  select(-from) # %>% select(FROM, pattern, first_name, last_name)

data %>% mutate(na = is.na(last_name)) %>% count(na)








if(F){ # Testing 
  
  
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
      str_c(collapse = ";") %>%
      str_remove(";404error|404error;")
  }
  
  data %<>% 
    mutate(typos = FROM %>% map_chr(findTypos)) %>% 
    left_join(typos) %>% 
    mutate(FROM = str_replace_all(FROM, regex(typos, ignore_case = T), correct))
  
  data %>% select(FROM, correct, typos) %>% .[18,]
}





###########################################################################################################
# This function will extract names found in the members dataset
# typical call:   
# data %<>% extractMemberName(members, 'FROM') 
# NOTE: A VAR NAMED "members" IN DATA CAN CAUSE PROBLEMS AND A VAR NAMED string WILL BE OVERWRITTEN 

extractMemberNameOld <- function(data, members, col_name){
  
  # FIXME # due to memory subsetting members 
  members %<>% filter(congress > 109)
  
  data %<>% mutate(string = data[[col_name]])
  
  # clean up text
  data$string %<>% cleanFROMcolumn()
  
  
  # correct common OCR errors
  data$string <- data$string %>% ocr.errors() %>% tolower()
  
  
  data %<>% 
    # find common typos
    mutate(typos = string %>% map_chr(findTypos)) %>% 
    # add in corrections
    left_join(typos) %>% #, by = c("typos", "correct")) %>%
    # replace typos with corrections
    mutate(string = str_replace_all(string, regex(typos, ignore_case = T), correct)) %>%
    mutate(typos = str_replace(typos, "404error", "none"))
  
  
  #####################
  # Match names in different formats
  ###################
  
  # create FROM2 varible extracting name from data$string
  
  # WARNING THIS OVERWRITES WITH THE MOST RECENT NAME MATCHED
  
  # extract common_last name formats
  data$FROM2 <- gsub(pattern = paste(c('.*(', paste(members$common_last[1:850], collapse = '|'), ').*'), collapse = ""),
                     replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    
    # extracts  first_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    
    # first_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    
    # first_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    
    # common_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    
    # common_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[2550:3400], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[3400:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$string, ignore.case = TRUE) 
  
  
  
  
  
  
  
  # Assume first name is first word and last name appears last? 
  # THIS SEEMS LIKE A BAD ASSSUMPTION 
  data$first_name <- gsub("^(\\w+) .*", replacement = "\\1", data$FROM2)
  data$last_name <- gsub(".* (\\w+)$", replacement = '\\1', data$FROM2)
  
  
  # apply formating functions from above
  data$first_name <- formatFirstName(data, 'first_name')
  data$last_name <- formatLastName(data, 'last_name')
  
  # keep if first name matches a first name in the members data, otherwise, NA
  data %<>%
    mutate(first_name = ifelse(   grepl(paste(members$first_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE),
                                  first_name, NA) ) 
  
  # keep if last name matches a last name in the members file, otherwise, NA
  data %<>% 
    mutate(last_name = ifelse(
      grepl(paste(members$first_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$string, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$string, ignore.case = TRUE), 
      last_name, NA)) 
  
  # if both first name and last name are NA, FROM2 is NA 
  data %>% 
    mutate(FROM2 = ifelse( is.na(first_name) & is.na(last_name), NA, FROM2))
  
  # corrections to first and last names 
  data %<>%
    mutate(last_name = ifelse(grepl("Matso|Masto", string,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Matso|Masto", string,ignore.case = TRUE), "Catherine", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n", string,ignore.case = TRUE)&grepl("Grishman|Grisham", string,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n", string,ignore.case = TRUE)&grepl("Grishman|Grisham", string,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n", string,ignore.case = TRUE)&grepl("Ben", string,ignore.case=TRUE), "LUJAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n", string,ignore.case = TRUE)&grepl("Ben", string,ignore.case=TRUE), "Ben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Deb", string,ignore.case = TRUE)&grepl("Wasserman|Schultz", string,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Deb",string,ignore.case=TRUE)&grepl("Wasserman|Schultz",string,ignore.case=TRUE), "Debbie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Mario|Lincoln", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Mario", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "Mario", first_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Lincoln", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "Lincoln", first_name)) #fixed
  
  # Fix specific common errors
  data %<>%
    mutate(last_name = ifelse(grepl("HERSETH", string, ignore.case = TRUE )|grepl('SANDLIN', string, ignore.case = TRUE), "HERSETH SANDLIN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("HERSETH", string,ignore.case = TRUE)|grepl('SANDLIN', string,ignore.case = TRUE), "Stephanie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("PAULSEN", string, ignore.case = TRUE )&grepl('Erik', string, ignore.case = TRUE), "PAULSEN", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("PAULSEN", string,ignore.case = TRUE)&grepl('Erik', string,ignore.case = TRUE), "Erik", first_name)) %>% #no error 
    mutate(last_name = ifelse(grepl("CONYERS", string, ignore.case = TRUE )&grepl('John', string, ignore.case = TRUE), "CONYERS", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("CONYERS", string,ignore.case = TRUE)&grepl('John', string,ignore.case = TRUE), "John", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Ben|E.B|E B", string,ignore.case = TRUE)& grepl('NELSON', string,ignore.case = TRUE), "NELSON", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Ben|E.B|E B", string,ignore.case = TRUE)& grepl('NELSON', string,ignore.case = TRUE), "Ben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Casey", string,ignore.case = TRUE)& grepl('Rob|Bob|Jr', string,ignore.case = TRUE), "CASEY", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Casey", string,ignore.case = TRUE)& grepl('Rob|Bob|Jr', string,ignore.case = TRUE), "Robert", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("RUPPERSBERGER", string,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("RUPPERSBERGER", string,ignore.case = TRUE), "Dutch", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("KRATOVIL", string,ignore.case = TRUE), "KRATOVIL", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("KRATOVIL", string,ignore.case = TRUE), "Frank", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Sheila", string,ignore.case = TRUE)&grepl("JACKSON|Lee", string,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Sheila", string,ignore.case = TRUE)&grepl("JACKSONLee", string,ignore.case = TRUE), "Sheila", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Paul", string,ignore.case = TRUE)&grepl("KIRK", string,ignore.case = TRUE), "KIRK", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Paul", string,ignore.case = TRUE)&grepl("KIRK", string,ignore.case = TRUE), "Paul", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Gresham", string,ignore.case = TRUE), "BARRETT", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Gresham", string,ignore.case = TRUE)&grepl("James|Barret", string,ignore.case = TRUE), "James", first_name)) %>% #may need to fix later
    mutate(last_name = ifelse(grepl("John", string,ignore.case = TRUE)&grepl("Hall", string,ignore.case = TRUE), "HALL", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("John", string,ignore.case = TRUE)&grepl("Hall", string,ignore.case = TRUE), "John", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("ENBRENNER", string,ignore.case = TRUE), "SENSENBRENNER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("ENBRENNER", string,ignore.case = TRUE), "Frank", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("FRELINGHUY", string,ignore.case = TRUE), "FRELINGHUYSEN", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("FRELINGHUY", string,ignore.case = TRUE), "Rodney", first_name)) %>% #no error 
    mutate(last_name = ifelse(grepl("LARRICK", string,ignore.case = TRUE), "LARSEN", last_name)) %>% #may need to fix later
    mutate(first_name = ifelse(grepl("LARRICK", string,ignore.case = TRUE), "Rick", first_name)) %>% #may need to fix later
    mutate(last_name = ifelse(grepl("BOUSTANY", string,ignore.case = TRUE), "BOUSTANY", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("BOUSTANY", string,ignore.case = TRUE), "Charles", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("HINOJOSA", string,ignore.case = TRUE), "HINOJOSA", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("HINOJOSA", string,ignore.case = TRUE), "Ruben", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Payne", string,ignore.case = TRUE)&grepl("Don", string,ignore.case = TRUE), "PAYNE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Payne", string,ignore.case = TRUE)&grepl("Don", string,ignore.case = TRUE), "Donald", first_name)) %>% #fixed 
    mutate(last_name = ifelse(grepl("Pascrell", string,ignore.case = TRUE)&grepl("Bill", string,ignore.case = TRUE), "PASCRELL", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Pascrell", string,ignore.case = TRUE)&grepl("Bill", string,ignore.case = TRUE), "Bill", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Tony", string,ignore.case = TRUE)&grepl("C.rdenas", string,ignore.case = TRUE), "CARDENAS", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Tony", string,ignore.case = TRUE)&grepl("C.rdenas", string,ignore.case = TRUE), "Tony", first_name)) %>%  #fixed
    mutate(last_name = ifelse(grepl("Kay", string,ignore.case = TRUE)&grepl("Hutch", string,ignore.case = TRUE), "HUTCHISON", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Kay", string,ignore.case = TRUE)&grepl("Hutch", string,ignore.case = TRUE), "Kay", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Mack|Mary", string,ignore.case = TRUE)&grepl("Bono", string,ignore.case = TRUE), "BONO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Mack|mary", string,ignore.case = TRUE)&grepl("Bono", string,ignore.case = TRUE), "Mary", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Arlen", string,ignore.case = TRUE)&grepl("Spec", string,ignore.case = TRUE), "SPECTER", last_name)) %>% #no error
    mutate(first_name = ifelse(grepl("Arlen", string,ignore.case = TRUE)&grepl("Spec", string,ignore.case = TRUE), "Arlen", first_name)) %>% #no error
    mutate(last_name = ifelse(grepl("Chris", string,ignore.case = TRUE)&grepl("Van", string,ignore.case = TRUE)&grepl("Hollen", string,ignore.case = TRUE), "VAN HOLLEN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Chris", string,ignore.case = TRUE)&grepl("Van", string,ignore.case = TRUE)&grepl("Hollen", string,ignore.case = TRUE), "Chris", first_name))  %>% #fixed
    mutate(last_name = ifelse(grepl("Bonnie", string,ignore.case = TRUE)&grepl("Coleman", string,ignore.case = TRUE), "WATSON COLEMAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Bonnie", string,ignore.case = TRUE)&grepl("Coleman", string,ignore.case = TRUE), "Bonnie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Beto", string,ignore.case = TRUE)&grepl("ROURKE", string,ignore.case = TRUE), "O'ROURKE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Beto", string,ignore.case = TRUE)&grepl("ROURKE", string,ignore.case = TRUE), "Beto", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Gloria", string,ignore.case = TRUE)&grepl("McLeod|Negrete", string,ignore.case = TRUE), "NEGRETE McLEOD", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Gloria", string,ignore.case = TRUE)&grepl("McLeod", string,ignore.case = TRUE), "Gloria", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Frank", string,ignore.case = TRUE)&grepl("Pallone", string,ignore.case = TRUE), "PALLONE", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Frank", string,ignore.case = TRUE)&grepl("Pallone", string,ignore.case = TRUE), "Frank", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Sanford", string,ignore.case = TRUE)&grepl("Bishop", string,ignore.case = TRUE), "BISHOP", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Sanford", string,ignore.case = TRUE)&grepl("Bishop", string,ignore.case = TRUE), "Sanford", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Matso|Masto", string,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Matso|Masto", string,ignore.case = TRUE), "Catherine", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Luj.n|Michelle", string,ignore.case = TRUE)&grepl("Grishman|Grisham", string,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Luj.n|Michelle", string,ignore.case = TRUE)&grepl("Grishman|Grisham", string,ignore.case=TRUE), "Michelle", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Donovan|Donavan", string,ignore.case = TRUE)&grepl("Dan", string,ignore.case=FALSE), "DONOVAN", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Donovan|Donavan", string,ignore.case = TRUE)&grepl("Dan", string,ignore.case=FALSE), "Daniel", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Randy| j.|james|j ", string,ignore.case = TRUE)&grepl("Forbes", string,ignore.case=TRUE), "FORBES", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Randy| j.|james|j ", string,ignore.case = TRUE)&grepl("Forbes", string,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Mario|Lincoln", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Mario", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "Mario", first_name)) %>% #fixed
    mutate(first_name = ifelse(grepl("Lincoln", string,ignore.case = TRUE)&grepl("Diaz|Balart", string,ignore.case=TRUE), "Lincoln", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Gresham", string,ignore.case = TRUE)&grepl("Bar", string,ignore.case=TRUE), "BARRETT", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Gresham",string,ignore.case=TRUE)&grepl("Barret",string,ignore.case=TRUE), "James", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Deb", string,ignore.case = TRUE)&grepl("Wasserman|Schultz", string,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Deb",string,ignore.case=TRUE)&grepl("Wasserman|Schultz",string,ignore.case=TRUE), "Debbie", first_name)) %>% #fixed
    mutate(last_name = ifelse(grepl("Kristen|Kirsten", string,ignore.case = TRUE)&grepl("Gil", string,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Kristen|Kirsten",string,ignore.case=TRUE)&grepl("Gil",string,ignore.case=TRUE), "Kirsten", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Jo ",string,ignore.case=TRUE)&grepl("Davis",string,ignore.case=TRUE), "DAVIS", last_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Jo ",string,ignore.case=TRUE)&grepl("Davis",string,ignore.case=TRUE), "Jo", first_name)) %>% #no error
    mutate(last_name = ifelse( grepl("Waite|Brown",string,ignore.case=TRUE)&grepl("Ginny|Virginia",string,ignore.case=TRUE), "BROWN-WAITE", last_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Waite|Brown",string,ignore.case=TRUE)&grepl("Ginny|Virginia",string,ignore.case=TRUE), "Virginia", first_name)) %>% #no error
    mutate(last_name = ifelse( grepl("Jo",string,ignore.case=TRUE)&grepl("Emerson",string,ignore.case=TRUE), "EMERSON", last_name)) %>% #no error
    mutate(first_name = ifelse( grepl("Jo",string,ignore.case=TRUE)&grepl("Emerson",string,ignore.case=TRUE), "Jo", first_name)) %>% #no error
    mutate(last_name = ifelse( grepl("Shelley|Moore",string,ignore.case=TRUE)&grepl("Capito",string,ignore.case=TRUE), "CAPITO", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Shelley|Moore",string,ignore.case=TRUE)&grepl("Capito",string,ignore.case=TRUE), "Shelley", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("McMorris|Rodgers",string,ignore.case=TRUE)&grepl("Cathy|McMorris",string,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("McMorris|Rodgers",string,ignore.case=TRUE)&grepl("Cathy|McMorris",string,ignore.case=TRUE), "Cathy", first_name)) %>% #fixed
    
    mutate(last_name = ifelse( grepl("Rounds",string,ignore.case=TRUE)&grepl("Marion|Mike|Michael",string,ignore.case=TRUE), "ROUNDS", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Rounds",string,ignore.case=TRUE)&grepl("Marion|Mike|Michael",string,ignore.case=TRUE), "Marion", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Panetta",string,ignore.case=TRUE)&grepl("Jim|James",string,ignore.case=TRUE), "PANETTA", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Panetta",string,ignore.case=TRUE)&grepl("Jim|James",string,ignore.case=TRUE), "James", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Roybal",string,ignore.case=TRUE)&grepl("Allard",string,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Roybal",string,ignore.case=TRUE)&grepl("Allard",string,ignore.case=TRUE), "Lucille", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Clay",string,ignore.case=TRUE)&grepl("Lacy|William|Bill",string,ignore.case=TRUE), "CLAY", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Clay",string,ignore.case=TRUE)&grepl("Lacy|William|Bill",string,ignore.case=TRUE), "William", first_name))%>% #fixed
    #mutate(last_name = ifelse( grepl("Eleanor",string,ignore.case=TRUE)&grepl("Holmes|Norton",string,ignore.case=TRUE), "NORTON", last_name)) %>% 
    #mutate(first_name = ifelse( grepl("Eleanor",string,ignore.case=TRUE)&grepl("Holmes|Norton",string,ignore.case=TRUE), "Eleanor", first_name))%>% 
    #mutate(last_name = ifelse( grepl("Gregorio",string,ignore.case=TRUE)&grepl("Sablan",string,ignore.case=TRUE), "SABLAN", last_name)) %>% 
    #mutate(first_name = ifelse( grepl("Gregorio",string,ignore.case=TRUE)&grepl("Sablan",string,ignore.case=TRUE), "Gregorio", first_name))%>% 
    mutate(last_name = ifelse( grepl("Shea",string,ignore.case=TRUE)&grepl("Porter",string,ignore.case=TRUE), "SHEA-PORTER", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Shea",string,ignore.case=TRUE)&grepl("Porter",string,ignore.case=TRUE), "Carol", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Jane",string,ignore.case=TRUE)&grepl("Harmon",string,ignore.case=TRUE), "HARMAN", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jane",string,ignore.case=TRUE)&grepl("Harmon",string,ignore.case=TRUE), "Jane", first_name))%>% #fixed
    mutate(last_name = ifelse( grepl("Butterfield",string,ignore.case=TRUE)&grepl("G",string,ignore.case=TRUE), "BUTTERFIELD", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Butterfied",string,ignore.case=TRUE)&grepl("G",string,ignore.case=TRUE), "George", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Jon",string,ignore.case=TRUE)&grepl("Kyi",string,ignore.case=TRUE), "KYL", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jon",string,ignore.case=TRUE)&grepl("Kyi",string,ignore.case=TRUE), "Jon", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Lou",string,ignore.case=TRUE)&grepl("Gohmert",string,ignore.case=TRUE), "GOHMERT", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Lou",string,ignore.case=TRUE)&grepl("Gohmert",string,ignore.case=TRUE), "Louie", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Jaime|Jamie|Jaimie",string,ignore.case=TRUE)&grepl("Herrera|Beutler",string,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Jaime|Jamie|Jaimie",string,ignore.case=TRUE)&grepl("Herrera|Beutler",string,ignore.case=TRUE), "Jaime", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Dian",string,ignore.case=TRUE)&grepl("Feinstein|Feinstien|Fenstein",string,ignore.case=TRUE), "FEINSTEIN", last_name)) %>% #fixed
    #mutate(first_name = ifelse( grepl("Dian",string,ignore.case=TRUE)&grepl("Herrera|Beutler",string,ignore.case=TRUE), "Dianne", first_name)) %>% #incorrect name
    mutate(last_name = ifelse( grepl("Issa",string,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",string,ignore.case=TRUE), "ISSA", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Issa",string,ignore.case=TRUE)&grepl("Darryl|Daryl|Darrel|Darel",string,ignore.case=TRUE), "Darrell", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("Whitfield|Whitefield",string,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",string,ignore.case=TRUE), "WHITFIELD", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Whitfield|Whitefield",string,ignore.case=TRUE)&grepl("Edward|Ed|Wayne",string,ignore.case=TRUE), "Wayne", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Dana( |$)",string,ignore.case=TRUE)&grepl("(^| )RO.*HER( |$)",string,ignore.case=TRUE), "ROHRABACHER", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Dana( |$)",string,ignore.case=TRUE)&grepl("(^| )RO.*HER( |$)",string,ignore.case=TRUE), "Dana", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Womack( |$|,)",string,ignore.case=TRUE)&grepl("(^| )St",string,ignore.case=TRUE), "WOMACK", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Womack( |$|,)",string,ignore.case=TRUE)&grepl("(^| )St",string,ignore.case=TRUE), "Steve", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Mary( |$)",string,ignore.case=TRUE)&grepl("Mack|Bono",string,ignore.case=TRUE), "BONO", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Mary( |$)",string,ignore.case=TRUE)&grepl("Mack|Bono",string,ignore.case=TRUE), "Mary", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",string,ignore.case=TRUE)&grepl("Morgan| H | H\\.",string,ignore.case=TRUE), "GRIFFITH", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )GRIFFITH( |$|,)",string,ignore.case=TRUE)&grepl("Morgan| H | H\\.",string,ignore.case=TRUE), "Morgan", first_name)) %>% #fixed
    mutate(last_name = ifelse( grepl("(^| )Lindsay( |$|,)",string,ignore.case=TRUE)&grepl("(^| )Graham",string,ignore.case=TRUE), "GRAHAM", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Lindsay( |$|,)",string,ignore.case=TRUE)&grepl("(^| )Graham",string,ignore.case=TRUE), "Lindsey", first_name)) %>% #fixed
    
    mutate(last_name = ifelse( grepl("(^| )Conaway( |$|,)",string,ignore.case=TRUE)&grepl("(^| )Mi.",string,ignore.case=TRUE), "CONAWAY", last_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("(^| )Conaway( |$|,)",string,ignore.case=TRUE)&grepl("(^| )Mi.",string,ignore.case=TRUE), "Michael", first_name)) %>% #fixed
    mutate(first_name = ifelse( grepl("Anna A. ",string,ignore.case=TRUE),"Anna", first_name)) #fixed
  
  # i<- which(members$last_name == "GRAMM")[1] 
  ## clean up in future
  
  for (i in 1:length(members$id)) {
    data %<>% mutate(first_name = ifelse( !is.na(members$common_name[i]) & data$first_name == members$common_name[i] & data$last_name == members$last_name[i],
                                          members$first_name[i], data$first_name))
    
  }
  
  return(data)
  
  
}
