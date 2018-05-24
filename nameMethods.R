# Formats col_name (usually last_name) to similiar format as members$last_name
# Capitalizes letters and fixes common errors 
formatLastName <- function(data, col_name){
  
  data$last_name <- data[[col_name]]
  
  data %<>%
    mutate(last_name = str_to_upper(last_name)) %>% 
    # correct capitalization
    mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
    mutate(last_name = gsub("DEFAZIO", replacement = "DeFAZIO", last_name)) %>% 
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
    # Spelling and specific corrections
    mutate(last_name = gsub("DUNCAN JOHN.*", replacement = "DUNCAN", last_name)) %>% 
    mutate(last_name = gsub("JOHNSON HENRY.*", replacement = "JOHNSON", last_name)) %>% 
    mutate(last_name = gsub("BONO MACK.*", replacement = "BONO", last_name)) %>% 
    mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% 
    mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Lujan, Ben.*", FROM),gsub("Lujan, Ben.*", replacement = "LUJÁN", FROM), last_name)) %>% 
    mutate(last_name = gsub("MOORE CAPITO.*", replacement = "CAPITO", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Milkulski, Barbara", FROM), "MIKULSKI", last_name)) %>% 
    mutate(last_name = ifelse(grepl("GRESHAM BARRETT", last_name), "BARRETT", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Shelley Moore", FROM), "CAPITO", last_name)) %>% 
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Jackson",FROM)&grepl("She",FROM)&grepl("Lee",FROM), "JACKSON LEE", last_name)) %>% 
    mutate(last_name = ifelse( grepl("McMorris",FROM)&grepl("Rodgers",FROM)&grepl("Cathy",FROM), "McMORRIS RODGERS", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Michael",FROM)&grepl("Conaway",FROM), "CONAWAY", last_name)) %>%
    mutate(last_name = ifelse( grepl("Ben",FROM)&grepl("Nelson",FROM), "NELSON", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Beutler",FROM)&grepl("Herrera",FROM), "HERRERA BEUTLER", last_name)) %>%
    mutate(last_name = ifelse( grepl("Gillbrand",FROM), "GILLIBRAND", last_name)) %>%
    mutate(last_name = ifelse( grepl("Hillary|Hilary",FROM)&grepl("Rodham",FROM), "CLINTON", last_name)) %>%
    mutate(last_name = ifelse(grepl("Sandlin", FROM), "HERSETH SANDLIN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Murhpy", FROM), "MURPHY", last_name)) %>% 
    
    mutate(last_name = gsub("GONZALES", replacement = "GONZALEZ", last_name)) 
  
  return(data$last_name)
  
}

# Formats col_name (usually last_name) to similiar format as members$first_name
# Capitalizes letters appropriately and fixes common errors
formatFirstName <- function(data, col_name){
  
  data$first_name <- data[[col_name]]
  
  data %<>%
    mutate(first_name = stri_trans_totitle(first_name)) %>% 
    mutate(first_name = ifelse( grepl("Don",FROM)&grepl("Young",FROM), "Donald", first_name)) %>%
    mutate(first_name = ifelse( grepl("Andr",FROM)&grepl("Carson",FROM), "André", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM)&grepl("Thune",FROM), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM)&grepl("Rockefeller",FROM), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("Harold",FROM)&grepl("Rogers",FROM), "Harold", first_name)) %>%
    
    mutate(first_name = ifelse( grepl("James",FROM)&grepl("Sensenbrenner",FROM), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Richard",FROM)&grepl("Blumenthal",FROM), "Richard", first_name)) %>%
    mutate(first_name = ifelse( grepl("Bill",FROM)&grepl("Nelson",FROM), "Clarence", first_name)) %>%
    mutate(first_name = ifelse( grepl("Fred",FROM)&grepl("Upton",FROM), "Frederick", first_name)) %>%
    mutate(first_name = ifelse( grepl("Thad",FROM)&grepl("Cochran",FROM), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Kristen",FROM)&grepl("Gillibrand",FROM), "Kirsten", first_name)) %>%
    mutate(first_name = ifelse( grepl("C",FROM)&grepl("Ruppersberger",FROM), "Dutch", first_name)) %>%
    mutate(first_name = ifelse( grepl("Paul",FROM)&grepl("Gosar",FROM), "Paul", first_name)) %>%
    mutate(first_name = ifelse( grepl("Ros-Lehtinen",FROM), "Ileana", first_name)) %>%
    mutate(first_name = ifelse( grepl("Beutler",FROM)&grepl("Herrera",FROM), "Jaime", first_name)) %>%
    mutate(first_name = ifelse( grepl("Will|Bill",FROM)&grepl("Owens",FROM), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Butterfield",FROM)&grepl("G",FROM), "George", first_name)) %>%
    mutate(first_name = ifelse( grepl("G. K.",FROM), "G.K.", first_name)) %>%
    mutate(first_name = ifelse( grepl("Nelson",FROM)&grepl("Ben",FROM), "Earl", first_name)) %>%
    
    
    mutate(first_name = ifelse( grepl("Young",FROM)&grepl("C.W|C. W|CW",FROM), "Charles", first_name)) %>%
    mutate(first_name = ifelse( grepl("Jackson",FROM)&grepl("She",FROM)&grepl("Lee",FROM), "Sheila", first_name)) %>%
    mutate(first_name = ifelse( grepl("Gresham",FROM)&grepl("Barret",FROM), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Putnam",FROM)&grepl("Ad",FROM), "Adam", first_name)) %>%
    mutate(first_name = ifelse( grepl("Lind",FROM)&grepl("Graham",FROM), "Lindsey", first_name)) %>%
    
    mutate(first_name = gsub(pattern = "Christoher", replacement = "Christopher", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Hilllary|Hilary", replacement = "Hillary", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Babara", replacement = "Barbara", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Colin", replacement = "Collin", first_name)) %>% 
    mutate(first_name = gsub(pattern = "Melisssa", replacement = "Melissa", first_name)) %>% 
    
    
    mutate(first_name = gsub("Duncan John.*", replacement = "John", first_name)) %>% 
    mutate(first_name = gsub("Johnson Henry.*", replacement = "Henry", first_name))
  
  
  return(data$first_name)
  
}

# function will extract names found in members dataset from data$Summary column 

extractMemberName <- function(data, members, col_name){
  
  data$Summary <- data[[col_name]]
  
  
  #create full name variables with different combinations of first, common, middle, middle initial, and last name
  members$first_last <- paste(members$first_name, members$last_name, sep = " ")
  members$common_last <- paste(members$common_name, members$last_name, sep = " ")
  members$first_middle_last <- paste(members$first_name, members$middle_name, members$last_name, sep = " ")
  members$first_initial_last <- paste(members$first_name, members$middle_initial, members$last_name, sep = " ")
  members$common_middle_last <- paste(members$common_name, members$middle_name, members$last_name, sep = " ")
  members$common_initial_last <- paste(members$common_name, members$middle_initial, members$last_name, sep = " ")
  
  data$Summary <- gsub('\\.','', data$Summary)
  data$Summary <- gsub('(.*)\\.(.*)', "\\1\\2", data$Summary)
  data$Summary <- gsub('\\+', "", data$Summary)
  
  
  # create FROM2 varible extracting name from data$Summary
  data$FROM2 <- gsub(pattern = paste(c('.*(', paste(members$common_last[1:850], collapse = '|'), ').*'), collapse = ""),
                    replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # extracts  first_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # first_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # first_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) 
  
  
  data$first_name <- gsub("^(\\w+) .*", replacement = "\\1", data$FROM2)
  data$last_name <- gsub(".* (\\w+)$", replacement = '\\1', data$FROM2)
  
  
  data$first_name <- formatFirstName(data, 'first_name')
  data$last_name <- formatLastName(data, 'last_name')
  
  
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
                                  
                                  first_name, NA) ) %>% 
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
      last_name, NA)) %>% 
    mutate(FROM2 = ifelse( is.na(first_name) & is.na(last_name), NA, FROM2))
  
  
  
  return(data)
}

# Function may need small add ons or adjustments for new/different datasets
# Function will take comma separated names (e.g. Johnson, Ralph) from a specified column (usually FROM) 
# and create first_name and last_name columns in the dataframe. Typical call: getFirstLast.Comma(data,'FROM')
getFirstLast.Comma <- function(data, col_name){
  
  data$FROM <- data[[col_name]]

  #create full name variables with different combinations of first, common, middle, middle initial, and last name
  members$first_last <- paste(members$first_name, members$last_name, sep = " ")
  members$common_last <- paste(members$common_name, members$last_name, sep = " ")
  members$first_middle_last <- paste(members$first_name, members$middle_name, members$last_name, sep = " ")
  members$first_initial_last <- paste(members$first_name, members$middle_initial, members$last_name, sep = " ")
  members$common_middle_last <- paste(members$common_name, members$middle_name, members$last_name, sep = " ")
  members$common_initial_last <- paste(members$common_name, members$middle_initial, members$last_name, sep = " ")
  
  # create duplicate FROM column and preprocess
  #data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, Jr..|, III| III| II|, II| ll| IV|VI", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV|IV| ll| Jr,", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, M.C.,|, III,|, P.E.,| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$FROM2)
  data$FROM2 <- gsub(pattern = "Member, U.S", "U.S", data$FROM2)
  data$FROM2 <- gsub(pattern= "\\.\\.", replacement = ".", data$FROM2)
  data$FROM2 <- gsub("(REP|SEN)(.|- | - |. )", "", data$FROM2)
  
  #create variable for last name of the Sen/Rep
  data %<>%
    mutate(last = gsub(pattern = "^(\\w+|\\w+ \\w+|\\w+-\\w+)( ,|,).*", 
                       replacement = "\\1", x=FROM2)) %>% 
    mutate(last = gsub(pattern= "^(\\w')(\\w+)-(\\w+)( ,|,).*", replacement = "\\1\\2-\\3", last)) %>% 
    mutate(last = gsub(pattern= "^(\\w')(\\w+)( ,|,).*", replacement = "\\1\\2", last))
 
   # create variable for first name of Sen/Rep
  data %<>%
    mutate(first = gsub(pattern = ".*?(,|, |,\\w |,\\w. |,, \\w |, \\w. |, \\w.|, \\w+|,\\w+)(\\w+)( |.|).*",
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
    mutate(last_name = ifelse(grepl("HERSETH", last)|grepl('SANDLIN', last), "HERSETH SANDLIN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("HERSETH", last)|grepl('SANDLIN', last), "Stephanie", first_name))
  
  
   #Remove colums. Comment out for debugging
 # data <- subset(data, select = -c(first, last, first_last, FROM2))
  
  
  return(data)
}


