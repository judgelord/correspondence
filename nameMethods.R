# Formats col_name (usually last_name) to similiar format as members$last_name
# Capitalizes letters and fixes common errors 
formatLastName <- function(data, col_name){
  
  data$last_name <- data[[col_name]]
  
  data$last_name <- gsub("(^ |^  |^   |\n)", "", data$last_name)
  
  data %<>%
    mutate(last_name = str_to_upper(last_name)) %>% 
    # correct capitalization
    mutate(last_name = gsub("^MC", replacement = "Mc", last_name)) %>% 
    mutate(last_name = gsub("McEACHIN", replacement = "MCEACHIN", last_name, ignore.case = TRUE)) %>% 
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
    mutate(last_name = gsub("DENIS", replacement = "DENNIS", last_name)) %>% 
    mutate(last_name = gsub("DUNCAN JOHN.*", replacement = "DUNCAN", last_name)) %>% 
    mutate(last_name = gsub("JOHNSON HENRY.*", replacement = "JOHNSON", last_name)) %>% 
    mutate(last_name = gsub("BONO MACK.*", replacement = "BONO", last_name)) %>% 
    mutate(last_name = gsub(".*ROCKEFELLER.*|.*ROCKFELLER.*", replacement = "ROCKEFELLER", last_name)) %>% 
    mutate(last_name = gsub(".*SANDLIN.*", replacement = "HERSETH SANDLIN", last_name)) %>% 
   # mutate(last_name = ifelse(grepl("Lujan", FROM,ignore.case=TRUE)&grepl("Ben", FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>% 
   # mutate(last_name = ifelse( grepl("Lujan",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "LUJÁN", last_name)) %>%
    
     mutate(last_name = gsub("MOORE CAPITO.*", replacement = "CAPITO", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Milkulski, Barbara", FROM), "MIKULSKI", last_name)) %>% 
    mutate(last_name = ifelse(grepl("GRESHAM BARRETT", last_name,ignore.case=TRUE), "BARRETT", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Shelley Moore", FROM,ignore.case=TRUE), "CAPITO", last_name)) %>% 
    mutate(last_name = gsub(".*SCHULTZ.*", replacement = "WASSERMAN SCHULTZ", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM), "JACKSON LEE", last_name)) %>% 
    mutate(last_name = ifelse( (grepl("McMorris|Rodgers",FROM,ignore.case=TRUE))&grepl("Cathy|McMorris",FROM), "McMORRIS RODGERS", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Michael",FROM,ignore.case=TRUE)&grepl("Conaway",FROM,ignore.case=TRUE), "CONAWAY", last_name)) %>%
    mutate(last_name = ifelse( grepl("Ben",FROM)&grepl("Nelson",FROM), "NELSON", last_name)) %>% 
    mutate(last_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>%
    mutate(last_name = ifelse( grepl("Gillbrand",FROM,ignore.case=TRUE), "GILLIBRAND", last_name)) %>%
    mutate(last_name = ifelse( grepl("Hillary|Hilary",FROM,ignore.case=TRUE)&grepl("Rodham",FROM), "CLINTON", last_name)) %>%
    mutate(last_name = ifelse(grepl("Sandlin", FROM,ignore.case=TRUE), "HERSETH SANDLIN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Murhpy", FROM,ignore.case=TRUE), "MURPHY", last_name)) %>% 
    #mutate(last_name = ifelse( grepl("Linda",FROM,ignore.case=TRUE)&grepl("Sanchez",FROM,ignore.case=TRUE), "SÁNCHEZ", last_name)) %>%
    mutate(last_name = ifelse(grepl("Wasserman", FROM,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% 
    mutate(last_name = ifelse(grepl("McMorris", FROM,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% 
    mutate(last_name = ifelse(grepl("ROS-LEHTINEN", FROM,ignore.case=TRUE), "ROS-LEHTINEN", last_name)) %>% 
    mutate(last_name = ifelse(grepl(".SCLOSKY", FROM,ignore.case=TRUE), "VISCLOSKY", last_name)) %>% 
    mutate(last_name = ifelse(grepl("Guitierrez", FROM,ignore.case=TRUE), "GUTIERREZ", last_name)) %>% 
    
    mutate(last_name = ifelse(grepl("Roybal", last_name,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% 
    mutate(last_name = ifelse(grepl("RAHALL", last_name,ignore.case=TRUE), "RAHALL", last_name)) %>% 
    mutate(last_name = ifelse(grepl("BEUTLER", last_name,ignore.case=TRUE), "HERRERA BEUTLER", last_name)) %>% 
    
    mutate(last_name = gsub("GONZALES", replacement = "GONZALEZ", last_name))
  
  #data$last_name <- gsub("(^ |^  |^   |\n)", "", data$last_name)
  
  

  
  return(data$last_name)
  
}

# Formats col_name (usually last_name) to similiar format as members$first_name
# Capitalizes letters appropriately and fixes common errors
formatFirstName <- function(data, col_name){
  
  data$first_name <- data[[col_name]]
  #data %<>% mutate(FROM = ifelse("FROM2" %in% names(data), FROM2, FROM))
  
  data %<>%
    mutate(first_name = stri_trans_totitle(first_name)) %>% 
    mutate(first_name = ifelse( grepl("Don",FROM,ignore.case=TRUE)&grepl("Young",FROM,ignore.case=TRUE), "Donald", first_name)) %>%
    #mutate(first_name = ifelse( grepl("Andr",FROM,ignore.case=TRUE)&grepl("Carson",FROM,ignore.case=TRUE), "André", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Thune",FROM,ignore.case=TRUE), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("John",FROM,ignore.case=TRUE)&grepl("Rockefeller",FROM,ignore.case=TRUE), "John", first_name)) %>%
    mutate(first_name = ifelse( grepl("Harold",FROM,ignore.case=TRUE)&grepl("Rogers",FROM,ignore.case=TRUE), "Harold", first_name)) %>%
    
    mutate(first_name = ifelse( grepl("James",FROM,ignore.case=TRUE)&grepl("Sensenbrenner",FROM,ignore.case=TRUE), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Richard",FROM,ignore.case=TRUE)&grepl("Blumenthal",FROM,ignore.case=TRUE), "Richard", first_name)) %>%
    mutate(first_name = ifelse( grepl("Bill",FROM,ignore.case=TRUE)&grepl("Nelson",FROM,ignore.case=TRUE), "Clarence", first_name)) %>%
    mutate(first_name = ifelse( grepl("Fred",FROM,ignore.case=TRUE)&grepl("Upton",FROM,ignore.case=TRUE), "Frederick", first_name)) %>%
    mutate(first_name = ifelse( grepl("Thad",FROM,ignore.case=TRUE)&grepl("Cochran",FROM,ignore.case=TRUE), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Kristen",FROM,ignore.case=TRUE)&grepl("Gillibrand",FROM,ignore.case=TRUE), "Kirsten", first_name)) %>%
    mutate(first_name = ifelse( grepl("C",FROM,ignore.case=TRUE)&grepl("Ruppersberger",FROM,ignore.case=TRUE), "Dutch", first_name)) %>%
    mutate(first_name = ifelse( grepl("Paul",FROM,ignore.case=TRUE)&grepl("Gosar",FROM,ignore.case=TRUE), "Paul", first_name)) %>%
    mutate(first_name = ifelse( grepl("Ros-Lehtinen",FROM,ignore.case=TRUE), "Ileana", first_name)) %>%
    mutate(first_name = ifelse( grepl("Beutler",FROM,ignore.case=TRUE)&grepl("Herrera",FROM,ignore.case=TRUE), "Jaime", first_name)) %>%
    mutate(first_name = ifelse( grepl("Will|Bill",FROM,ignore.case=TRUE)&grepl("Owens",FROM,ignore.case=TRUE), "William", first_name)) %>%
    mutate(first_name = ifelse( grepl("Butterfield",FROM,ignore.case=TRUE)&grepl("G",FROM,ignore.case=TRUE), "George", first_name)) %>%
    mutate(first_name = ifelse( grepl("G. K.",FROM,ignore.case=TRUE), "G.K.", first_name)) %>%
    mutate(first_name = ifelse( grepl("Nelson",FROM,ignore.case=TRUE)&grepl("Ben",FROM,ignore.case=TRUE), "Earl", first_name)) %>%
    #mutate(first_name = ifelse( grepl("Carson",FROM,ignore.case=TRUE)&grepl("Andr",FROM,ignore.case=TRUE), "André", first_name)) %>%
   #mutate(first_name = ifelse( grepl("Griv",FROM,ignore.case=TRUE)&grepl("Raul",FROM,ignore.case=TRUE), "Raúl", first_name)) %>%
    mutate(first_name = ifelse( grepl("Scott",FROM,ignore.case=TRUE)&grepl("Bobby",FROM,ignore.case=TRUE), "Bob", first_name)) %>%
    
    
    mutate(first_name = ifelse( grepl("Young",FROM,ignore.case=TRUE)&grepl("C.W|C. W|CW",FROM,ignore.case=TRUE), "Charles", first_name)) %>%
    mutate(first_name = ifelse( grepl("Jackson",FROM,ignore.case=TRUE)&grepl("She",FROM)&grepl("Lee",FROM,ignore.case=TRUE), "Sheila", first_name)) %>%
    mutate(first_name = ifelse( grepl("Gresham",FROM,ignore.case=TRUE)&grepl("Barret",FROM,ignore.case=TRUE), "James", first_name)) %>%
    mutate(first_name = ifelse( grepl("Putnam",FROM,ignore.case=TRUE)&grepl("Ad",FROM,ignore.case=TRUE), "Adam", first_name)) %>%
    mutate(first_name = ifelse( grepl("Lind",FROM,ignore.case=TRUE)&grepl("Graham",FROM,ignore.case=TRUE), "Lindsey", first_name)) %>%
    mutate(first_name = ifelse( grepl("SERRANO",FROM,ignore.case=TRUE)&grepl("Jos",FROM,ignore.case=TRUE), "José", first_name)) %>%
    
    mutate(first_name = gsub(pattern = "Christoher", replacement = "Christopher", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub(pattern = "Hilllary|Hilary", replacement = "Hillary", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub(pattern = "Babara", replacement = "Barbara", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub(pattern = "Colin", replacement = "Collin", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub(pattern = "Melisssa", replacement = "Melissa", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub(pattern = "Denis", replacement = "Dennis", first_name,ignore.case=TRUE)) %>% 
    mutate(first_name = gsub("Eliott", replacement = "Eliot", first_name)) %>% 
    mutate(first_name = gsub("Brain", replacement = "Brian", first_name)) %>% 
    
    mutate(first_name = gsub("Duncan John.*", replacement = "John", first_name)) %>% 
    mutate(first_name = gsub("Johnson Henry.*", replacement = "Henry", first_name))
  
  data$first_name <- gsub("(^ |^  |^   |\n)", "", data$first_name)
  
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
  
  # code commented out was preprocessing from getFirstLast.Comma() method. Might be useful later. 
  # data$Summary <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$Summary)
  # data$Summary <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\), Rep.",
  #                    replacement = "", data$Summary)
  data$Summary <- gsub('\\.','', data$Summary)
  data$Summary <- gsub('(.*)\\.(.*)', "\\1\\2", data$Summary)
  data$Summary <- gsub('\\+', "", data$Summary)
  data$Summary <- gsub('\\"Bobby\\"|\\"Buddy\\"|\\"GT\\"', "", data$Summary)
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  data$Summary <- gsub("(^ |^  |^   |\n)", "", data$Summary)
  
  
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
  
  
 data %<>%
   mutate(last_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "Catherine", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Lujan", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Lujan", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "Michelle", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Deb", Summary,ignore.case = TRUE)&grepl("Wasserman|Schultz", Summary,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Deb",Summary,ignore.case=TRUE)&grepl("Wasserman|Schultz",Summary,ignore.case=TRUE), "Debbie", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Mario|Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Mario", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Mario", first_name)) %>% 
   mutate(first_name = ifelse(grepl("Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Lincoln", first_name)) 
 
 # Fix specific common errors
 data %<>%
   mutate(last_name = ifelse(grepl("HERSETH", Summary, ignore.case = TRUE )|grepl('SANDLIN', Summary, ignore.case = TRUE), "HERSETH SANDLIN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("HERSETH", Summary,ignore.case = TRUE)|grepl('SANDLIN', Summary,ignore.case = TRUE), "Stephanie", first_name)) %>%
   mutate(last_name = ifelse(grepl("PAULSEN", Summary, ignore.case = TRUE )&grepl('Erik', Summary, ignore.case = TRUE), "PAULSEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("PAULSEN", Summary,ignore.case = TRUE)&grepl('Erik', Summary,ignore.case = TRUE), "Erik", first_name)) %>% 
   mutate(last_name = ifelse(grepl("CONYERS", Summary, ignore.case = TRUE )&grepl('John', Summary, ignore.case = TRUE), "CONYERS", last_name)) %>% 
   mutate(first_name = ifelse(grepl("CONYERS", Summary,ignore.case = TRUE)&grepl('John', Summary,ignore.case = TRUE), "John", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Ben|E.B|E B", Summary,ignore.case = TRUE)& grepl('NELSON', Summary,ignore.case = TRUE), "NELSON", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Ben|E.B|E B", Summary,ignore.case = TRUE)& grepl('NELSON', Summary,ignore.case = TRUE), "Ben", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Casey", Summary,ignore.case = TRUE)& grepl('Rob|Bob|Jr', Summary,ignore.case = TRUE), "CASEY", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Casey", Summary,ignore.case = TRUE)& grepl('Rob|Bob|Jr', Summary,ignore.case = TRUE), "Robert", first_name)) %>% 
   mutate(last_name = ifelse(grepl("RUPPERSBERGER", Summary,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% 
   mutate(first_name = ifelse(grepl("RUPPERSBERGER", Summary,ignore.case = TRUE), "Dutch", first_name)) %>% 
   mutate(last_name = ifelse(grepl("KRATOVIL", Summary,ignore.case = TRUE), "KRATOVIL", last_name)) %>% 
   mutate(first_name = ifelse(grepl("KRATOVIL", Summary,ignore.case = TRUE), "Frank", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Sheila", Summary,ignore.case = TRUE)&grepl("JACKSON", Summary,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Sheila", Summary,ignore.case = TRUE)&grepl("JACKSON", Summary,ignore.case = TRUE), "Sheila", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Paul", Summary,ignore.case = TRUE)&grepl("KIRK", Summary,ignore.case = TRUE), "KIRK", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Paul", Summary,ignore.case = TRUE)&grepl("KIRK", Summary,ignore.case = TRUE), "Paul", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE), "GRESHAM", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE)&grepl("James", Summary,ignore.case = TRUE), "Paul", first_name)) %>% 
   mutate(last_name = ifelse(grepl("John", Summary,ignore.case = TRUE)&grepl("Hall", Summary,ignore.case = TRUE), "HALL", last_name)) %>% 
   mutate(first_name = ifelse(grepl("John", Summary,ignore.case = TRUE)&grepl("Hall", Summary,ignore.case = TRUE), "John", first_name)) %>% 
   mutate(last_name = ifelse(grepl("ENBRENNER", Summary,ignore.case = TRUE), "SENSENBRENNER", last_name)) %>% 
   mutate(first_name = ifelse(grepl("ENBRENNER", Summary,ignore.case = TRUE), "Frank", first_name)) %>% 
   mutate(last_name = ifelse(grepl("FRELINGHUY", Summary,ignore.case = TRUE), "FRELINGHUYSEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("FRELINGHUY", Summary,ignore.case = TRUE), "Rodney", first_name)) %>% 
   mutate(last_name = ifelse(grepl("LARRICK", Summary,ignore.case = TRUE), "LARSEN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("LARRICK", Summary,ignore.case = TRUE), "Rick", first_name)) %>% 
   mutate(last_name = ifelse(grepl("BOUSTANY", Summary,ignore.case = TRUE), "BOUSTANY", last_name)) %>% 
   mutate(first_name = ifelse(grepl("BOUSTANY", Summary,ignore.case = TRUE), "Charles", first_name)) %>% 
   mutate(last_name = ifelse(grepl("King", Summary,ignore.case = TRUE)&grepl("Jr", Summary,ignore.case = TRUE), "KING", last_name)) %>% 
   mutate(first_name = ifelse(grepl("King", Summary,ignore.case = TRUE)&grepl("jr", Summary,ignore.case = TRUE), "Angus", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("Andr", Summary,ignore.case = TRUE)&grepl("Carson", Summary,ignore.case = TRUE), "CARSON", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("Andr", Summary,ignore.case = TRUE)&grepl("Carson", Summary,ignore.case = TRUE), "André", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("Jos", Summary,ignore.case = TRUE)&grepl("Serrano", Summary,ignore.case = TRUE), "SERRANO", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("Jos", Summary,ignore.case = TRUE)&grepl("Serrano", Summary,ignore.case = TRUE), "José", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("GRIJALVA", Summary,ignore.case = TRUE), "GRIJALVA", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("GRIJALVA", Summary,ignore.case = TRUE), "Raúl", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("Nydia", Summary,ignore.case = TRUE), "VELÁZQUEZ", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("Nydia", Summary,ignore.case = TRUE), "Nydia", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("LABRADOR", Summary,ignore.case = TRUE), "Raúl", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("LABRADOR", Summary,ignore.case = TRUE), "Raúl", first_name)) %>% 
   mutate(last_name = ifelse(grepl("HINOJOSA", Summary,ignore.case = TRUE), "HINOJOSA", last_name)) %>% 
   #mutate(first_name = ifelse(grepl("HINOJOSA", Summary,ignore.case = TRUE), "Rubén", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Payne", Summary,ignore.case = TRUE)&grepl("Don", Summary,ignore.case = TRUE), "PAYNE", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Payne", Summary,ignore.case = TRUE)&grepl("Don", Summary,ignore.case = TRUE), "Donald", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Pascrell", Summary,ignore.case = TRUE)&grepl("Bill", Summary,ignore.case = TRUE), "PASCRELL", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Pascrell", Summary,ignore.case = TRUE)&grepl("Bill", Summary,ignore.case = TRUE), "BILL", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Tony", Summary,ignore.case = TRUE)&grepl("C.rdenas", Summary,ignore.case = TRUE), "CÁRDENAS", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Tony", Summary,ignore.case = TRUE)&grepl("C.rdenas", Summary,ignore.case = TRUE), "Tony", first_name)) %>% 
  # mutate(last_name = ifelse(grepl("Luis", Summary,ignore.case = TRUE)&grepl("GUTI.RREZ", Summary,ignore.case = TRUE), "GUTIÉRREZ", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("Luis", Summary,ignore.case = TRUE)&grepl("GUTI.RREZ", Summary,ignore.case = TRUE), "Luis", first_name)) %>% 
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
   mutate(last_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "CORTEZ MASTO", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Matso|Masto", Summary,ignore.case = TRUE), "Catherine", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Lujan|Michelle", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Lujan|Michelle", Summary,ignore.case = TRUE)&grepl("Grishman|Grisham", Summary,ignore.case=TRUE), "Michelle", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Donovan||Donavan", Summary,ignore.case = TRUE)&grepl("Dan", Summary,ignore.case=FALSE), "DONOVAN", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Donovan|Donavan", Summary,ignore.case = TRUE)&grepl("Dan", Summary,ignore.case=FALSE), "Daniel", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Randy| j.|james", Summary,ignore.case = TRUE)&grepl("Forbes", Summary,ignore.case=TRUE), "FORBES", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Randy| j.|james", Summary,ignore.case = TRUE)&grepl("Forbes", Summary,ignore.case=TRUE), "James", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Mario|Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "DIAZ-BALART", last_name)) %>% 
   mutate(first_name = ifelse(grepl("Mario", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Mario", first_name)) %>% 
   mutate(first_name = ifelse(grepl("Lincoln", Summary,ignore.case = TRUE)&grepl("Diaz|Balart", Summary,ignore.case=TRUE), "Lincoln", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Gresham", Summary,ignore.case = TRUE)&grepl("Barret", Summary,ignore.case=TRUE), "BARRETT", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Gresham",Summary,ignore.case=TRUE)&grepl("Barret",Summary,ignore.case=TRUE), "James", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Deb", Summary,ignore.case = TRUE)&grepl("Wasserman|Schultz", Summary,ignore.case=TRUE), "WASSERMAN SCHULTZ", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Deb",Summary,ignore.case=TRUE)&grepl("Wasserman|Schultz",Summary,ignore.case=TRUE), "Debbie", first_name)) %>% 
   mutate(last_name = ifelse(grepl("Kristen|Kirsten", Summary,ignore.case = TRUE)&grepl("Gil", Summary,ignore.case=TRUE), "GILLIBRAND", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Kristen|Kirsten",Summary,ignore.case=TRUE)&grepl("Gil",Summary,ignore.case=TRUE), "Kirsten", first_name)) %>% 
   mutate(last_name = ifelse( grepl("Jo ",Summary,ignore.case=TRUE)&grepl("Davis",Summary,ignore.case=TRUE), "DAVIS", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Jo ",Summary,ignore.case=TRUE)&grepl("Davis",Summary,ignore.case=TRUE), "Jo", first_name)) %>% 
   mutate(last_name = ifelse( grepl("Waite|Brown",Summary,ignore.case=TRUE)&grepl("Ginny|Virginia",Summary,ignore.case=TRUE), "BROWN-WAITE", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Waite|Brown",Summary,ignore.case=TRUE)&grepl("Ginny|Virginia",Summary,ignore.case=TRUE), "Virginia", first_name)) %>% 
   mutate(last_name = ifelse( grepl("Jo",Summary,ignore.case=TRUE)&grepl("Emerson",Summary,ignore.case=TRUE), "EMERSON", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Jo",Summary,ignore.case=TRUE)&grepl("Emerson",Summary,ignore.case=TRUE), "Jo", first_name)) %>% 
   mutate(last_name = ifelse( grepl("Shelley|Moore",Summary,ignore.case=TRUE)&grepl("Capito",Summary,ignore.case=TRUE), "CAPITO", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Jo",Summary,ignore.case=TRUE)&grepl("Emerson",Summary,ignore.case=TRUE), "Shelley", first_name))%>% 
   mutate(last_name = ifelse( grepl("McMorris|Rodgers",Summary,ignore.case=TRUE)&grepl("Cathy|McMorris",Summary,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% 
   mutate(first_name = ifelse( grepl("McMorris|Rodgers",Summary,ignore.case=TRUE)&grepl("Cathy|McMorris",Summary,ignore.case=TRUE), "Cathy", first_name)) %>% 

   mutate(last_name = ifelse( grepl("Rounds",Summary,ignore.case=TRUE)&grepl("Marion|Mike|Michael",Summary,ignore.case=TRUE), "ROUNDS", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Rounds",Summary,ignore.case=TRUE)&grepl("Marion|Mike|Michael",Summary,ignore.case=TRUE), "Marion", first_name))%>% 
   mutate(last_name = ifelse( grepl("Panetta",Summary,ignore.case=TRUE)&grepl("Jim|James",Summary,ignore.case=TRUE), "PANETTA", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Panetta",Summary,ignore.case=TRUE)&grepl("Jim|James",Summary,ignore.case=TRUE), "James", first_name))%>% 
   mutate(last_name = ifelse( grepl("Roybal",Summary,ignore.case=TRUE)&grepl("Allard",Summary,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Roybal",Summary,ignore.case=TRUE)&grepl("Allard",Summary,ignore.case=TRUE), "Lucille", first_name))%>% 
   mutate(last_name = ifelse( grepl("Clay",Summary,ignore.case=TRUE)&grepl("Lacy|William|Bill",Summary,ignore.case=TRUE), "CLAY", last_name)) %>% 
   mutate(first_name = ifelse( grepl("Clay",Summary,ignore.case=TRUE)&grepl("Lacy|William|Bill",Summary,ignore.case=TRUE), "William", first_name))
 
 
 

 
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
  data$FROM2 <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$FROM)
  data$FROM2 <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$FROM2)
  data$FROM2 <- gsub(pattern = "Member, U.S", "U.S", data$FROM2)
  data$FROM2 <- gsub(pattern= "\\.\\.", replacement = ".", data$FROM2)
  data$FROM2 <- gsub("(REP|SEN)(.|- | - |. )", "", data$FROM2)
  data$FROM2 <- gsub("^ ", "", data$FROM2)
  
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
    mutate(last_name = ifelse(grepl("HERSETH", last, ignore.case = TRUE )|grepl('SANDLIN', last, ignore.case = TRUE), "HERSETH SANDLIN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("HERSETH", last,ignore.case = TRUE)|grepl('SANDLIN', last,ignore.case = TRUE), "Stephanie", first_name)) %>%
    mutate(last_name = ifelse(grepl("PAULSEN", last, ignore.case = TRUE )&grepl('Erik', last, ignore.case = TRUE), "PAULSEN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("PAULSEN", last,ignore.case = TRUE)&grepl('Erik', last,ignore.case = TRUE), "Erik", first_name)) %>% 
    mutate(last_name = ifelse(grepl("CONYERS", last, ignore.case = TRUE )&grepl('John', last, ignore.case = TRUE), "CONYERS", last_name)) %>% 
    mutate(first_name = ifelse(grepl("CONYERS", last,ignore.case = TRUE)&grepl('John', last,ignore.case = TRUE), "John", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Ben|E.B|E B", FROM2,ignore.case = TRUE)& grepl('NELSON', FROM2,ignore.case = TRUE), "NELSON", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Ben|E.B|E B", first_last,ignore.case = TRUE)& grepl('NELSON', first_last,ignore.case = TRUE), "Ben", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "CASEY", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Casey", FROM2,ignore.case = TRUE)& grepl('Rob|Bob|Jr', FROM2,ignore.case = TRUE), "Robert", first_name)) %>% 
    mutate(last_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "RUPPERSBERGER", last_name)) %>% 
    mutate(first_name = ifelse(grepl("RUPPERSBERGER", FROM2,ignore.case = TRUE), "Dutch", first_name)) %>% 
    mutate(last_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "KRATOVIL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("KRATOVIL", FROM2,ignore.case = TRUE), "Frank", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON", FROM2,ignore.case = TRUE), "JACKSON LEE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Sheila", FROM2,ignore.case = TRUE)&grepl("JACKSON", FROM2,ignore.case = TRUE), "Sheila", first_name)) %>% 
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
    mutate(first_name = ifelse(grepl("Andr", FROM2,ignore.case = TRUE)&grepl("Carson", FROM2,ignore.case = TRUE), "André", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE), "SERRANO", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Jos", FROM2,ignore.case = TRUE)&grepl("Serrano", FROM2,ignore.case = TRUE), "José", first_name)) %>% 
    # mutate(last_name = ifelse(grepl("GRIJALVA", FROM2,ignore.case = TRUE), "GRIJALVA", last_name)) %>% 
    # mutate(first_name = ifelse(grepl("GRIJALVA", FROM2,ignore.case = TRUE), "Raúl", first_name)) %>% 
   # mutate(last_name = ifelse(grepl("Nydia", FROM2,ignore.case = TRUE), "VELÁZQUEZ", last_name)) %>% 
  # mutate(first_name = ifelse(grepl("Nydia", FROM2,ignore.case = TRUE), "Nydia", first_name)) %>% 
  #  mutate(last_name = ifelse(grepl("LABRADOR", FROM2,ignore.case = TRUE), "Raúl", last_name)) %>% 
  #  mutate(first_name = ifelse(grepl("LABRADOR", FROM2,ignore.case = TRUE), "Raúl", first_name)) %>% 
    mutate(last_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "HINOJOSA", last_name)) %>% 
   # mutate(first_name = ifelse(grepl("HINOJOSA", FROM2,ignore.case = TRUE), "Rubén", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "PAYNE", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Payne", FROM2,ignore.case = TRUE)&grepl("Don", FROM2,ignore.case = TRUE), "Donald", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "PASCRELL", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Pascrell", FROM2,ignore.case = TRUE)&grepl("Bill", FROM2,ignore.case = TRUE), "BILL", first_name)) %>% 
   # mutate(last_name = ifelse(grepl("Tony", FROM2,ignore.case = TRUE)&grepl("C.rdenas", FROM2,ignore.case = TRUE), "CÁRDENAS", last_name)) %>% 
  #  mutate(first_name = ifelse(grepl("Tony", FROM2,ignore.case = TRUE)&grepl("C.rdenas", FROM2,ignore.case = TRUE), "Tony", first_name)) %>% 
  #  mutate(last_name = ifelse(grepl("Luis", FROM2,ignore.case = TRUE)&grepl("GUTI.RREZ", FROM2,ignore.case = TRUE), "GUTIÉRREZ", last_name)) %>% 
  #  mutate(first_name = ifelse(grepl("Luis", FROM2,ignore.case = TRUE)&grepl("GUTI.RREZ", FROM2,ignore.case = TRUE), "Luis", first_name)) %>% 
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
    mutate(last_name = ifelse(grepl("Lujan|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "LUJAN GRISHAM", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Lujan|Michelle", FROM2,ignore.case = TRUE)&grepl("Grishman|Grisham", FROM2,ignore.case=TRUE), "Michelle", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Donovan||Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "DONOVAN", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Donovan|Donavan", FROM2,ignore.case = TRUE)&grepl("Dan", FROM2,ignore.case=FALSE), "Daniel", first_name)) %>% 
    mutate(last_name = ifelse(grepl("Randy| j.|james", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "FORBES", last_name)) %>% 
    mutate(first_name = ifelse(grepl("Randy| j.|james", FROM2,ignore.case = TRUE)&grepl("Forbes", FROM2,ignore.case=TRUE), "James", first_name)) %>% 
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
    mutate(last_name = ifelse( grepl("Shelley|Moore",FROM2,ignore.case=TRUE)&grepl("Capito",FROM2,ignore.case=TRUE), "CAPITO", last_name)) %>% 

    mutate(last_name = ifelse( grepl("McMorris|Rodgers",FROM2,ignore.case=TRUE)&grepl("Cathy|McMorris",FROM2,ignore.case=TRUE), "McMORRIS RODGERS", last_name)) %>% 
    mutate(first_name = ifelse( grepl("McMorris|Rodgers",FROM2,ignore.case=TRUE)&grepl("Cathy|McMorris",FROM2,ignore.case=TRUE), "Cathy", first_name)) %>% 
    mutate(last_name = ifelse( grepl("Rounds",FROM2,ignore.case=TRUE)&grepl("Marion|Mike|Michael",FROM2,ignore.case=TRUE), "ROUNDS", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Rounds",FROM2,ignore.case=TRUE)&grepl("Marion|Mike|Michael",FROM2,ignore.case=TRUE), "Marion", first_name))%>% 
    mutate(last_name = ifelse( grepl("Panetta",FROM2,ignore.case=TRUE)&grepl("Jim|James",FROM2,ignore.case=TRUE), "PANETTA", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Panetta",FROM2,ignore.case=TRUE)&grepl("Jim|James",FROM2,ignore.case=TRUE), "James", first_name))%>% 
    mutate(last_name = ifelse( grepl("Roybal",FROM2,ignore.case=TRUE)&grepl("Allard",FROM2,ignore.case=TRUE), "ROYBAL-ALLARD", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Roybal",FROM2,ignore.case=TRUE)&grepl("Allard",FROM2,ignore.case=TRUE), "Lucille", first_name))%>% 
    mutate(last_name = ifelse( grepl("Clay",FROM2,ignore.case=TRUE)&grepl("Lacy|William|Bill",FROM2,ignore.case=TRUE), "CLAY", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Clay",FROM2,ignore.case=TRUE)&grepl("Lacy|William|Bill",FROM2,ignore.case=TRUE), "William", first_name))%>% 
    mutate(last_name = ifelse( grepl("Todd",FROM2,ignore.case=TRUE)&grepl("Akin",FROM2,ignore.case=TRUE), "AKIN", last_name)) %>% 
    mutate(first_name = ifelse( grepl("Todd",FROM2,ignore.case=TRUE)&grepl("Akin",FROM2,ignore.case=TRUE), "Todd", first_name))

    
  
  
    
    
  
  
  
   #Remove colums. Comment out for debugging
 # data <- subset(data, select = -c(first, last, first_last, FROM2))
  
  
  return(data)
}


