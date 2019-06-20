# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "FHFA" # for testing

clean <- function(file.name) {
   data <- gs_title(file.name) %>% gs_read() # get data
  

  # create ID variable
  data$ID <- c(1:nrow(data))
  
  
  # create agency column
  data$agency <- file.name
  
  #Filters out headings 
  data %<>%
    filter( ! FROM %in% c("Sender", "Originator")) %>%
    filter( ! SUBJECT %in% ("Title"))
  
  # Format date, year, Congress, member name etc. 
  is.na(data$`Modified O`) <- data$`Modified O` == "N/A(NAR)"
  is.na(data$`Modified O`) <- data$`Modified O` == "Sens. Schumer, Brown, Casey & Menendez"
  data %<>%
    mutate(DATE = ifelse(is.na(DATE), `Modified O`, DATE))
  #data$DATE <- data$`Modified O`
  data$DATE %<>% as.Date("%m/%d/%Y")
  
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  #Chamber
  data %<>% 
    mutate(chamber = ifelse(grepl("Senate|SENATE|Senator|SENATOR|MAJORITY LEADER", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("House|HOUSE|Representative|REPRESENTATIVE|REPRESENTATIAVE|^REP |Congressman|Congresswoman|Reps.", FROM), "House", chamber))
 
#Inserting ";"
  data %<>%
    mutate(FROM = str_replace(FROM, "Senators Hagel, Sununu, Dole & Martinez", "Senators Hagel; Sununu; Dole & Martinez")) %>%
    mutate(FROM = str_replace(FROM, "Reps. Hoyer, Davis, etal", "Reps. Hoyer; Davis, etal")) %>%
    mutate(FROM = str_replace(FROM, "Rep. Bachus, Bachmann, Blunt, Hensarling, Feeney, Garrett, Price, Pence, Biggert, Royce, Blackburn, Neugebauer, McHenry, Roskam, McCotter, Barrett, David, Marchant, Campbell, Walberg, Kline, Brown-White, Paul, Manzullo, Broun, Musgrave, Poe, etal",
                              "Rep. Bachus; Bachmann; Blunt; Hensarling; Feeney; Garrett; Price; Pence; Biggert; Royce; Blackburn; Neugebauer; McHenry; Roskam; McCotter; Barrett; David; Marchant; Campbell; Walberg; Kline; Brown-White; Paul; Manzullo; Broun; Musgrave; Poe, etal")) %>%
    mutate(FROM = str_replace(FROM, "Goodlatte, Davis, Boucher etal", "Goodlatte; Davis; Boucher, etal"))
  ###############    
#Splits Rows with multiple authors
  data %<>%
    mutate(FROM = str_split(FROM, ";|\\&| and ")) %>%
    unnest(FROM)

  #Removes unneeded rows
  data %<>%
    filter( ! FROM %in% ("etal"))
  #Comments errors for non members
  data %<>%
    mutate(ERROR = ifelse(str_detect(FROM, "Lockhart, James, Director of FHFA"), "FHFA Director", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Pratt, Leonard"), "Not Member", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "DeMarco, Edward, Acting Director"), "Acting Director", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Schroeder, Jeannine, Senior Strategic Planning & Management Specialist"), "Senior Strategic Planning & Management Specialist", ERROR)) %>%
    mutate(ERROR = ifelse(str_detect(FROM, "Kelley, Eric, Associate Director for Internal Audit"), "Associate Director for Internal Audit", ERROR)) %>%
    mutate(NOTES = ifelse(str_detect(FROM, "Congressional Aide| Aide| aid"), "Congressional Aide", NOTES))
  
  #Fixes some mispelled names
  data %<>%
    mutate(FROM = str_replace(FROM, "Polls, Jared", "Polis, Jared")) %>%
    mutate(FROM = str_replace(FROM, "Kyi, Jon", "Kyl, Jon")) %>%
    mutate(FROM = str_replace(FROM, "Schultz,", "Schultz, Debbie")) %>%
    mutate(FROM = str_replace(FROM, "Walters, Maxine", "Waters, Maxine")) %>%
    mutate(FROM = str_replace(FROM, "Klien, Ron", "Klein, Ron")) %>%
    mutate(FROM = str_replace(FROM, "Giffords, M.C.", "Giffords, Gabrielle")) %>%
    mutate(FROM = str_replace(FROM, "Manzullo, Daniel", "Manzullo, Donald")) %>%
    mutate(FROM = str_replace(FROM, "Velazques, Nydia", "Velazquez, Nydia")) %>%
    mutate(FROM = str_replace(FROM, "Akerman, Gary", "Ackerman, Gary")) %>%
    mutate(FROM = str_replace(FROM, "Perimutter, Ed", "Perlmutter, Ed")) %>%
    mutate(FROM = str_replace(FROM, "DeFauro, Rosa", "DeLauro, Rosa")) %>%
    mutate(FROM = str_replace(FROM, "Hirano, Mazie", "Hirono, Mazie")) %>%
    mutate(FROM = str_replace(FROM, "Napolitano,", "Napolitano, Grace")) %>%
    mutate(FROM = str_replace(FROM, "Buchson, Larry", "Bucshon, Larry")) %>%
    mutate(FROM = str_replace(FROM, "Johnson, Bernice", "Johnson, Eddie")) %>%
    mutate(FROM = str_replace(FROM, "Moran, lames", "Moran, James")) %>%
    mutate(FROM = str_replace(FROM, "Christopher J. Dodd", "Dodd J. Christopher")) %>%
    mutate(FROM = str_replace(FROM, "Charles E. Schumer", "Schumer E. Charles")) %>%
    mutate(FROM = str_replace(FROM, "Kanjorski", "KANJORSKI, Paul")) %>%
    mutate(FROM = str_replace(FROM, "Bachman, Michele,", "BACHMANN, Michele")) %>%
    mutate(FROM = str_replace(FROM, "Russell D. Feingold", "Feingold, Russell D."))
  
  #Filter while working, comment out
  #data %<>%
   # filter( ! FROM %in% c("Franks, Trent, Congressman of Arizona", "Lockhart, James, Director of FHFA",
    #     "Pratt, Leonard", "DeMarco, Edward, Acting Director",
     #   "Schroeder, Jeannine, Senior Strategic Planning & Management Specialist",
      #   "Kelley, Eric, Associate Director for Internal Audit", "Brereton, Peter, Associate Director for Congressional Affairs",
      #  "Lockhart, James", "Marshall, Donald (OFHEO Contractor)", "Lenoir, Simuel"))

  #Clean to run getFirstLast.Comma
  data %<>%
    mutate(FROM = (str_remove_all(FROM, ", Congresswoman|, Congressman|, Senator|, Representative|, Chairman|Senator |, Senate.*|Congresswoman |Congressman|, Rep|, etal|, Represenative|Senators |Rep. |Reps. |, US Senator|resenative")))
  
  ################
  
  #Matches to member data
  data <- getFirstLast.Comma(data, 'FROM')

  #Filters for all rows that still don't match
  FROMunamed <- data %>%
    filter(is.na(last_name))
  
  FROMunamed %<>% select(ID, DATE, FROM, first_name, last_name, SUBJECT, everything())
  
  #Filters for names still unmatched
  Unfoundnames <- data %>%
    filter(is.na(last_name)) %>%
    extractMemberName(members = members, col_name = "SUBJECT") %>% 
    select(ID, DATE, FROM, first_name, last_name, SUBJECT, everything())

  #Drops duplicate NAs when rejoined  
  Unfoundnames %<>%
    drop_na(last_name)
  
  #Rejoins data
  data %<>%
    full_join(Unfoundnames)
  
  data %<>% select(ID, DATE, FROM, first_name, last_name, SUBJECT, chamber, everything())
  
  data %<>%
    mutate(Blank = is.na(FROM) & is.na(SUBJECT)) %>%
    filter(! Blank)
  
  #Unmatched
  unmatched <- data %>%
    filter(is.na(last_name))
  #John, Cook could be John Cooksey; however Cooksey did not serve in 2011
  # Mike, Fitzgerald might be Mike, Fitzpatrick
  
  Foundnames <- data %>%
    drop_na(last_name)
  
  data %<>%
    mutate(NOTES = ifelse(str_detect(FROM, "32 more congressmen"), "Multiple unnamed Members", NOTES))
  
  #Format last name and put in last_name  
  data %<>%
    mutate(last_name = ifelse(! str_detect(FROM, "\\,") & is.na(last_name), formatLastName(data, 'FROM'), last_name))
  
  #data %<>%
   # mutate(last_name = ifelse(! str_detect(FROM, "\\,") & is.na(last_name), FROM, last_name))
  
  #Check after run through merge
  #Unfoundnames <- d %>%
  #filter(is.na(bioname))
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, first_name, last_name, everything())
  
  # apply codebook to type 
  data %<>%
  mutate(SUBJECT = paste(SUBJECT,DATE)) %>%
  mutate(SUBJECT = paste(SUBJECT,SYSTEM)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONSTITUENT|LOAN MODIFICATION|EVICTION|FOIA REQUEST|GOLDEN PARACHUTE|PRIVATE TRANSFER FEE|REPURCHASE|HARP|REQUEST FOR ASSISTANCE|MULTIFAMILY|TERMITE|TRANSFER FEES|PRIVACY|QUALIFIED|LANGUAGE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONSTITUENT|LOAN MODIFICATION|EVICTION|FOIA REQUEST|GOLDEN PARACHUTE|PRIVATE TRANSFER FEE|REPURCHASE|HARP|REQUEST FOR ASSISTANCE|MULTIFAMILY|TERMITE|TRANSFER FEES|PRIVACY|QUALIFIED|LANGUAGE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("LIFE INSURANCE COMPANIES|PRIVATE LAW", SUBJECT, ignore.case = TRUE), "4", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("LIFE INSURANCE COMPANIES|PRIVATE LAW", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("REPORT TO CONGRESS|FOIA REPORT|TESTIFY|FOLLOW UP QUESTIONS|HEARING TRANSCRIPT|HEARING|TESTIMONY", SUBJECT, ignore.case = TRUE), "INFORMATION", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FSOC|FHLB|MERKLEY-LEVIN|STRATEGIC PLAN", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FSOC|FHLB|MERKLEY-LEVIN|STRATEGIC PLAN", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("CONFORMING LOAN LIMITS", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("SECURIT", SUBJECT, ignore.case = TRUE), "2", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PROGRAM", SUBJECT, ignore.case = TRUE), "3", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "5", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(POLICY_EVENT = ifelse (!grepl("[0-9]", POLICY_EVENT) & grepl("PROPOSED RULE|RULEMAKING", SUBJECT, ignore.case = TRUE), "RULE", POLICY_EVENT)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("FORECLOSURE|PACE", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("FORECLOSURE|PACE", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "2", CERTAINTY)) %>%
  mutate(ALT_TYPE = ifelse (!grepl("[0-9]", ALT_TYPE) & grepl("PRINCIPAL REDUCTION", SUBJECT, ignore.case = TRUE), "5", ALT_TYPE)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("THANK YOU|APOLOGY", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("THANK YOU|APOLOGY", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%
  mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("EVERBANK|BANK OF AMERICA", SUBJECT, ignore.case = TRUE), "2", TYPE)) %>%
  mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("EVERBANK|BANK OF AMERICA", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}






