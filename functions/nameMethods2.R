

###########################################################################################################
# This function will extract names found in members dataset from data$Summary column 
# typical call:   
# data %<>% extractMemberName(members, 'FROM') 
# NOTE: A VAR NAMED "members" IN DATA can cause problems

extractMemberName <- function(data, members, col_name){
  
  data$Summary <- data[[col_name]]
  
  data %<>% mutate(Summary = data[[col_name]])

  # clean up text
  # remove periods 
  data$Summary <- gsub('\\.','', data$Summary)
  data$Summary <- gsub('(.*)\\.(.*)', "\\1\\2", data$Summary)
  # remove plus 
  data$Summary <- gsub('\\+', "", data$Summary)
  
  # remove common names in quotes 
  data$Summary <- gsub('\\"(Bobby|Buddy|GT|Buck|Chuck|Rick)\\"', "", data$Summary, ignore.case = TRUE)
  
  # trim down extra spaces
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  
  # drop paragraph breaks and trailing white space 
  data$Summary <- gsub("(^ |^  |^   |\n)", "", data$Summary)
  data$Summary <- gsub("Courntey", "Courtney", data$Summary)
  
  
  
  data$Summary <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,", "", data$FROM)
  data$Summary <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, MD,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$Summary)
  data$Summary <- gsub(pattern = "Member, U.S", "U.S", data$Summary)
  data$Summary <- gsub(pattern= "\\.\\.", replacement = ".", data$Summary)
  data$Summary <- gsub("(REP|SEN)(\\.|- | - |\\. )", "", data$Summary)
  data$Summary <- gsub("(^S(-| ))|Senator|Sen\\.", "", data$Summary)
  data$Summary <- gsub("(^(R|C)(-| ))|Repres|Congress|Rep", "", data$Summary)
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  data$Summary <- gsub("  |   |    ", " ", data$Summary)
  data$Summary <- gsub("(^ |^  |^   |\n)", "", data$Summary)
  

  # Common TYPOS 
  data$Summary <- gsub("Phill ", "Phil ", data$Summary) # added space after this one because some first or last name may begin with Phill...
  data$Summary <- gsub("Shelly", "Shelley", data$Summary)
  # data$Summary <- gsub("Ana", "Anna", data$Summary) # we can't do this because other first or last names may begin with Ana, it is to common of a string 
  data$Summary <- gsub("LaMalfn", "LaMalfa", data$Summary)
  data$Summary <- gsub("Jime ", "Jim ", data$Summary) # added space after this one because some first or last name may begin with Jime...
  
  # correct common OCR errors
  data$Summary <- ocr.errors(data$Summary)
  
  
  
  
  
  
  
  
  
  
  
  
  #####################
  # Match names in different formats
  ###################

  
  
  
  
  
  
  #   
  # for (i in 1:length(members$id)) {
  #   data %<>% mutate(first_name = ifelse( !is.na(members$common_name[i]) & data$first_name == members$common_name[i] & data$last_name == members$last_name[i],
  #                                         members$first_name[i], data$first_name))
  # 

  
  return(data)
}


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
# Fixes common errors when names read in from OCR
# May need adjustments for different agencies/formats
ocr.errors <- function(FROM){
  
  # adds deleted "ll" to last names
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
  FROm <- gsub("([A-Z])(55)([A-Z])", "\\1SS\\2",FROM)
  FROm <- gsub("([A-Z])(5)([A-Z])", "\\1S\\2",FROM)
  
  
  # other errors 3 
  # These  are applied to data$Summary
  FROM <- gsub(".1.", "", FROM)
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("J.*?n","Jason", FROM), FROM)
  FROM <- ifelse(grepl(" Cha", FROM)&grepl("((^| )Ja)|(J a.son)", FROM)&grepl('etz', FROM), gsub("Ch.*?z","Chaffetz", FROM), FROM)
  FROM <- ifelse(grepl("Tom", FROM)&grepl("Cobum|Co bum", FROM), gsub("Cobum|Co bum", "Coburn", FROM), FROM)
  FROM <- ifelse(grepl("DarrellIssa", FROM), 'Darrell Issa', FROM)
  FROM <- ifelse(grepl("Trent|Robin|Mike", FROM)&grepl("Key", FROM), gsub("(Trent|Robin|Mike) Key", "\\1 Kelly",FROM), FROM)
  FROM <- ifelse(grepl("Comyn|Com yn|Cobum|Corvyn", FROM)&grepl("John", FROM), gsub("Comyn|Com yn|Corvyn","Cornyn", FROM), FROM)
  FROM <- ifelse(grepl("Jon", FROM)&grepl("(^| )Kyi( |$)", FROM), gsub("Kyi","Kyl", FROM), FROM)
  FROM <- ifelse(grepl("Diane", FROM)&grepl("(^| )Feinstein( |$|,)", FROM), gsub("Diane","Dianne", FROM), FROM)
 # FROM <- ifelse(grepl("Cliff", FROM)&grepl("Steams", FROM), gsub("Steams","Stearns", FROM), FROM)
  FROM <- gsub("Cwnmings", 'Cummings', FROM)
  FROM <- gsub("Tnhofe", "Inhofe", FROM)
  FROM <- gsub("Ellrners","Ellmers", FROM)
  FROM <- gsub("TONKA", "TONKO", FROM)
  FROM <- gsub("Mcarthur|Mccarthur", "MacArthur", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Coryn( |$|,)", "\\1Cornyn\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Connelly( |$|,)", "\\1Connolly\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Heitkmap( |$|,)", "\\1Heitkamp\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Micahel( |$)", "\\1Michael\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Farenhold( |$|,)", "\\1Farenthold\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Eschoo( |$|,)", "\\1Eshoo\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Lary( |$)", "\\1Larry\\3", FROM, ignore.case = TRUE)
  FROM <- gsub("Christophers", "Christopher", FROM, ignore.case = TRUE)
  FROM <- gsub("Courntey", "Courtney", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Martrin( |$)", "\\1Martin\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Machin( |$|,)", "\\1Manchin\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Marry( |$|,)", "\\1Mary\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )T MOTHY( |$|,)", "\\1Timothy\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )L NCOLN( |$|,)", "\\1Lincoln\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Wydon( |$|,)", "\\1Wyden\\2", FROM, ignore.case = TRUE)
  FROM <- gsub("(^| )Klobachur( |$|,)", "\\Klobuchar\\2", FROM, ignore.case = TRUE)
  
  
  
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
#     This funtion call may be necessary:   data$last_name <- formatLastName(data, 'last_name')

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
# FIXME 

# CORRECTIONS TO ADD TO ABOVE 


# COMMON FIRST AND LAST NAME TYPOS TEMPLATE
tribble(
  ~correct.first, ~correct.last, ~incorrect.first, ~incorrect.last,
  "Patty", "Murray", NA, "Muray",
  "Patty", "Murray", NA, "Muray"
)

# FREQUENT FIRST AND LAST NAME TYPOS 
tribble(
  ~correct.first, ~correct.last, ~incorrect.first, ~incorrect.last,
  "Patty", "Murray", NA, "Muray",
  "Patrick", "Leahy", "Partrick", NA,
  "Ralph", "Regula", "Raplh", NA,
  "Lois", "Capps", NA, "Crapps",
  "Rick", "Boucher", NA, "Bocuher",
  "Ric", "Keller", "Rick", NA,
  "Robert", "Andrews", NA, "Andrew",
  "Rodney", "Frelinghuysen", NA, "Frelinhuysen",
  "Shelly", "Berkley", NA, "Barkley",
  "Steny", "Hoyer", NA, "Royer",
  "Steven", "Lynch", "Stephen", NA,
  "Zoe", "Lofgren", "Toe", NA,
  "Vito", "Fossella", NA, "Fosella",
  "John", "Barrasso", NA, "Barasso",
  "Larry", "Bucshon", NA, "Bueston",
  "Matt", "Cartwright", NA, "Cartwrite",
  "Chris", "Gibson", "Cris", NA,
  "Barbara", "Boxer", "Barabara", NA,
  "Jack", "Reed", NA, "Red",
  "David", "Schweikert", NA, "Schweikerl",
  "Peter", "DeFazio", NA, "DiFazio",
  "Zoe", "Lofgren", NA, "Lufgren",
  "Thomas", "Holden", NA, "Holen"
  
)  
  # FREQUENT MIDDLE NAME TYPOS 
  tribble(
    ~correct.first, ~correct.middle, ~correct.last, ~incorrect.first, ~incorrect.middle, ~incorrect.last,
    "Shelley", "Moore", "Capito", "Shelly", NA, NA, # this and most of these seem to be last name typos, not middle name typos. Can we put these above? Are the middle initials necessary to match? 
    "Charles", "E.", "Schumer", "Charls", NA, NA,
    "Hillary", "Rodham", "Clinton", NA, "Redham", NA, # <- this is what I would expect to see here, not the rest.
    "Russell", "D", "Feingold", "Russel", NA, NA,
    "Russell", "D", "Feingold", "Rusell", NA, NA,
    "Robert", "C", "Byrd", "Robert", "C", "Bryd", # ? these seem the same 
    "Orrin", "G", "Hatch", "Orring", NA, NA,
    "Olympia", "J", "Snowe", "Olymia", NA, NA,
    "Olympia", "J", "Snowe", NA, NA, "Showe"
  )
    
    


# #Fixes name typo (from DOL_SOL)
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

