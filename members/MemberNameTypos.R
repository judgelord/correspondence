
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
  "Kirsten", "(Kirstine|Kirstein)",
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
  "Martin","Martrin", 
  "Cardenas", "C.rdenas",
  "VISCLOSKY", ".isclosky",
  "Murphy", "Murhpy",
  "Sanchez", "S.nchez",
  "Gutierrez", "(Guitierrez|GUTI.RREZ|Gut.errez)",
  "Melissa", "Melisssa",
  "Brian", "Brain",
  "Christopher", "Christoher",
  "Lujan", "Luj.n",
  "Raul", "R.ul",
  "Charles", "Charkes",
  "Benjamin", "Bemjamin",
  "McCaskill", "McCaskil$",
  "McCaskill,", "McCaskil,",
  "McCaskill ", "McCaskil ",
  "Luetkemeyer", "(Leutkemeyer|Leautkemeyer)",
  "HERRERA BEUTLER", "Herrera-Beutler",
  "WASSERMAN SCHULTZ", "WASSERMAN-SCHULTZ",
  "Michael", "(Midlael|Michaell|Micahel)",

  #"Diaz-Balart", "(Diaz($|,)|Balart($|,))", #still not working for whatever reason
  
  
  ###############################
  
  # Reversing order of first name and last name
  # Added the missing commas so they match the last, first pattern
  "Duncan, John", "Duncan John",
  "Johnson, Henry", "Johnson Henry",
  "Bono, Mary", "Bono Mary",
  "Rahall, Nick", "Rahall Nick",
  "Lee, Jackson", "Lee Jackson",
  "Conaway, Michael", "Conaway Michael",
  "Griffith, Morgan", "Griffith Morgan",
  "Womack, Steve", "Womack Steve",
  "ROHRABACHER, Dana", "ROHRABACHER Dana",
  "Nelson, Ben", "Nelson Ben"
  
  
)

# FREQUENT LAST NAME TYPOS WHERE WE ALSO WANT TO SEE THE FIRST NAME 
typos_last <- tribble(
  ~first_name, ~last_name, ~last_name_typos,
  "Patty", "Murray", "(Murrary|Muray)",
  "Lois", "Capps", "Crapps",
  "Rick", "Boucher", "Bocuher",
  "Robert", "Andrews", "Andrew",
  "Rodney", "Frelinghuysen", "Frelinhuysen",
  "Shelly", "Barkley", "Berkley",
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
  "Steve", "Scalise", "(Scalisse|Scallise)",
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
  "George","Radanovich", "(Radanovich|Radavich|Radnovich)",
  "George", "Nethercutt", "Nethecutt",
  "Frank", "Murkowski", "(Murkowsk|Mukowski)",
  "Marcy", "Kaptur", "Kaptor",
  "Bob", "Goodlatte", "(Godlatte|Goodlat)",
  "Dutch", "Ruppersberger", "Rupperberger",
  "Carolyn", "Maloney", "Malony",
  "Charles", "Rangel", "Ranger",
  "Christopher", "Van Hollen", "Van Kollen", 
  "Gary", "Ackerman", "Acherman",
  "Conrad", "Burns", "Bums",
  "John", "Hostettler", "Hostetler",
  "Amy", "Klobuchar", "Klobachur",
  "Raja", "Krishnamoorthi", "Krishnamoothi",
  "Barbara", "Mikulski", "(Mukulski|Milkulski)",
  "Ruben", "Hinojosa", "Hinohosa",
  "George", "Lemieux", "Lemieuz",
  "Tom", "Periello", "Perielo",
  "Stephanie", "Herseth Sandlin", "(Herseth|Sandlin)",
  "John", "ROCKEFELLER", "ROCKFELLER",
  "Ron", "Wyden", "Wydon",
  "John", "Cornyn", "(Comyn|Com yn|Corvyn|Coryn)",
  "Joe", "Manchin", "Machin",
  "Blake", "Farenthold", "Farenhold",
  "Anna", "Eshoo", "Eschoo",
  "Heidi", "Heitkamp", "(Heitkkamp|Heitkmap)",
  "Gerry", "Connolly", "Connelly",
  "Anthony", "Gonzalez", "Gonzales",
  "James", "Inhofe", "(Inholfe|Imhofe|Imholfe|Inhoffe)",
  "Yvette", "Clarke", "Clark",
  "Maurice", "Hinchey", "Henchey",
  "Chaka", "Fattah", "Chakka",
  "Kirsten", "Gillibrand", "Gillbrand",
  "James","Barrett", "(Barrat|Barret)",
  "Jaime", "HERRERA BEUTLER", "(HERRERA|BEUTLER)",
  "Lucille", "ROYBAL-ALLARD", "(Allard|Roybal)",
  "James", "Barrett", "(GRESHAM|BARRETT)",
  "Catherine", "CORTEZ MASTO", "(CORTEZ|MASTO|MATSO|Cortez Mastro)",
  "Thomas","Coburn","(Cobum|Co bum)",
  "Trent", "Kelly", "Key",
  "Robin", "Kelly", "Key",
  "Mike", "Kelly", "Key",
  "Jon","Kyl", "Kyi",
  "Cliff", "Stearns","Steams",
  "Wayne", "Whitfield", "Whitefield",
  "Jane", "Harman", "Harmon",
  "Christopher", "Van Hollen", "(Van|Hollen)",
  "Debbie", "WASSERMAN SCHULTZ", "(Wasserman|Schultz|Wasserman-Schultz)",
  "Cathy", "McMORRIS RODGERS", "(McMorris|Rodgers)",
  "James", "Barrett", "(Baret|Barett|Barret)",
  "Mario", "Diaz-Balart", "(Diaz|Balart)",
  "Lincoln", "Diaz-Balart", "(Diaz|Balart)",
  "Stephanie", "Lujan Grishman", "Luj.n Grishman",
  "Carol", "Shea-Porter", "(Shea|Porter)",
  "Daniel", "Donovan", "Donavan",
  "Gloria", "Negrete McLeod", "(Negrete|McLeod)",
  "Sheila", "Jackson Lee", "(Jackson|Lee|Jackson-Lee)",
  "Virginia", "Brown-Waite", "(Brown|Waite)",
  "Bonnie", "Watson Coleman", "(Watson|Coleman)",
  "Beto", "O'Rourke", "O.Rourke",
  "Beto", "O'Rourke", "Rourke",
  "Barbara", "Boxer", "Baxter",
  "David", "Dreier", "Reier",
  "Kelly", "Ayotte", "Aytotte",
  "Steve", "King", "Kiing",
  "Peter", "King", "Kiing",
  "Angus", "King", "Kiing",
  "Martin", "Heinrich", "(Hienrich|Heinriech)",
  "Ellen", "Tauscher", "Ianscher",
  "Jeff", "Bingaman", "Bingamen",
  "Elizabeth", "Warren", "Varren",
  "Adam", "Schiff", "(Schif|Sdxiff)",
  "Robert", "TORRICELLI", "Toricelli",
  "Kay", "Hutchison", "Hutchinson",
  "Ann", "Kuster", "McLane",
  "Charles", "Grassley", "Grassly",
  "Slade", "Gorton", "Gordon",
  "Bob", "Franks", "Francks",
  "Mary", "Bono", "Bone",
  "Nancy", "Johnson", "Jonhson",
  "Paul", "COVERDELL", "Cordovell",
  "Tom", "DeLay", "De Lay",
  "Steve", "Pearce", "Peace",
  "Charlie", "Crist", "Christ",
  "Rick", "Crawford", "Crawfrod",
  "Mikie", "SHERRILL", "Sheril",
  "Robert", "BENNETT", "Bernnett",
  "Peter","DeFazio","De Fazio",
  "Paul", "Sarbanes", "Sabanes",
  "Mike", "McIntyre", "McLntyre"
  
  
  
  
) %>% 
  transmute(typos = str_c(str_c(first_name, "( | [A-z]* )", last_name_typos, "($| |,|;)"),
                          str_c(last_name_typos, ", ", first_name), sep = "|"),
            correct = paste(first_name, last_name)) 


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
  "Shelley","Moore Capito", "(Shelby|Shelly)", 
  "Charles", "Schumer", "(Charls|Charls E)", # FIXME just adding middle initial for now, but eventually, it should be added to the typo pattern by merging with members data
  "Julia", "Carson", "Julie", 
  "Tom", "Barrett", "Mark", 
  "Matt", "Cartwright", "Mark", 
  "Katherine", "Clark", "Kathrine", 
  "Randy", "Weber", "Randay", 
  "Nancy", "Pelosi", "Nanci", 
  "Patrick", "Leahy", "Partrick", 
  "Ralph", "Regula", "Raplh", 
  "Chris", "Gibson", "Cris", 
  "Ron", "Estes", "(John|Jon)",
  "Dean", "Heller", "Den",
  "Dennis", "Cardoza", "Dinnes",
  "Harry", "Reid", "Hary",
  "George", "Voinovich", "Geaorge",
  "Candice", "Miller", "(Candace|Candance)",
  "Cedric", "Richmond", "Cedic",
  "Christopher", "Smith", "Christoper",
  "Christopher", "Bond", "(Chritoper|Christoper)",
  "Chris", "Coons", "Cris",
  "Chris", "Stewart", "Cris",
  "Hillary", "Clinton", "(Hilllary|Hilary|Fillary)",
  "Filemon", "Vela", "Filimon",
  "Artur", "Davis", "Arthur",
  "Patrick", "Leahy", "Ted",
  "Mary", "Bono", "Mack",
  "Harry", "Reid", "Marry",
  "Anna", "Eshoo", "(Ana|Anna A.)",
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
  "Sheila", "Jackson Lee", "Shelee",
  "Jose", "Serrano", "Jos.",
  "Lindsey", "Graham", "Lindsay",
  "Andre", "Carson", "Andr.",
  "Corrine", "Brown", "Corinne",
  "Anna", "Eshoo", "Ana",
  "Vernon", "Ehlers", "Vermon",
  "Margaret", "Hassan", "Margret",
  "Angus", "King", "Argus",
  "Gary", "Ackerman", "Garry",
  "Jon", "Corzine", "John",
  "Renee", "Ellmers", "Renne",
  "Bart", "Stupak", "Bark",
  "Jane", "Harman", "James",
  "Byron", "Dorgan", "Bayron",
  "Ann", "Kuster", "Anne",
  "Maxine", "Waters", "Martina",
  "John", "Cornyn", "Jon",
  "William", "Clay", "(Wim|Wm.)",
  "William", "Tauzin", "WJ"
 
  
  
)   %>% 
  transmute(typos = str_c(str_c(first_name_typos, "( | [A-z]* )", last_name), # match any middle initial
                          str_c(last_name, ", ", first_name_typos), sep = "|" ), 
            correct = paste(first_name, last_name))

# FREQUENT MIDDLE NAME TYPOS 
typos_middle <-  tribble(
  ~first_name, ~middle_name, ~last_name, ~middle_name_typos, 
  "Hillary", "Rodham", "Clinton", "(Rodman|Redham)",
  "Benjamin", "Nighthorse", "Campbell", "Nighhorse",
  "Ben", "Nighthorse", "Campbell", "Nighhorse",
  "John", "Dennis", "Hastert", "Denis",
  "Mike", "Dennis", "Rehberg", "Denis",
  "James", "Strom", "Thurmond", "Stom"
) %>% 
  transmute(typos = str_c(paste(first_name, " ", middle_name_typos, " ", last_name),
                          str_c(last_name, ", ", first_name, " ", middle_name_typos), sep = "|"),
            correct = paste(first_name, middle_name, last_name))

# FREQUENT COMMON NAME TYPOS 
typos_common_name <-  tribble(
  ~common_name, ~last_name, ~common_name_typos, 
  "Jon", "Corize", "John",
  "Joe", "Barton", "Joel",
  "Ben", "Cardin", "Bin",
  "Don", "Nickles", "Dob",
  "Don", "Young", "(Dong|Dob)",
  "Dennis", "Heck", "Denis",
  "Dennis", "Rehberg", "Denis"
) %>% 
  transmute(typos = str_c(str_c(common_name_typos, "( | [A-z]* )", last_name),
                          str_c(last_name, ", ", common_name_typos), sep = "|"),
            correct = paste(common_name, last_name)) 

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
  "Jon", "S", "Corzine", "C",
  "David", "N", "Cicilline", "(R|L|P)",
  "David", "B", "McKinley", "P",
  "James", "R", "Langevin", "P",
  "James", "M", "Inhofe", "N",
  "Charles", "H", "Taylor", "F",
  "Eliot", "L", "Engel", "E",
  "Bobby", "L", "Rush", "E",
  "Gary", "L", "Ackerman", "J",
  "Anthony", "G", "Brown", "J",
  "Peter", "G", "Fitzgerald", "B"
)%>% 
  transmute(typos = str_c(str_c(first_name, " ", middle_initial_typos, " ", last_name),
                          str_c(last_name, ", ", first_name, " ", middle_initial_typos), sep = "|"),
            correct = paste(first_name, middle_initial, last_name )) 





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
  # mutate(correct = paste(first_name, last_name)) %>% 
  group_by(correct) %>%
  summarise(typos = typos %>% str_c(collapse = "|") ) %>%
  full_join(typos_clear) %>% 
  mutate(typos = tolower(typos),
         correct = tolower(correct))




rm(typos_first, typos_last, typos_middle, typos_middle_initial, typos_clear)
