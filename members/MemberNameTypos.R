
# FREQUENT TYPOS WHERE WE CAN JUST REPLACE THEM REGARDLESS OF THE WORDS BEFORE AND AFTER (i.e. we are very confident that this is what they should be)
# THERE IS NOTHING ELSE IT COULD POSSIBLY BE
typos_clear <- tribble(
  ~correct, ~typos,
  "Cummings", "(Cummins|Cwnmings)",
  "Ellmers", "Ellrners",
  "TONKO", "TONKA",
  "Darrell Issa", "DarrellIssa",
  "Phil ", "Phill ", # I added a space because it seems risky to match Phill...
  "LaMalfa", "LaMalfn",
  "Courtey", "Courntney",
  "Gerlach", "Gerlah",
  "Darlene", "Darene",
  "Elizabeth", "Elezabeth",
  "Jeffrey", "Jeflrey",
  "Barbara", "(Babara|Barabara)",
  "Velazquez", "Vel.zquez",
  "Timothy", "T.mothy",
  "MacArthur","(Mcarthur|Mccarthur)", # there is no McArthur in members file
  "Martin","Martrin", 
  "VISCLOSKY", ".isclosky",
  "Murphy", "Murhpy",
  "Sanchez", "S.nchez",
  "Gutierrez", "(Guitierrez|GUTI.RREZ|Gut.errez)",
  "Melissa", "Melisssa",
  "Brian", "Brain",
  "Christopher", "Christoher",
  "Raul", "R.ul",
  "Charles", "Charkes",
  "Benjamin", "Bemjamin",
  "McCaskill", "McCaskil$",
  "McCaskill,", "McCaskil,",
  "McCaskill ", "McCaskil ",
  "Luetkemeyer", "(Leutkemeyer|Leautkemeyer|Luektemeyer)",
  "Herrera Beutler", "(Herrera|Beutler|Herrera-Beutler|Harrera Beutler)", 
  "Michael", "(Midlael|Michaell|Micahel)",
  "SHERRILL", "Sheril",
  "Moolenaar", "(Molinar|Moolenar)",
  "Perlmutter", "Perimutter",
  "Manzullo", "Manzulo",
  "NEUGEBAUER", "BEUGEBAUER",

  


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
  "Nelson, Ben", "Nelson Ben",
  "Ryan, Paul", "Ryan Paul",
  "Chao, Elaine", "Chao Elaine",
  "Miller, George", "Miller George",
  "Herrera Beutler, Jaime", "Herrera Beutler Jaime"
  
  
)

# FREQUENT LAST NAME TYPOS WHERE WE ALSO WANT TO SEE THE FIRST NAME 
typos_last <- tribble(
  ~first_name, ~last_name, ~last_name_typos,
  "Patty", "Murray", "(Murrary|Muray)",
  "Lois", "Capps", "Crapps",
  "Rick", "Boucher", "Bocuher",
  "Robert", "Andrews", "Andrew",
  "Rodney", "Frelinghuysen", "(Frelinghuyser|Frelinhuysen)",
  "Shelley", "Berkley", "Barkley",
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
  "Russ", "Carnahan", "(Carnhan|Camahan)",
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
  "David", "Vitter", "(Vitters|Vilter)",
  "Debbie", "Stabenow", "(Stebenow|Stabeno|Stavenow)",
  "Dianne", "Feinstein", "(Feinsten|Feinstein|Feinstien|Fenstein)",
  "Don", "Nickles", "Nickels",
  "Doug", "Lamborn", "(Lamborg|Lambon)",
  'Edward', "Markey", "Marley",
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
  "Christopher", "Van Hollen", "(VANHOLLEN|Van Kollen|Van Hollen|Van|Hollen)", 
  "Gary", "Ackerman", "Acherman",
  "Conrad", "Burns", "Bums",
  "John", "Hostettler", "Hostetler",
  "Amy", "Klobuchar", "Klobachur",
  "Raja", "Krishnamoorthi", "(Krishnamoortni|Krishnamoothi)",
  "Barbara", "Mikulski", "(Mukulski|Milkulski)",
  "Ruben", "Hinojosa", "Hinohosa",
  "George", "Lemieux", "Lemieuz",
  "Tom", "Periello", "Perielo",
  "Stephanie", "Herseth Sandlin", "(Herseth|Sandlin)",
  "John", "ROCKEFELLER", "(Rockefellar|ROCKFELLER)",
  "Ron", "Wyden", "Wydon",
  "John", "Cornyn", "(Comyn|Com yn|Corvyn|Coryn)",
  "Joe", "Manchin", "Machin",
  "Blake", "Farenthold", "Farenhold|Farenthod",
  "Anna", "Eshoo", "Eschoo",
  "Heidi", "Heitkamp", "(Heitkkamp|Heitkmap|Heitkamps)",
  "Gerry", "Connolly", "Connelly",
  "Anthony", "Gonzalez", "Gonzales",
  "James", "Inhofe", "(Inholfe|Imhofe|Imholfe|Inhoffe|Lnhofe|Tnhofe)",  
  "Yvette", "Clarke", "Clark",
  "Maurice", "Hinchey", "Henchey",
  "Chaka", "Fattah", "Chakka",
  "Kirsten", "Gillibrand", "(Gillebrand|Gillbrand)",
  "James","Barrett", "(Barrat|Barret)",
  "Lucille", "ROYBAL-ALLARD", "(Allard|Roybal)",
  "James", "Barrett", "(GRESHAM|BARRETT)",
  "Catherine", "CORTEZ MASTO", "(CORTEZ|MASTO|MATSO|Cortez Mastro|Cortez Nasto|Cortez-Masto)",
  "Thomas","Coburn","(Cobum|Co bum)",
  "Trent", "Kelly", "Key",
  "Robin", "Kelly", "Key",
  "Mike", "Kelly", "Key",
  "Jon","Kyl", "Kyi",
  "Cliff", "Stearns","Steams",
  "Wayne", "Whitfield", "Whitefield",
  "Jane", "Harman", "Harmon",
  "Cathy", "McMORRIS RODGERS", "(McMorris|Rodgers|McMORRIS-RODGERS)",
  "James", "Barrett", "(Baret|Barett|Barret)",
  "Mario", "Diaz-Balart", "(Diaz|Balart)",
  "Lincoln", "Diaz-Balart", "(Diaz|Balart)",
  "Michelle", "Lujan Grisham", "(Grisham|Lujan|Lbuj.n Grishman|Lujan Grishar|Lujan Grish,|Grishar|Grish)",
  "Carol", "Shea-Porter", "(Shea|Porter|Shea Porter)",
  "Daniel", "Donovan", "Donavan",
  "Gloria", "Negrete McLeod", "(Negrete|McLeod)",
  "Sheila", "Jackson Lee", "(Jackson|Lee|Jackson-Lee)",
  "Ginny", "Brown-Waite", "(Brown|Waite|Brown Waite|BrownWaite)",
  "Bonnie", "Watson Coleman", "(Watson|Coleman)",
  "Beto", "O'Rourke", "(O Rourke|O.Rourke|Rourke|Orourke)",
  "Barbara", "Boxer", "Baxter",
  "David", "Dreier", "Reier",
  "Kelly", "Ayotte", "Aytotte",
  "Steve", "King", "Kiing",
  "Peter", "King", "Kiing",
  "Angus", "King", "Kiing",
  "Martin", "Heinrich", "(Hienrich|Heinriech)",
  "Ellen", "Tauscher", "Ianscher",
  "Jeff", "Bingaman", "(Bingman|Bingmen|Bingamen)",
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
  "Robert", "BENNETT", "Bernnett",
  "Peter","DeFazio","De Fazio",
  "Paul", "Sarbanes", "Sabanes",
  "Mike", "McIntyre", "(McEntire|McLntyre)",
  "Margaret", "Hassan", "Hassen",
  "Michael", "Capuano", "Capuno",
  "Max","BAUCUS","Baucuz",
  "Jerry","Costello", "Costelo",
  "James", "Hansen", "Hansch",
  "Mac", "Thornberry", "Thomberry",
  "Michael", "Turner", "Tuner",
  "Doug", "Lamborn", "Lambom|Lanborn",
  "Bill", "Posey", "Posev",
  "Carl", "Levin", "Levine",
  "Mark", "Bennet", "Bennett",
  "Shelley", "Moore Capito", "(Moore|Capito|MooreCapito|Moore-Capito)",
  "Jeanne", "Shaheen", "(Shaneen|Shahenn)",
  "Debbie", "Wasserman Schultz", "(Schultz|Wasserman Shultz|Wasserman-Schultz|Wasserman-S|Wasserman-|Wasserman)",   
  # FIXME #  last names hypenation can be a clear typo, but we can't just replace "Schultz"--that needs to be a last name typo (otherwise "Debbie Wasserman Schultz" will be replaced with "Debbie Wasserman Wasserman Schultz" and fail to match)
  # I deleted "Schultz" and moved it to a last name typos, but we may still have problems replacing "Wasserman-" like this (for example, "Wasserman-Schultz, Debbie" would be replaced with "Wasserman SchultzSchultz, Debbie and thus fail to match)
  "William", "Keating", "Keeting",
  "Mike", "Crapo", "Carpo",
  "Mark", "Desaulnier", "Desauliner",
  "Nanette", "Diaz Barragan", "Diaz-Barragan",
  "Bonnie", "Watson Coleman", "Watson-Coleman",
  "Adriano", "Espaillat", "Espaillet",
  "Tom", "O'Halleran", "O'Holleran",
  "Anthony", "BRINDISI", "Brindis",
  "Lisa", "BLUNT ROCHESTER", "BLUNT-ROCHESTER",
  "Bruce", "Braley", "Braly",
  "Marsha", "Blackburn", "Black burn",
  "Chris", "Collins", "Collings",
  "Scott", "Tipton", "Titpon",
  "Randy", "Weber", "Wever",
  "Randy", "Neugebauer","NEUGBAUER",
  "Johnny", "Isakson", "Isaakson",
  "Deb", "FISCHER", "Fisher",
  "Ami", "Bera", "(Beta|Gera)",
  "Lynn", "WOOLSEY", "Woosley",
  "Roger", "Wicker", "Wickler",
  "David", "CICILLINE", "Cicillinc",
  "David", "Roe", "Roc",
  "Al", "Franken", "Fianken",
  "Arlen","Specter","Speeter",
  "Dana", "ROHRABACHER", "Rohrabaeher",
  "Mark", "Souder", "Sounder",
  "Maria", "Cantwell", "Catwell",
  "Earl", "Blumenauer", "Blumeanuer",
  "Jeff", "Fortenberry", "Fortenbery",
  "Neil", "Abercrombie", "Abercombie",
  "Robert", "Menendez", "Mendez|Menedez",
  "Earl", "Carter", "Cater",
  "Greg", "Gianforte", "Gianfote",
  "Lisa", "Murkowski", "Murkowsi",
  "Marco", "Rubio", "Robio",
  "Tom", "Harkin", "Harkins",
  "George", "Holding", "Holdings",
  "Marlin", "Stutzman", "(Stutzzman|Stutzmann)",
  "Jeff", "Flake", "Flakes",
  "Ben", "Lujan", "Luj.n",
  "Pete", "Aguilar", "Aguliar",
  "Sean", "Duffy", "Dufffy",
  "Charles", "Schumer", "Shumer",
  "Robert", "Hurt", "Hunt",
  "Randy", "Hultgren", "(Hultgreen|Hultgreeen)",
  "Eric", "Swalwell", "(Swallwell|Swalwell)",
  "Sherrod", "Brown", "Browns",
  "Tony", "Cardenas", "(Caedenas|C.rdenas)",
  "Mick", "Mulvaney", "Mulvancy",
  "Kathy", "Castor", "Caster"
  
 
  


  
  
  
  
) %>% 
  transmute(typos = str_c(str_c(first_name, "( | [A-z]* )", last_name_typos, "($| |,|;)"),
                          str_c(last_name_typos, ", ", first_name), sep = "|"),
            correct = paste(first_name, last_name)) 


# FREQUENT FIRST NAME TYPOS 
typos_first <- tribble(
  ~first_name, ~last_name, ~first_name_typos,
  "Stephen", "Lynch", "Steven", 
  "Zoe", "Lofgren", "Toe", 
  "Jeanne", "Shaheen", "(Joanne|Teanne)", 
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
  "William", "Tauzin", "WJ",
  "Kirsten", "Gillibrand", "(Kristen|Kirstine|Kirstein)",
  "Joe", "Barton", "Joel",
  "Christopher","Smith","C. NJ",
  "Richard", "Durbin", "Richad",
  "Debbie", "Wasserman Schultz", "Debbies",
  "David", "Kustoff", "Davis",
  "Conor", "Lamb", "Connor",
  "Rob", "Woodall", "Rod",
  "John", "Thune", "Jon",
  "Vicente", "Gonzalez", "Vincente",
  "Emanuel", "Cleaver", "(EMAUNEL|Emmanuel)",
  "Adriano", "Espaillat", "Adrian",
  "Cory", "Gardner", "Corey",
  "Maria", "Cantwell", "Marie",
  "Jim", "Himes", "Jime",
  "Julia", "Brownley", "Juila",
  "Carol", "Shea-Porter", "Carole",
  "Collin", "Peterson", "Colin",
  "Ron", "Barber", "Rob",
  "Michael", "Michaud", "Micha",
  "Nanette", "BARRAGAN", "Nannette",
  "Sanford", "Bishop", "Sandford",
  "Alcee", "Hastings", "(Alccc|Ateee)",
  "Arlen", "Specter", "(Alan|Alien)",
  "Bart", "Gordon", "Art",
  "Barney", "Frank", "Bamey",
  "Richard", "Shelby", "Ricard",
  "Ruben", "Hinojosa", "Rub.n",
  "Shelley", "Berkley", "Shelly",
  "Chris", "Collins", "Chirs",
  "Diane", "Black", "Diana",
  "Lloyd", "Doggett", "Llyod"
  
 

 
  
  
)   %>% 
  transmute(typos = str_c(str_c(first_name_typos, "( | [A-z]* )", last_name), # match any middle initial
                          str_c(last_name, ", ", first_name_typos), sep = "|" ), 
            correct = paste(first_name, last_name))

# FREQUENT MIDDLE NAME TYPOS 
typos_middle <-  tribble(
  ~first_name, ~middle_name, ~last_name, ~middle_name_typos, 
  "Hillary", "Rodham", "Clinton", "(Rodman|Redham)",  
  "Benjamin", "Nighthorse", "Campbell", "(Nigbthorse|Nighhorse)",
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
  "Don", "Young", "(Dong|Dob|Dan)",
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
  "Peter", "G", "Fitzgerald", "B",
  "John", "J", "Faso", "S"
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
