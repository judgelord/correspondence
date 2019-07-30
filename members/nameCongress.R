library(devtools)

## Rvoteview dependencies through errors, so moving to a data file of member names defined by "members/nameCongress.R"
if(!"Rvoteview" %in% rownames(installed.packages())) {
   devtools::install_github("voteview/Rvoteview")
}
library(Rvoteview)

# This script aguments member names from voteview to enable merging with wide variety of name formats. Matching functions are in nameMethods.R

members <- full_join(member_search(congress = c(105:108)) %>% select(-congresses),
                     member_search(congress = c(109:120)))  # get voteview data for selected Congresses
  # format state
 members%<>%
   mutate(state = tolower(state)) %>%
   
  group_by(chamber, party_code, congress) %>%
   mutate(party_size = n()) %>% 
   
   ungroup()
   
   
   members %<>%
    # mutate(state = as.character(state)) %>%
    # extract first, middle, last, and common names
    mutate(last_name = gsub(", .*", "", bioname)) %>%
    mutate(first_name = gsub("^.*?, |, Jr.| Jr.|, III| III| IV", "", bioname)) %>% # this messes up Joe Kennedy Jr, Joe Kennedy III, but need to correct in MemberNameDateCorrections.R function because matching this with nameMethods is unrealistic and they do not overlap
    mutate(first_name = gsub(", II| II", "", first_name)) %>% 
    mutate(common_name = stringr::str_extract(bioname, "\\(.*\\)")) %>%
    mutate(common_name = gsub("\\)|\\(", "", common_name)) %>%
    mutate(first_name = gsub("\\(.*\\)", "", first_name)) %>%
    mutate(middle_name = stringr::str_extract(first_name, " .*")) %>%
    mutate(middle_name = gsub(" ", "", middle_name)) %>%
    mutate(middle_initial = substr(middle_name, 1, 1)) %>%
    mutate(first_name = gsub(" .*", "", first_name)) %>%
    mutate(common_name = ifelse(is.na(common_name), "", common_name)) %>%
    mutate(first_initial = gsub("^(\\w).*",  "\\1", first_name)) %>% 
    mutate(last_name = ifelse(last_name == "MCCARTHY", "McCARTHY", last_name)) %>% # IS THIS A TYPO FROM VOTEVIEW, OR ARE THEY ALL LIKE THIS?
    mutate(maiden_name = NA) %>%
    
    # maiden names
    mutate(maiden_name = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "Bailey", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "HASSAN, Margaret (Maggie)", "Wood", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "KUSTER, Ann McLane", "McLane", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "SMITH, Tina", "Flint", maiden_name)) %>% 
    mutate(maiden_name = ifelse(bioname == "BONO, Mary", "Mack", maiden_name)) %>% 
     
    # last names
    mutate(last_name = ifelse(bioname == "CAPITO, Shelley Moore", "Moore Capito", last_name)) %>%  
     
    # common names
     # NOTE, as written this will overwrite existing common names. 
     # FIXME by adding "common_name == "" &" unless we want to overwrite 
    mutate(common_name = ifelse( first_name == "Daniel", "Dan", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Andrew", "Andy", common_name)) %>%
    mutate(common_name = ifelse(first_name == "Gregory", "Greg", common_name)) %>%
    mutate(common_name = ifelse(  (first_name == "Dan")&(common_name==""), "Daniel", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Michael", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Joe", "Joseph", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Joseph", "Joe", common_name)) %>% 
    mutate(common_name = ifelse(first_name == "Mike", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "David")&(common_name==""), "Dave", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Dave")&(common_name==""), "David", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Thomas")&(common_name==""), "Tom", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Tom")&(common_name==""), "Thomas", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kathleen")&(common_name==""), "Kathy", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kathy")&(common_name==""), "Kathleen", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Patrick")&(common_name==""), "Pat", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Pat")&(common_name==""), "Patrick", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "James")&(common_name==""), "Jim", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jim")&(common_name==""), "James", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Pete")&(common_name==""), "Peter", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Peter")&(common_name==""), "Pete", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Richard")&(common_name==""), "Rich", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Chris")&(common_name==""), "Christopher", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Christopher")&(common_name==""), "Chris", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Robert")&(common_name==""), "Bob", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "William")&(common_name==""), "Bill", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Bill")&(common_name==""), "William", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Melvin")&(common_name==""), "Mel", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jeffrey")&(common_name==""), "Jeff", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Jeff")&(common_name==""), "Jeffrey", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Ben")&(common_name==""), "Benjamin", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Benjamin")&(common_name==""), "Ben", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Charles")&(common_name==""), "Charlie", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Charlie")&(common_name==""), "Charles", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Chuck")&(common_name==""), "Charles", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Kenneth")&(common_name==""), "Ken", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Mathew")&(common_name==""), "Matt", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Matthew")&(common_name==""), "Matt", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Matt")&(common_name==""), "Matthew", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Edward")&(common_name==""), "Ed", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Theodore")&(common_name==""), "Ted", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Stevan")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Stephen")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Steven")&(common_name==""), "Steve", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Steve")&(common_name==""), "Steven", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Brad")&(common_name==""), "Bradley", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Bradley|Bradly")&(common_name==""), "Brad", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Nicholas")&(common_name==""), "Nick", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Russell")&(common_name==""), "Russ", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Philip")&(common_name==""), "Phil", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Dennis")&(common_name==""), "Denny", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Denny")&(common_name==""), "Dennis", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Tim")&(common_name==""), "Timothy", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Timothy")&(common_name==""), "Tim", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Al")&(common_name==""), "Alan", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Alfred")&(common_name==""), "Al", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Donald")&(common_name==""), "Don", common_name)) %>% 
    mutate(common_name = ifelse(  (first_name == "Don")&(common_name==""), "Donald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HURD, William Ballard", "Will", common_name)) %>%
    mutate(common_name = ifelse(bioname == "BUNNING, James Paul David", "Jim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "FORBES, J. Randy", "Randy", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GRIFFITH, H. Morgan", "Morgan", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DURBIN, Richard Joseph", "Dick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BARLETTA, Lou", "Lou", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BUCHANAN, Vernon G.", "Vern", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SCHAKOWSKY, Janice D.", "Jan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KOHL, Herbert H.", "Herb", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "STEARNS, Clifford Bundy", "Cliff", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SNYDER, Victor F.", "Vic", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "EVERETT, Robert Terry", "Terry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WAMP, Zachary Paul", "Zach", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DEAL, John Nathan", "Nathan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COCHRAN, William Thad", "Thad", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GOODLATTE, Robert William", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "THOMPSON, Michael", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WILSON, Charlie", "Charles", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PENCE, Mike", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GAETZ, Matthew L. II", "Matt", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "STABENOW, Deborah Ann", "(Deb|Debbie)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "VAN HOLLEN, Christopher", "Chris", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ROSS, Michael Avery", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ASHFORD, John Bradley", "Brad", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MCEACHIN, Aston Donald", "Donald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WITTMAN, Robert J.", "Rob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ALLARD, A. Wayne", "Wayne", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GRASSLEY, Charles Ernest", "Chuck", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GOHMERT, Louie", "(Lou|Louis)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WALKER, Bradley Mark", "Mark", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DEMINGS, Valdez Butler", "Val", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SENSENBRENNER, Frank James, Jr.", "(James|Jim)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "AKIN, W. Todd", "Todd", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GRAVES, Samuel", "Sam", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "UPTON, Frederick Stephen", "Fred", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DOYLE, Michael F.", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MURPHY, Timothy", "Tim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARNEY, Chris", "Christopher", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CAMP, David Lee", "Dave", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CRAPO, Michael Dean", "Mike", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DeMINT, James W.", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LANGEVIN, James", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SANDERS, Bernard", "Bernie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PAUL, Ronald Ernest", "Ron", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "YOUNG, Donald Edwin", "Don", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COBURN, Thomas Allen", "Tom", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BUYER, Stephen Earle", "Steve", common_name)) %>%  
    mutate(common_name = ifelse(bioname == "WYDEN, Ronald Lee", "Ron", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DELAHUNT, Bill", "William", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BOYD, F. Allen, Jr.", "Allen", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOEKSTRA, Peter", "Pete", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "REICHERT, David G.", "Dave", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BOREN, Daniel David", "Dan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PASCRELL, William J., Jr.", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MACK, Connie, IV", "Connie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HALVORSON, Deborah L.", "(Deb|Debbie)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COHEN, Stephen", "Steve", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "INGLIS, Robert Durden", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ETHERIDGE, Bobby R.", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BOUCHER, Frederick C.", "Rick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "RYAN, Timothy J.", "Tim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KAGEN, Steven", "Steve", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BURTON, Danny Lee", "Dan", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LATHAM, Thomas", "Tom", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "COOPER, James Hayes Shofner", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "GORDON, Barton Jennings", "Bart", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DICKS, Norman DeValois", "Norm", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HONDA, Mike", "Michael", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "JOHNSON, Ron", "Ronald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MEEHAN, Patrick", "Pat", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SCHILLING, Bobby", "Robert", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "RIGELL, E. Scott", "Scott", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WILLIAMS, Roger", "John", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BERRY, Robert Marion", "Marion", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LABRADOR, Raúl R.", "Raul", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOLLINGSWORTH, Joseph Albert III", "Trey", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "NOLAN, Richard Michael", "Rick", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "SAXTON, Hugh James", "Jim", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "McNERNEY, Jerry", "Gerald", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KHANNA, Rohit", "Ro", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KRISHNAMOORTHI, S. Raja", "Raja", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "JOHNSON, Hank", "Henry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DUNCAN, John J., Jr.", "(Jimmy|Jim)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "PERLMUTTER, Ed", "Earl", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "NEUGEBAUER, Randy", "Rand", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "DENT, Charles W.", "Charlie", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "WOODALL, Rob", "Robert", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LOTT, Chester Trent", "Trent", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CONAWAY, K. Michael", "(Mike|Michael)", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CRAWFORD, Rick", "Eric", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LONG, Billy", "Bill", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "FERGUSON, Anderson Drew IV", "Drew", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "CARTER, Buddy", "Earl", common_name)) %>% 
    
    mutate(common_name = ifelse(bioname == "ROSEN, Jacklyn", "Jacky", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BURR, Richard M.", "Rich", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HURT, Robert", "Bob", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "LOTT, Chester Trent", "Trent", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MURPHY, Timothy", "Tim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HOLDEN, Thomas Timothy (Tim)", "Tim", common_name)) %>%
    mutate(common_name = ifelse(bioname == "TSONGAS, Nicola S. (Niki)", "Niki", common_name)) %>%
    mutate(common_name = ifelse(bioname == "KENNEDY, Edward Moore (Ted)", "Ted", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CUNNINGHAM, Randall (Duke)", "(Randy|Duke)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HOUGHTON, Amory, Jr.", "Amo", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GOODE, Virgil H., Jr", "Virgil", common_name)) %>%
    mutate(common_name = ifelse(bioname == "RUPPERSBERGER, C.", "Dutch", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "MEEHAN, Martin Thomas", "Marty", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "ROUKEMA, Margaret Scafati", "Marge", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "KLECZKA, Gerald Daniel", "Jerry", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "BERGMAN, John", "Jack", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "HOLLINGS, Ernest Frederick", "Fritz", common_name)) %>% 
    mutate(common_name = ifelse(bioname == "THOMPSON, Glenn", "G.T.", common_name)) %>% 
    
    mutate(common_name = ifelse(bioname == "COX, Charles Christopher", "Christopher", common_name)) %>%
    mutate(common_name = ifelse(bioname == "GRAHAM, Daniel Robert (Bob)", "Bob", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SHAW, Eugene Clay, Jr.", "Clay", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ANDREWS, Robert Ernest", "(Bob|Rob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "LUGAR, Richard Green", "(Dick|Rich)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SCOTT, Robert C.", "(Bob|Bobby)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "VAN DREW, Jefferson", "Jeff", common_name)) %>%
    mutate(common_name = ifelse(bioname == "OWENS, William", "(Will|Bill)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "NELSON, Earl Benjamin (Ben)", "(Ben|E.B.)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "(Rob|Bob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "WHITFIELD, Wayne Edward (Ed)", "(Ed|Edward)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CLELAND, Joseph Maxwell (Max)", "(Max|Joe)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "EHLERS, Vernon James", "Vern", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MARKEY, Edward John", "(Ed|EJ)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ROSEN, Jacklyn Sheryl", "Jacky", common_name)) %>%
    mutate(common_name = ifelse(bioname == "KUSTER, Ann McLane", "Annie", common_name)) %>%
    mutate(common_name = ifelse(bioname == "BISHOP, Robert (Rob)", "(Rob|Bob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "DOOLEY, Calvin M.", "Cal", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SHOWS, Clifford Ronald", "Ronnie", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PACKARD, Ronald C", "Ron", common_name)) %>%
    mutate(common_name = ifelse(bioname == "MURRAY, Patty", "(Patricia|Pat)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SCARBOROUGH, Charles Joseph", "(Charlie|Joe)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SPACE, Zack", "Zachary", common_name)) %>%
    mutate(common_name = ifelse(bioname == "RICE, Tom", "Hugh", common_name)) %>%
    mutate(common_name = ifelse(bioname == "SMITH, Tina", "Christine", common_name)) %>%
    mutate(common_name = ifelse(bioname == "ROY, Charles", "Chip", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PORTMAN, Robert Jones (Rob)", "(Bob|Rob)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "PANETTA, James Varni", "(Jimmy|Jim)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CISNEROS, Gil", "Gilbert", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CLAY, William Lacy, Jr.", "(Bill|Lacy)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "AXNE, Cynthia", "Cindy", common_name)) %>%
    mutate(common_name = ifelse(bioname == "STEUBE, William", "(Bill|Greg|Gregory)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "CORREA, Jose Luis", "Lou", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HUDSON, Richard", "(Rich|Rick)", common_name)) %>%
    mutate(common_name = ifelse(bioname == "HAWLEY, Joshua David", "Josh", common_name)) %>%
     
  # remove accent marks (using RegEx dot for specials, not exact matching)
    mutate(common_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", common_name)) %>% 
    mutate(last_name = ifelse(grepl("VEL.ZQUEZ, Nydia M.", bioname), "VELAZQUEZ", last_name)) %>% 
    mutate(last_name = ifelse(grepl("C.RDENAS, Tony", bioname), "CARDENAS", last_name)) %>% 
    mutate(last_name = ifelse(grepl("GUTI.RREZ, Luis V.", bioname), "GUTIERREZ", last_name)) %>% 
    mutate(first_name = ifelse(grepl("SERRANO, Jos. E.", bioname), "Jose", first_name)) %>% 
    mutate(first_name = ifelse(grepl("CARSON, Andr.", bioname), "Andre", first_name)) %>% 
    mutate(last_name = ifelse(grepl("LUJ.N, Ben Ray", bioname), "LUJAN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("LUJ.N GRISHAM, Michelle", bioname), "LUJAN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("BARRAG.N, Nanette Diaz", bioname), "BARRAGAN", last_name)) %>% 
    mutate(last_name = ifelse(grepl("S.NCHEZ, Linda T.", bioname), "SANCHEZ", last_name)) %>% 
    mutate(first_name = ifelse(grepl("GRIJALVA, Ra.l M.", bioname), "Raul", first_name)) %>% 
    mutate(first_name = ifelse(grepl("HINOJOSA, Rub.n", bioname), "Ruben", first_name)) %>% 
    mutate(first_name = ifelse(grepl("LABRADOR, Ra.l R.", bioname), "Raul", first_name)) %>% 
  
  # middle name
    mutate(middle_name = ifelse(bioname == "PLATTS, Todd", "Russell", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CARTER, Buddy", "Leroy", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "GRIFFITH, H. Morgan", "Morgan", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "HARRIS, Kamala Devi", "Devi", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CLARKE, Yvette", "Diane", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CRAIG, Larry Edwin", "Edwin", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "GOODLATTE, Robert William", "William", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "WOLF, Frank Rudolph", "Rudolph", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "LUGAR, Richard Green", "Green", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "COLLINS, Susan Margaret", "Margaret", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "FEINGOLD, Russell Dana", "Dana", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "WARNER, John William", "William", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "GRASSLEY, Charles Ernest", "Ernest", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "MIKULSKI, Barbara Ann", "Ann", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "LYNCH, Stephen F.", "Francis", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "McCLINTOCK, Tom", "Miller", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "PELOSI, Nancy", "Patricia", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "HARMAN, Jane L.", "Margaret Lakes", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "BONNER, Jr., Josiah Robins (Jo).", "Robins", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ESHOO, Anna Georges", "Georges", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "FRANKEN, Al", "Stuart", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "SCHIFF, Adam", "Bennett", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "CAPITO, Shelley Moore", "Wellons", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "Ann", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "THOMPSON, Glenn", "William", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "PRICE, Tom", "Edmunds", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "RICE, Tom", "Thompson", middle_name)) %>%  
    mutate(middle_name = ifelse(bioname == "SMITH, Tina", "Elizabeth", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CULBERSON, John", "Abney", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "SCOTT, David", "Albert", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "CRAMER, Kevin", "John", middle_name)) %>% 
    mutate(middle_name = ifelse(bioname == "KLINE, John", "Paul", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "DUFFY, Sean", "Patrick", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ABRAHAM, Ralph", "Lee", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "ROSS, Dennis", "Alan", middle_name)) %>%
    mutate(middle_name = ifelse(bioname == "MURPHY, Patrick" & state == "florida", "Erin", middle_name)) %>%
   
    # middle initials
    mutate(middle_initial = ifelse(bioname == "CASEY, Robert (Bob), Jr.", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "FRANKEN, Al", "S", middle_initial)) %>%
    mutate(middle_initial = ifelse(bioname == "ESHOO, Anna", "G", middle_initial)) %>%
    mutate(middle_initial = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BOND, Christopher Samuel (Kit)", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PENCE, Mike", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "LANGEVIN, James", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WEST, Allen", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RISCH, James", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HIRONO, Mazie", "K", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "DJOU, Charles", "K", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "GILLIBRAND, Kirsten", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "KUCINICH, Dennis", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SMITH, Adrian", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "THUNE, John", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HUTCHISON, Kathryn Ann Bailey (Kay)", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WALZ, Tim", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SARBANES, John", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SALAZAR, John", "T", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "UDALL, Mark", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "THOMPSON, Bennie", "G", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BENNETT, Robert", "F", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PLATTS, Todd", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "DELAHUNT, Bill", "D", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RICHARDSON, Laura", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "TIBERI, Patrick (Pat)", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BRALEY, Bruce", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HAGAN, Kay", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WARNER, Mark", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BEYER, Donald Sternoff Jr.", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RICHMOND, Cedric", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ADERHOLT, Robert", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "McKINLEY, David", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SEWELL, Terri", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HANNA, Richard", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "FLEISCHMANN, Chuck", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "MURPHY, Christopher", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "SCHIFF, Adam", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "HONDA, Mike", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "CARTWRIGHT, Matt", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "LOWENTHAL, Alan", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ADAMS, Alma", "S", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOYCE, David", "P", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WENSTRUP, Brad", "R", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "RENACCI, Jim", "B", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "TONKO, Paul", "D", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOHNSON, Hank", "C", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "VISCLOSKY, Peter", "J", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "OBAMA, Barack", "H", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "OWENS, William", "L", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "WILSON, Heather", "A", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "BILIRAKIS, Gus", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "ISSA, Darrell", "E", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PUTNAM, Adam", "H", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "PALAZZO, Steven", "M", middle_initial)) %>% 
    mutate(middle_initial = ifelse(bioname == "JOHNSON, Ron", "H", middle_initial)) %>% 
  
  mutate(middle_initial = ifelse(bioname == "CRAWFORD, Rick", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "AYOTTE, Kelly", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "BOXER, Barbara", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "MATSUI, Doris", "O", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "MERKLEY, Jeff", "A", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "ROTHMAN, Steven", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "REHBERG, Denny", "R", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "YOHO, Ted", "S", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CARTER, Buddy", "L", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "LIPINSKI, Daniel", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GOSAR, Paul", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROSEN, Jacklyn Shery", "S", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SNOWE, Olympia Jean", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WEBB, James H. (Jim)", "H.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SCOTT, Robert C.", "C.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROGERS, Mike", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "KING, Peter T.", "T.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CRAIG, Larry Edwin", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GOODE, Virgil H., Jr", "H.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "SHELBY, Richard C.", "C.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "FEINGOLD, Russell Dana", "D", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "LANDRIEU, Mary L.", "L.", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RUPPERSBERGER, C. A. (Dutch)", "A", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "MIKULSKI, Barbara Ann", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GRASSLEY, Charles Ernest", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WARNER, John William", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "COLLINS, Susan Margaret", "M", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "LUGAR, Richard Green", "G", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "WOLF, Frank Rudolph", "R", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARRIS, Kamala Devi", "D", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "CLARKE, Yvette", "D", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "BURR, Richard", "M", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "ESTY, Elizabeth", "H", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "MURPHY, Timothy", "F", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "WILSON, Charlie", "A", middle_initial)) %>%   
  mutate(middle_initial = ifelse(bioname == "ROTHFUS, Keith", "J", middle_initial)) %>% 
  mutate(middle_initial = ifelse(bioname == "KING, Steve", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RIBBLE, Reid", "J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WEBER, Randy", "K", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MEEHAN, Patrick", "L", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HODES, Paul", "W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "LEE, Mike", "S", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "FERGUSON, Michael", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MOOLENAAR, John", "R", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "PETERSON, John", "E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "MERKLEY, Jeff", "A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARMAN, Jane L.", "ML", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "REID, Harry","M", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CARNEY, Chris","P", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "KILDEE, Dan","T", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "GRUCCI, Jr., Felix J.","J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WARREN, Elizabeth","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HASSAN, Margaret (Maggie)","C", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ARCURI, Michael","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HALL, John","J", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "TIPTON, Scott","R", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "KUSTER, Ann McLane","L", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "COVERDELL, Paul","D", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CAPITO, Shelley Moore","W", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "PRYOR, Mark","L", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "RICE, Tom","T", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "PRICE, Tom","E", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "ROSKAM, Peter","J", middle_initial)) %>%  
  mutate(middle_initial = ifelse(bioname == "NUGENT, Richard","B", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "WOMACK, Steve","A", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "HARRIS, Andy","P", middle_initial)) %>%
  mutate(middle_initial = ifelse(bioname == "CRAMER, Kevin","J", middle_initial)) 

 members %<>%    
  # first names
    mutate(first_name = ifelse(bioname == "BARLETTA, Lou", "Louis", first_name)) %>% 
    mutate(first_name = ifelse(bioname == "FORBES, J. Randy", "James", first_name)) %>%
    mutate(first_name = ifelse(bioname == "MACK, Connie, IV", "Connie", first_name)) %>% 
    mutate(first_name = ifelse(bioname == "CONAWAY, K. Michael", "Kenneth", first_name)) %>%
    mutate(first_name = ifelse(bioname == "BONNER, Jr., Josiah Robins (Jo)", "Josiah", first_name)) %>%
    mutate(first_name = ifelse(bioname == "GRUCCI, Jr., Felix J.)", "Felix", first_name))
     
  
  
  # make blank common names NA
  members %<>%
    mutate(common_name = ifelse(members$common_name=="", NA,  members$common_name))
  
  # # Creates new rows in member dataset. These are not actual members, but common names that we know shouldn't be matching
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "JEWELL"; members$first_name[nrow(members)] <- "Sally"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "NORTON"; members$first_name[nrow(members)] <- "Eleanor"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "SABLAN"; members$first_name[nrow(members)] <- "Gregorio"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PLASKETT"; members$first_name[nrow(members)] <- "Stacey"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "RADEWAGEN"; members$first_name[nrow(members)] <- "Amata"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "CHRISTENSEN"; members$first_name[nrow(members)] <- "Donna";members$middle_initial[nrow(members)] <- "M"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "PIERLUISI"; members$first_name[nrow(members)] <- "Pedro"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "WHITEHOUSE";
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "BORDALLO"; members$first_name[nrow(members)] <- "Madeleine"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "FALEOMAVAEGA"; members$first_name[nrow(members)] <- "Eni"
  # members[nrow(members)+1,] <- NA; members$last_name[nrow(members)] <- "JOHNSON"; members$first_name[nrow(members)] <- "Tia"; members$common_name[nrow(members)] <- "M. Tia"
  # 
  
  # select
  members %<>% 
    select(first_name, first_initial ,common_name, middle_name, middle_initial, last_name, bioname, everything()) 
  
  # NOTE: 
  # Voteview is missing non-voting members:
  # American Samoa at-large	Delegate	Amata Coleman Radewagen	Republican	2014
  # District of Columbia at-large	Delegate	Eleanor Holmes Norton	Democratic	1990
  # Guam at-large	Delegate	Madeleine Bordallo	Democratic	2002
  # Northern Mariana Islands at-large	Delegate	Gregorio Sablan	Independent	2008
  # Puerto Rico at-large	Resident Commissioner	Jenniffer González	Republican/NPP	2016
  # U.S. Virgin Islands at-large	Delegate	Stacey Plaskett	Democratic	2014
  
  # Replace blanks with NA 
  members %<>% 
    mutate(middle_initial = ifelse(middle_initial == "", NA, middle_initial)) %>% 
    mutate(middle_name = ifelse(middle_name == "", NA, middle_name)) #%>% 
  # seo middle names don't get us anything, and this just ends up filling in middle names with last names 
    #mutate(middle_name = ifelse(is.na(middle_initial), str_remove_all(seo_name, ".*?-|-.*"), middle_name))
  
  # Periods
  members$middle_initial %<>% str_remove("\\.")
  members$first_initial %<>% str_remove("\\.")
  members$middle_name %<>% str_remove("\\.")
  
  # add any missing middle initials
  
  members %<>%
    mutate(middle_initial = ifelse(is.na(middle_initial) & ! is.na(middle_name),
                                   str_sub(middle_name, 1,1),
                                   middle_initial))
  
  
  #create full name variables with different combinations of first, common, middle, middle initial, and last name
  members$first_last <- paste(members$first_name, members$last_name, sep = " ")
  members$first_maiden <- paste(members$first_name, members$maiden_name, sep = " ")
  members$common_last <- paste(members$common_name, members$last_name, sep = " ")
  members$first_middle_last <- paste(members$first_name, members$middle_name, members$last_name, sep = " ")
  members$first_initial_last <- paste(members$first_name, members$middle_initial, members$last_name, sep = " ")
  members$common_middle_last <- paste(members$common_name, members$middle_name, members$last_name, sep = " ")
  members$common_initial_last <- paste(members$common_name, members$middle_initial, members$last_name, sep = " ")
  #members$firstinitial_middleinitial_last <- paste(members$first_initial, members$middle_initial, members$last_name, sep = " ")
  
  members %<>% 
    ungroup() %>% 
    mutate(last_comma_first = paste0(last_name, ", ", first_name),
           first_maiden_last = paste(first_name, maiden_name, last_name),
           common_maiden_last = paste(common_name, maiden_name, last_name),
           #first_initial_last = paste(first_name, middle_initial, last_name),
           firstinitial_middleinitial_last = paste(first_initial, middle_initial, last_name),
           last_comma_initial = paste0("^", last_name, ", ", first_initial, "$"),
           last_comma_common = paste0(last_name, ", ", common_name),
           chamber_last = paste0(chamber, " ", last_name,"($|,)") %>% 
             str_replace("Senate", "Senator") %>% 
             str_replace("House", "Representative"))
  
  # drop chamber_last when there are multiple members with the same last name in that chamber 
  # FIXME -- may be able to do this by congress if matching by congress in the future; right now it would create duplicates and then drop them in the merge
  last_name_count <- members %>% 
    select(last_name, chamber, bioname, congress) %>%  
    distinct() %>%     
    count(last_name, chamber, congress) %>% 
    rename(last_name_count = n)
  
  members %<>% 
    full_join(last_name_count) %>% 
    mutate(chamber_last = ifelse(last_name_count > 1, NA, chamber_last)) %>% 
    select(-last_name_count)
  
  # # drop common last when there are multiple members with the same name
  # # commented out because I am option to over match and then drop people in merge, then fail to match people with the same name 
  # last_name_count <- members %>% 
  #   select(last_name, common_name, bioname) %>%  
  #   distinct() %>%     
  #   count(last_name, common_name) %>% 
  #   rename(last_name_count = n)
  # 
  # members %<>% 
  #   full_join(last_name_count) %>% 
  #   mutate(common_last = ifelse(last_name_count > 1, NA, common_last))%>% 
  #   select(-last_name_count)


# Replace NA names with "404error"
replace404 <- . %>% ifelse(str_detect(., "^NA | NA | NA$"), "404error", .)

members %<>% mutate_all(replace404)

members %<>%
  # FIXME # ? is it ok to group by congress? 
  group_by(bioname, congress) %>%
  mutate(pattern = c(first_last,
                     first_maiden,
                     first_middle_last,
                     first_initial_last,
                     common_last,
                     common_middle_last,
                     common_initial_last,
                     last_comma_first,
                     last_comma_initial,  # I worry about this over-matching, but we could test it--needed for VA # FIXME 
                     chamber_last, 
                     first_maiden_last,
                     common_maiden_last,
                     last_comma_common,
                     firstinitial_middleinitial_last

  ) %>%
    unique() %>%
    str_subset("404error", negate = T) %>%
    str_c(collapse = "|") %>%
    tolower() ) %>%
  ungroup()



# mismatches between middle name and middle initial? 
suspect_middle_names <- members %>% filter(!str_detect(middle_name, middle_initial) & !is.na(middle_name))


  # causes problems, but should eventually be used for more targeted matching
  members %<>% select(-congresses)

  
  
  # FIXME 
  # Until purrr version of extractMemberNames is done, we need to split up members
  # The new purrr solution will use members2 for now
  members2 <- members
    
  # 2000-2007 congresses 
  members_106to109th <- filter(members, congress < 112)
  
  members %<>% filter(congress > 109)
  
  save(members, members_106to109th, members2, file = "members/nameCongress.Rdata")
  
  