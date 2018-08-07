if(require("Rvoteview")==F) {
  devtools::install_github("voteview/Rvoteview")
}
library(Rvoteview)
library(dplyr)
library(magrittr)
library(readr)
library(readxl)

myvars<-c("congress", "stewarticpsr", "name","statenumber", "cd","party", "seniorstatus", "chamber", "commcode", "committeename", "assigneddate", "terminationdate", "partystatus")


## Stewart's House Committee Assignments 103-113.  

hcd <- readxl::read_excel("committees/house_assignments_103-115-3.xls")
# library(readxl)
# hcd_late <- read_excel("house_assignments_103-115-3.xls")

# [1] "Congress"                                            "Committee.code"                                     
# [3] "ID.."                                                "Name"                                               
# [5] "Maj.Min"                                             "Rank.Within.Party.Status"                           
# [7] "Party"                                               "Date.of.Assignment"                                 
# [9] "Date.of.Termination"                                 "Senior.Party.Member"                                
# [11] "Committee.Seniority"                                 "Committee.Period.of.Service"                        
# [13] "Committee.status.at.end.of.this.Congress"            "Committee.continuity.of.assignment.in.next.Congress"
# [15] "Appointment.Citation"                                "Committee.Name"                                     
# [17] "State"                                               "CD"                                                 
# [19] "State.Name"                                          "Notes"                                              
# [21] "X" 

# rename consistant with Powell names 
names(hcd)<-c("congress", "commcode", "stewarticpsr", "name", "partystatus", "partyrank", "party", "assigneddate", "terminationdate", "seniorstatus", "committeeseniority", "committeeperiod", "assignmentstatusatend", "assignmentstatusnext", "ac",  "committeename", "statenumber", "cd", "state.name", "notes")

hcd$chamber<-"House"

hcd<-hcd[,myvars]




## Stewart's Senate Committee Assignments 80-102. Downloaded July 12, 2016.

# scd_early<-read_fwf("snc80102.mit.txt", fwf_widths(c(2,1,2,2,1,1,1,3,1,5,1,25,1,1,3,3,1,2,2,2,1,2,2,2,2,2,4,2,2,4,1,1,1,1,1,1,5,1,1)))

# scd_early<-as.data.frame(scd_early)

# names(scd_early)<-c("constant", "office",  "statenumber", "districtclass", "occupancy", "means", "reserved1", "party", "period", "stewarticpsr", "reserved2", "name", "chamber", "typecomm","congress", "commcode", "partystatus", "partyrank", "seniorstatus", "chamberseniority","committeeperiod","committeeseniority" ,"committeeorder", "statename", "assignedmonth", "assignedday", "assignedyear", "terminatedmonth", "terminatedday", "terminatedyear", "reserved3", "assignmentstatusatend","reserved4" ,"memberstatusnext", "reserved5", "assignmentstatusnext", "tempmember", "reserved6", "commcategory")

# scd_early$cd<-0

# scd_early$chamber<-"Senate"

# scdesmall<-scd_early[,myvars]



## Note, no region variable in senate file. 

## Note, warnings appear b/c in the data there are occasional xs (18 in total) in the committee order variable, where R expects to see numeric.  They are automatically reclassified as missing.  Not using the committee order variable. 

## Stewart's Senate Committee Assignments 103-112.  Dataset Date: 6/23/2011.  Downloaded July 12, 2016.

scd<-readxl::read_excel("committees/senate_assignments_103-115-3.xls")
# scd_late <- read_excel("senate_assignments_103-115-3.xls")

names(scd)<-c("congress", "commcode", "stewarticpsr", "name", "partystatus", "partyrank", "party", "assigneddate", "terminationdate", "X","seniorstatus", "committeeseniority", "committeeperiod", "assignmentstatusatend", "assignmentstatusnext", "ac",  "committeename", "statenumber", "cd", "state.name", "notes")

scd$cd<-0

scd$chamber<-"Senate"

scd<-scd[,myvars]





#### Merging all Stewart's committee data into a single file

stew<-as.data.frame(rbind(hcd, scd))


#### But Stewart uses a different ICPSR number convention that doesn't match with other ICPSR numbers. 

#### Manually recoding differences.  Many differences involve the 98-105th congresses.

stew$icpsr<-stew$stewarticpsr



stew$icpsr[stew$stewarticpsr==14846 & stew$name=="Molinari, Susan" ]<-15639 #Susan Molinari replaced G Molanari a few sessions earlier

stew$icpsr[stew$stewarticpsr==15001 & stew$name=="Bartlett, Steve"]<-15002  #Both Steve Bartlett and Michael A. Andrews.  Exile case are for Michael A. Andrews. 15001 correct for Andrews.  

stew$icpsr[stew$stewarticpsr==15000 & stew$name=="Andrews, Michael A."]<-15001  #Gary Ackerman and Michael A. Andrews.  Andrews should be 15001 according to voteview

stew$icpsr[stew$stewarticpsr==15012 & stew$name=="Bryant, John"]<-15013  # John Bryant miscoded to share with Congressman Britt for one cycle.  Bryant should be 15013 according to voteview.  

stew$icpsr[stew$stewarticpsr==15025 & stew$name=="Gekas, George W."]<-15026  # Congressman Gekas miscoded.  Should be 15026 according to voteview.  

stew$icpsr[stew$stewarticpsr==15027 & stew$name=="Johnson, Nancy L."]<-15028  # Congresswoman Johnson miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15043 & stew$name=="Mollohan, Alan B."]<-15083  # Congressman Mollohan miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15058 & stew$name=="Sikorski, Gerry"]<-15059  # Congressman Sikorski miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15065 & stew$name=="Sundquist, Don"]<-15066  # Congressman Sundquist miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15067 & stew$name=="Tallon, Robin"]<-15068  # Congressman Robin Tallon miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15074 & stew$name=="Vucanovich, Barbara F."]<-15075  # Congresswoman Vucanovich, Barbara F. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15079 & stew$name=="Burton, Sala"]<-15080  # Congresswoman Burton, Sala miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15080 & stew$name=="Darden, George W. (Buddy)"]<-15081  # Congressman "Darden, George W. (Buddy)" miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15080 & stew$name=="Ackerman, Gary L."]<-15000  # Congressman Ackerman, Gary L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15081 & stew$name=="Hayes, Charles A."]<-15079  # Congressman Hayes, Charles A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15093 & stew$name=="Collins, James M."]<-11066  # Congressman Collins, James M. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15200 & stew$name=="Armey, Richard"]<-15125  # Congressman Armey, Richard miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15201 & stew$name=="Atkins, Chester G."]<-15084  # Congressman Atkins, Chester G. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==11066& stew$name=="Combest, Larry"]<-15093 ## Congressman Larry Combest miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15202& stew$name=="Barton, Joe L."]<-15085 ## Congressman Barton, Joe L. miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15203& stew$name=="Bentley, Helen Delich"]<-15086 ## Congressman Bentley, Helen Delich miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15204& stew$name=="Boulter, Beau"]<-15087 ## Congressman Boulter, Beau miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15205& stew$name=="Bruce, Terry L."]<-15088 ## Congressman Bruce, Terry L. miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15206& stew$name=="Bustamante, Albert G."]<-15089 ## Congressman Bustamante, Albert G. miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15207& stew$name=="Callahan, H. L. (Sonny)"]<-15090 ## Congressman Callahan, H. L. (Sonny)  miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15208& stew$name=="Cobey, William W. Jr."]<-15091 ## Congressman Cobey, William W. Jr.  miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15209& stew$name=="Coble, Howard"]<-15092 ## Congressman Coble, Howard miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15211& stew$name=="DeLay, Thomas D."]<-15094 ## Congressman DeLay, Thomas D. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15212& stew$name=="DioGuardi, Joseph J."]<-15095 ## Congressman DioGuardi, Joseph J. miscoded. Recoded to match voteview.  

stew$icpsr[stew$stewarticpsr==15213& stew$name=="Eckert, Fred J."]<-15097 ## Congressman Eckert, Fred J. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15214& stew$name=="Fawell, Harris W."]<-15098 ## Congressman Fawell, Harris W. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15215& stew$name=="Gallo, Dean A."]<-15099 ## Congressman Gallo, Dean A. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15216& stew$name=="Gordon, Bart"]<-15100 ## Congressman Gordon, Bart miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15217& stew$name=="Grotberg, John E."]<-15102 ## Congressman Grotberg, John E. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15219& stew$name=="Henry, Paul B."]<-15103 ## Congressman Henry, Paul B. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15220& stew$name=="Kanjorski, Paul E."]<-15104 ## Congressman Kanjorski, Paul E. miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15221& stew$name=="Kolbe, Jim"]<-15105 ## Congressman Kolbe, Jimmiscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15222& stew$name=="Lightfoot, Jim Ross"]<-15106 ## Congressman Lightfoot, Jim Ross miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15223& stew$name=="Manton, Thomas J."]<-15107 ## Congressman Lightfoot, Jim Ross miscoded. Recoded to match voteview. 

stew$icpsr[stew$stewarticpsr==15224& stew$name=="McMillan, J. Alex"]<-15108 ## Congressman McMillan, J. Alex miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15225& stew$name=="Meyers, Jan"]<-15109 ## Congressman Meyers, Jan miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15225& stew$name=="Jontz, James"]<-15426 ## Congressman Jontz, James miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15226& stew$name=="Miller, John R."]<-15110 ## Congressman Miller, John R. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15227& stew$name=="Monson, David S."]<-15111 ## Congressman Monson, David S. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15229& stew$name=="Robinson, Tommy F."]<-15122 ## Congressman Robinson, Tommy F. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15230& stew$name=="Rowland, John G."]<-15057 ## Congressman Rowland, John G. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15231& stew$name=="Saxton, H. James"]<-15112 ## Congressman Saxton, H. James miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15232& stew$name=="Schuette, Bill"]<-15114 ## Congressman Schuette, Bill miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15233& stew$name=="Slaughter, D. French Jr."]<-15115 ## Congressman Slaughter, D. French Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15234& stew$name=="Smith, Robert C."]<-15116 ## Congressman Smith, Robert C. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15235& stew$name=="Stallings, Richard H."]<-15117 ## Congressman Stallings, Richard H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15236& stew$name=="Strang, Michael L."]<-15118 ## Congressman Strang, Michael L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15237& stew$name=="Sweeney, Mac"]<-15119 ## Congressman Sweeney, Mac miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15238& stew$name=="Swindall, Patrick Lynn"]<-15120 ## Congressman Swindall, Patrick Lynn miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15239& stew$name=="Traficant, James A. Jr."]<-15121 ## Congressman Traficant, James A. Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15240& stew$name=="Visclosky, Peter J."]<-15124 ## Congressman Visclosky, Peter J. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15241& stew$name=="Long, Cathy"]<-15128 ## Congressman Long, Cathy miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15242& stew$name=="Chapman, Jim"]<-15129 ## Congressman Chapman, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15243& stew$name=="Waldon, Alton R."]<-15244 ## Congressman Waldon, Alton R. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15244& stew$name=="Abercrombie, Neil"]<-15245 ## Congressman Abercrombie, Neil miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15400& stew$name=="Baker, Richard H."]<-15401 ## Congressman Baker, Richard H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15401& stew$name=="Ballenger, Cass"]<-15402 ## Congressman Ballenger, Cass miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15402& stew$name=="Bilbray, James H."]<-15403 ## Congressman Bilbray, James H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15403& stew$name=="Brennan, Joseph E."]<-15404 ## Congressman Brennan, Joseph E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15404& stew$name=="Buechner, Jack"]<-15405 ## Congressman Buechner, Jack miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15405& stew$name=="Bunning, Jim"]<-15406 ## Congressman Bunning, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15406& stew$name=="Campbell, Ben Nighthorse"]<-15407 ## Campbell, Ben Nighthorse midcoded.  Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15407& stew$name=="Cardin, Benjamin L."]<-15408 ## Congressman Cardin, Benjamin L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15408& stew$name=="Davis, Jack"]<-15409 ## Congressman  Davis, Jack miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15409& stew$name=="DeFazio, Peter A."]<-15410 ## Congressman DeFazio, Peter A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15410& stew$name=="Espy, Mike"]<-15411 ## Congressman Espy, Mike miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15411& stew$name=="Flake, Floyd H."]<-15412 ## Congressman Flake, Floyd H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15412& stew$name=="Gallegly, Elton"]<-15413 ## Congressman Gallegly, Elton miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15413& stew$name=="Grandy, Fred"]<-15414 ## Congressman Grandy, Fred miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15414& stew$name=="Grant, Bill" & stew$Congress==100 ]<-95415 ## Congressman Grant, Bill miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15414& stew$name=="Grant, Bill"& stew$Congress==101 ]<-15415 ## Congressman Grant, Bill miscoded. Recoded to match voteview. 



stew$icpsr[stew$stewarticpsr==15416& stew$name=="Hastert, J. Dennis"]<-15417 ## Congressman Hastert, J. Dennis miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15417& stew$name=="Hayes, James A."]<-15418 ## Congressman Hayes, James A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15418& stew$name=="Hefley, Joel"]<-15419 ## Congressman Hefley, Joel. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15419& stew$name=="Herger, Wally"]<-15420 ## Congressman Herger, Wally miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15420& stew$name=="Hochbrueckner, George J."]<-15421 ## Congressman Hochbrueckner, George J. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15421& stew$name=="Holloway, Clyde C."]<-15422 ## Congressman Holloway, Clyde C. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15422& stew$name=="Houghton, Amory Jr."]<-15423 ## Congressman Houghton, Amory Jr.. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15423& stew$name=="Inhofe, James M."]<-15424 ## Congressman Inhofe, James M. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15424& stew$name=="Johnson, Tim"]<-15425 ## Congressman Johnson, Tim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15425& stew$name=="Jontz, James"]<-15426 ## Congressman Jontz, James miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15426& stew$name=="Kennedy, Joseph P. II"]<-15427 ## Congressman Kennedy, Joseph P. II miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15427& stew$name=="Konnyu, Ernest L."]<-15428 ## Congressman Konnyu, Ernest L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15428& stew$name=="Kyl, Jon"]<-15429 ## Congressman Kyl, Jon miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15429& stew$name=="Lancaster, H. Martin"]<-15430 ## Congressman Lancaster, H. Martin miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15430& stew$name=="Lewis, John"]<-15431 ## Congressman Lewis, John miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15431& stew$name=="McMillen, C. Thomas"]<-15432 ## Congressman McMillen, C. Thomas miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15432& stew$name=="Mfume, Kweisi"]<-15433 ## Congressman Mfume, Kweisi miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15433& stew$name=="Morella, Constance A."]<-15434 ## Congressman Morella, Constance A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15434& stew$name=="Nagle, David R."]<-15435 ## Congressman Nagle, David R. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15435& stew$name=="Patterson, Elizabeth J."]<-15171 ## Congressman Patterson, Elizabeth J. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15436& stew$name=="Pickett, Owen B."]<-15437 ## Congressman Pickett, Owen B. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15437& stew$name=="Price, David E."]<-15438 ## Congressman Price, David E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15438& stew$name=="Ravenel, Arthur Jr."]<-15439 ## Congressman Ravenel, Arthur Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15439& stew$name=="Rhodes, John J. III"]<-15440 ## Congressman Rhodes, John J. III miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15440& stew$name=="Saiki, Patricia F."]<-15441 ## Congressman Saiki, Patricia F. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15441& stew$name=="Sawyer, Thomas C."]<-15442 ## Congressman Sawyer, Thomas C. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15442& stew$name=="Skaggs, David E."]<-15443 ## Congressman Skaggs, David E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15443& stew$name=="Slaughter, Louise M."]<-15444 ## Congressman Slaughter, Louise M. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15444& stew$name=="Smith, Lamar S."]<-15445 ## Congressman Smith, Lamar S. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15445& stew$name=="Upton, Frederick S."]<-15446 ## Congressman Upton, Frederick S. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15446& stew$name=="Weldon, Curt"]<-15447 ## Congressman Weldon, Curt miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15447& stew$name=="Pelosi, Nancy"]<-15448 ## Congressman Pelosi, Nancy miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15448& stew$name=="Shays, Christopher"]<-15449 ## Congressman Shays, Christopher miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15449& stew$name=="Clement, Bob"]<-15450 ## Congressman Clement, Bob miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15450& stew$name=="McCrery, Jim"]<-15451 ## Congressman McCrery, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15451& stew$name=="Payne, Lewis F. Jr."]<-15452 ## Congressman Payne, Lewis F. Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15452& stew$name=="Costello, Jerry F."]<-15453 ## Congressman Costello, Jerry F. miscoded. Recoded to match voteview.





stew$icpsr[stew$stewarticpsr==15610& stew$name=="James, Craig T."]<-15608 ## Congressman James, Craig T. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15625& stew$name=="Sangmeister, George E."]<-15622 ## Congressman Sangmeister, George E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15626& stew$name=="Sarpalius, Bill"]<-15623 ## Congressman Sarpalius, Bill miscoded. Recoded to match voteview.



stew$icpsr[stew$stewarticpsr==15632& stew$name=="Unsoeld, Jolene"]<-15629 ## Congresswoman Unsoeld, Jolene miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15633& stew$name=="Walsh, James T."]<-15630 ## Congressman Walsh, James T. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15638& stew$name=="Condit, Gary"]<-15635 ## Congressman Condit, Gary miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15640& stew$name=="Taylor, Gene (MS)"]<-15637 ## Congressman Taylor, Gene (MS) miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15641& stew$name=="Washington, Craig"]<-29145 ## Congressman Washington, Craig miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15642& stew$name=="Molinari, Susan"]<-15639 ## Congresswoman Molinari, Susan miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15643& stew$name=="Serrano, Jose E."]<-29134 ## Congressman Serrano, Jose E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15801& stew$name=="Allard, Wayne"]<-29108 ## Congressman Allard, Wayne miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15802& stew$name=="Andrews, Robert E."]<-29132 ## Congressman Andrews, Robert E. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15803& stew$name=="Andrews, Thomas H."]<-29121 ## Congressman Andrews, Thomas H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15804& stew$name=="Bacchus, Jim"]<-29112 ## Congressman Bacchus, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15805& stew$name=="Barrett, Bill"]<-29129 ## Congressman Barrett, Bill miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15806& stew$name=="Boehner, John A."]<-29137 ## Congressman Boehner, John A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15807& stew$name=="Brewster, Bill"]<-29138 ## Congressman Brewster, Bill miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15808& stew$name=="Camp, Dave"]<-29124 ## Congressman Camp, Dave miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15809& stew$name=="Collins, Barbara-Rose"]<-29125 ## Congresswoman Collins, Barbara-Rose miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15810& stew$name=="Cox, John W. Jr."]<-29116 ## Congressman Cox, John W. Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15811& stew$name=="Cramer, Bud"]<-29100 ## Congressman Cramer, Bud miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15812& stew$name=="Cunningham, Randy"]<-29107 ## Congressman Cunningham, Randy miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15813& stew$name=="DeLauro, Rosa"]<-29109 ## Congresswoman Cunningham, Randy miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15814& stew$name=="Dooley, Calvin"]<-29105 ## Congressman Dooley, Calvin miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15815& stew$name=="Doolittle, John T."]<-29104 ## Congressman Doolittle, John T. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15816& stew$name=="Edwards, Chet"]<-29144 ## Congressman Edwards, Chet miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15817& stew$name=="Franks, Gary"]<-29110 ## Congressman Franks, Gary miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15818& stew$name=="Gilchrest, Wayne T."]<-29122 ## Congressman Gilchrest, Wayne T. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15819& stew$name=="Hobson, David L."]<-29136 ## Congressman Hobson, David L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15820& stew$name=="Horn, Joan Kelly"]<-29128 ## Congresswoman Horn, Joan Kelly miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15821& stew$name=="Jefferson, William J."]<-29120 ## Congressman Jefferson, William J.miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15822& stew$name=="Klug, Scott L."]<-29150 ## Congressman Klug, Scott L. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15823& stew$name=="Kopetski, Mike"]<-29139 ## Congressman Kopetski, Mike miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15824& stew$name=="LaRocco, Larry"]<-29114 ## Congressman LaRocco, Larry miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15825& stew$name=="Luken, Charles A."]<-15823 ## Congressman Luken, Charles A. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15826& stew$name=="Moran, James P. Jr."]<-29149 ## Congressman Moran, James P. Jr. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15827& stew$name=="Nichols, Dick"]<-29119 ## Congressman Nichols, Dick miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15828& stew$name=="Nussle, Jim"]<-29118 ## Congressman Nussle, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15829& stew$name=="Orton, Bill"]<-29146 ## Congressman Orton, Bill miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15830& stew$name=="Peterson, Collin C."]<-29127 ## Congressman Peterson, Collin C. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15831& stew$name=="Peterson, Pete"]<-29111 ## Congressman Peterson, Pete miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15832& stew$name=="Ramstad, Jim"]<-29126 ## Congressman Ramstad, Jim miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15833& stew$name=="Reed, John F."]<-29142 ## Congressman Reed, John F. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15834& stew$name=="Riggs, Frank"]<-29103 ## Congressman Riggs, Frank miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15835& stew$name=="Roemer, Timothy J."]<-29117 ## Congressman Roemer, Timothy J. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15836& stew$name=="Sanders, Bernard"]<-29147 ## Congressman Sanders, Bernard miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15837& stew$name=="Santorum, Rick"]<-29141 ## Congressman Santorum, Rick miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15838& stew$name=="Swett, Dick"]<-29131 ## Congressman Swett, Dick miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15839& stew$name=="Taylor, Charles H."]<-29135 ## Congressman Taylor, Charles H. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15840& stew$name=="Waters, Maxine"]<-29106 ## Congressman Waters, Maxine miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15841& stew$name=="Zeliff, William"]<-29130 ## Congressman Zeliff, William miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15842& stew$name=="Zimmer, Richard"]<-29133 ## Congressman Zimmer, Richardmiscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15843& stew$name=="Johnson, Sam"]<-29143 ## Congressman Johnson, Sam miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15844& stew$name=="Olver, John W."]<-29123 ## Congressman Olver, John W. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15845& stew$name=="Ewing, Thomas W."]<-29115 ## Congressman Ewing, Thomas W. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15846& stew$name=="Pastor, Ed"]<-29101 ## Congressman Pastor, Ed miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15847& stew$name=="Allen, George F."]<-29148 ## Congressman Allen, George F. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15848& stew$name=="Blackwell, Lucien"]<-29140 ## Congressman Blackwell, Lucien miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==15851& stew$name=="Norton, Eleanor Holmes"]<-70303 ## Congressman Norton, Eleanor Holmesmiscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==3769& stew$name=="Gray, Kenneth J."]<-15101 ## Congressman Gray, Kenneth J. miscoded. Recoded to match voteview.

stew$icpsr[stew$stewarticpsr==90327& stew$name=="Alexander, Rodney" & stew$Congress==108]<-20327 ## Congressman Alexander, Rodney switched parties.  This was year before switch.

stew$icpsr[stew$stewarticpsr==15406& stew$name=="Campbell, Ben Nighthorse"]<-15407 ## Campbell, Ben Nighthorse midcoded.  Recoded to match voteview.



### Note: Tried using Adler and Wilkerson's ICPSR Crosswalk (Congressional Bills Project) to convert alternate icpsr scores to NOMINATE scores.  But their alternate scores didn't match stewart's icpsr scores. 


### Adding missing committee leadership positions

### Source: CQ's Politics in America 

stew$seniorstatus[stew$icpsr==14873 & stew$cong==106 & stew$commcode==142] <-21 ## Adding Steny Hoyer Ranking Member House Admin

stew$seniorstatus[stew$icpsr==14039 & stew$cong==106 & stew$commcode==176] <-21 ## Adding John Moakley Ranking Member Rules

###################################################
# FIXME 
###################################################
# need to add dates to this and should not be indexed 
# stew[nrow(stew)+1,]<-c(106,14620, "Dixon, Julian C.",71,32,100,21,1,242,14620) ## Adding Julian Dixon as Ranking Member House Select Intelligence

# \FIXME ##########################################


stew$seniorstatus[stew$icpsr==15448 & stew$cong==106 & stew$commcode==242] <-0 ## Removing Pelosi who wasn't ranking member of House Select Intelligence

stew$seniorstatus[stew$icpsr==15005 & stew$cong==106 & stew$commcode==186] <-21 ## Adding Howard Berman Ranking Member Standards of Official Conduct



stew$cong<-as.numeric(stew$cong)

stew$stewarticpsr<-as.numeric(stew$stewarticpsr)

# stew$state<-as.numeric(stew$state)

stew$cd<-as.numeric(stew$cd)

stew$party<-as.numeric(stew$party)

stew$seniorstatus<-as.numeric(stew$seniorstatus)

# stew$chamber<-as.numeric(stew$chamber)

stew$commcode<-as.numeric(stew$commcode)

stew$icpsr<-as.numeric(stew$icpsr)



### variable to merge with 

electionlist<-c(seq(from=1978, to=2016, by=2))

conglist<-c(seq(from=96, to=115, by=1))

stew$yearelected<-NA

for (i in 1:length(electionlist)){
  
  stew$yearelected[stew$cong==conglist[i]]<-electionlist[i]
  
}



stew$ICPSRYear<-paste(stew$icpsr, stew$yearelected, sep="")


### Dropping "[Vacant]" entries
# stew<-stew[stew$name!="[Vacant]",]

committees <- filter(stew, congress > 105)
committees$congress %<>% as.numeric()
committees$assigneddate %<>% as.Date()
committees$terminationdate %<>% as.Date()



###############################################################
# THE BELOW IS ONLY FOR IDENTIFYING ERRORS IN COMMITTEE DATA #
##############################################################

committees %<>%
  mutate(party = ifelse(name == "Byrne, Bradley", 200, party)) %>% 
  mutate(party = ifelse(name == "Johnson, Tim", 100, party)) %>% 
  mutate(party = ifelse(name == "Johnson, Bill", 200, party)) %>% 
  mutate(party = ifelse(name == "Davis, Rodney", 200, party)) %>% 
  mutate(party = ifelse(name == "Specter, Arlen" & assigneddate < as.Date("2009-04-28"), 200, party)) %>% 
  mutate(party = ifelse(name == "Turner, Bob L.", 200, party)) %>% 
  
  
  mutate(seniorstatus = ifelse(name == "Waters, Maxine" & assigneddate >= as.Date("2015-01-06"), 21, seniorstatus)) %>% 
  mutate(seniorstatus = ifelse(name == "Brown, Corrine" & assigneddate == as.Date("2015-01-06"), 22, seniorstatus)) %>% 
  mutate(seniorstatus = ifelse(name == "Stark, Fortney Pete" & assigneddate == as.Date("2009-01-07"), 0, seniorstatus)) %>% 
  mutate(seniorstatus = ifelse(name == "Coats, Dan" & assigneddate == as.Date("2015-01-07") & committeename== "Economic (Joint Committee)", 11, seniorstatus)) %>% 
  
  mutate(terminationdate = if_else(name =="Rangel, Charles B." & assigneddate == as.Date('2009-01-06')& committeename == "Ways and Means",
                                as.Date('2010-03-03'), terminationdate)) %>% 
  mutate(assigneddate = if_else(name =="Levin, Sander M." & assigneddate == as.Date('2009-01-07')& committeename == "Ways and Means",
                                   as.Date('2010-03-04'), assigneddate)) %>% 
  
  mutate(assigneddate = if_else(name =="Brady, Kevin" & assigneddate == as.Date('2015-01-13')& committeename == "Ways and Means",
                               as.Date('2015-10-29'), assigneddate))
# FIXME 





committee.membership <- left_join(member_search(congress = c(110:120)), committees)

bad.committee.match <- committee.membership[is.na(committee.membership$stewarticpsr),]

