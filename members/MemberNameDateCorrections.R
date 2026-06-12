# > # people that need fixing in fix.memberdate.coding() from MemberNameDateCorrections.R 
#   > # party corrections 
#   > d |> distinct(icpsr, bioname,congress, party_name) |> 
#   +   group_by(congress, bioname) |> 
#   +   add_count() |> 
#   +   filter(congress >104, n > 1)
# # A tibble: 18 × 5
# # Groups:   congress, bioname [9]
# icpsr bioname                         congress party_name           n
# <dbl> <chr>                              <dbl> <chr>            <int>
#   1 14910 SPECTER, Arlen                       111 Republican Party     2
# 2 94910 SPECTER, Arlen                       111 Democratic Party     2
# 3 21980 VAN DREW, Jefferson                  116 Democratic Party     2
# 4 91980 VAN DREW, Jefferson                  116 Republican Party     2
# 5 21996 SABLAN, Gregorio Kilili Camacho      111 Democratic Party     2
# 6 91996 SABLAN, Gregorio Kilili Camacho      111 Independent          2
# 7 20901 GRIFFITH, Parker                     111 Democratic Party     2
# 8 90901 GRIFFITH, Parker                     111 Republican Party     2
# 9 20964 PIERLUISI, Pedro                     111 Independent          2
# 10 90964 PIERLUISI, Pedro                     111 Democratic Party     2
# 11 21143 AMASH, Justin                        116 Republican Party     2
# 12 91143 AMASH, Justin                        116 Independent          2
# 13 89767 GOODE, Virgil H., Jr.                107 Republican Party     2
# 14 99767 GOODE, Virgil H., Jr.                107 Independent          2
# 15 14240 JEFFORDS, James Merrill              107 Republican Party     2
# 16 94240 JEFFORDS, James Merrill              107 Independent          2
# 17 29767 GOODE, Virgil H., Jr.                106 Democratic Party     2
# 18 99767 GOODE, Virgil H., Jr.                106 Independent          2
# > # chamber switchers 
#   > d |> distinct(icpsr, bioname,congress, chamber) |> 
#   +   group_by(congress, bioname) |> 
#   +   add_count() |> 
#   +   filter(congress >104, n > 1)
# # A tibble: 30 × 5
# # Groups:   congress, bioname [15]
# icpsr bioname             congress chamber     n
# <dbl> <chr>                  <dbl> <chr>   <int>
#   1 20735 GILLIBRAND, Kirsten      111 Senate      2
# 2 20735 GILLIBRAND, Kirsten      111 House       2
# 3 20115 KIRK, Mark Steven        111 Senate      2
# 4 20115 KIRK, Mark Steven        111 House       2
# 5 14910 SPECTER, Arlen           111 Senate      2
# 6 94910 SPECTER, Arlen           111 Senate      2
# 7 20730 HELLER, Dean             112 Senate      2
# 8 20730 HELLER, Dean             112 House       2
# 9 14435 MARKEY, Edward John      113 Senate      2
# 10 14435 MARKEY, Edward John      113 House       2
# # ℹ 20 more rows
# # ℹ Use `print(n = ...)` to see more rows
# > # other corrections 
#   > d |> distinct(icpsr, bioname) |> 
#   +   filter(str_detect(bioname, "PAYNE"))
# # A tibble: 2 × 2
# icpsr bioname              
# <dbl> <chr>                
#   1 31103 PAYNE, Donald, Jr.   
# 2 15619 PAYNE, Donald Milford

fix.member.date.coding <- function(d){
  
  d %<>% 
    # Corrections: 
    
    # GRIFFITH, Parker changed to GOP
    filter(!(bioname == "GRIFFITH, Parker" &
               DATE >= as.Date("2009-12-22") &
               icpsr == 20901)) %>% # no Dem ICPSR after switch
    filter(!(bioname == "GRIFFITH, Parker" &
               DATE < as.Date("2009-12-22") &
               icpsr == 90901)) %>% # no GOP ICPSR before switch
    
    # SPECTER, Arlen changed to DEM
    filter(!(bioname == "SPECTER, Arlen" &
               DATE >= as.Date("2009-04-28") &
               icpsr == 14910)) %>% # not GOP ICPSR after switch
    filter(!(bioname == "SPECTER, Arlen" &
               DATE < as.Date("2009-04-28") &
               icpsr == 94910)) %>% # not Dem ICPSR before switch
    
    # MENENDEZ, Robert appointed to Senate on Jan 16, 2006
    filter(!(bioname == "MENENDEZ, Robert" &
               chamber == "House" &
               DATE >= as.Date("2006-01-16"))) %>% 
    filter(!(bioname == "MENENDEZ, Robert" &
               chamber == "Senate" &
               DATE < as.Date("2006-01-16"))) %>%  
    
    # PAYNE Sr. died, replaced by PAYNE Jr.
    filter(bioname != "PAYNE, Donald Milford" |
             DATE < as.Date("2012-06-03")) %>% 
    filter(bioname != "PAYNE, Donald, Jr." |
             DATE >= as.Date("2012-06-03")) %>% 
    
    # GOODE, Virgil H., Jr.
    filter(!(bioname == "GOODE, Virgil H., Jr." &
               DATE < as.Date("2000-01-01") &
               icpsr == 89767)) %>% # no Republican ICPSR before 2000
    filter(!(bioname == "GOODE, Virgil H., Jr." &
               DATE >= as.Date("2000-01-01") &
               icpsr == 99767)) %>% # no Independent ICPSR after 2000
    
    # Rep Ed Markey elected to Senate in special election June 25, 2013
    filter(!(bioname == "MARKEY, Edward John" &
             chamber == "House" &
             DATE > as.Date("2013-06-25"))) %>% 
    filter(!(bioname == "MARKEY, Edward John" &
             chamber == "Senate" &
             DATE <= as.Date("2013-06-25"))) %>% 
    
    # Mark Kirk went from House to Senate, filled Obama's vacancy
    filter(!(bioname == "KIRK, Mark Steven" &
               DATE > as.Date("2010-11-29") &
               chamber == "House")) %>% 
    filter(!(bioname == "KIRK, Mark Steven" &
               DATE <= as.Date("2010-11-29") &
               chamber == "Senate")) %>% 
    
    # Dean Heller went from House to Senate
    filter(!(bioname == "HELLER, Dean" &
               DATE > as.Date("2011-05-09") &
               chamber == "House")) %>% 
    filter(!(bioname == "HELLER, Dean" &
               DATE <= as.Date("2011-05-09") &
               chamber == "Senate")) %>% 
    
    # Roger Wicker went from House to Senate
    filter(!(bioname == "WICKER, Roger F." &
               DATE > as.Date("2007-12-31") &
               chamber == "House")) %>% 
    filter(!(bioname == "WICKER, Roger F." &
               DATE <= as.Date("2007-12-31") &
               chamber == "Senate")) %>% 
    
    # Gillibrand appointed to Senate from House January 26, 2009
    filter(!(bioname == "GILLIBRAND, Kirsten" &
               DATE > as.Date("2009-01-26") &
               chamber == "House")) %>% 
    filter(!(bioname == "GILLIBRAND, Kirsten" &
               DATE <= as.Date("2009-01-26") &
               chamber == "Senate")) %>% 
    
    # Van Drew announced that he would join the Republican Party on Dec. 19, 2019
    filter(!(bioname == "VAN DREW, Jefferson" &
               DATE < as.Date("2019-12-19") &
               icpsr == 91980)) %>% # no GOP ICPSR before switch
    filter(!(bioname == "VAN DREW, Jefferson" &
               DATE >= as.Date("2019-12-19") &
               icpsr == 21980)) %>% # no Dem ICPSR after switch
    
    # Justin Amash left the Republican Party on July 4, 2019
    filter(!(bioname == "AMASH, Justin" &
               DATE > as.Date("2019-07-04") &
               icpsr == 21143)) %>% # no GOP ICPSR after switch
    filter(!(bioname == "AMASH, Justin" &
               DATE <= as.Date("2019-07-04") &
               icpsr == 91143)) %>% # no Independent ICPSR before switch
    
    # Jeffords left the Republican Party on May 24, 2001
    filter(!(bioname == "JEFFORDS, James Merrill" &
               DATE > as.Date("2001-05-24") &
               icpsr == 14240)) %>% # no GOP ICPSR after switch
    filter(!(bioname == "JEFFORDS, James Merrill" &
               DATE <= as.Date("2001-05-24") &
               icpsr == 94240))    # no Independent ICPSR before switch
  
  return(d)
  
  # LIEBERMAN Independent in Committees, Democrat in voteview data.
  # Voteview data will override, which is fine.
}


## LETTERS ON THE EXACT DAY THEY SWITCHED 
# 1 111      SPECTER, Arl… Democratic Party;;;Republ… 94910;;;… HUD_HQ  2009-04-28
# 2 111      GRIFFITH, Pa… Republican Party;;;Democr… 90901;;;… DOL_OW… 2009-12-22
# 3 111      GRIFFITH, Pa… Republican Party;;;Democr… 90901;;;… DOL_SOL 2009-12-22

