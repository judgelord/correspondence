fix.member.date.coding <- function(d){
  d %<>% 
    mutate(bioname = ifelse(is.na(bioname), "", bioname)) %>% 
    mutate(party_name = ifelse(is.na(party_name), "", party_name)) %>% 
    mutate(chamber = ifelse(is.na(chamber), "", chamber)) %>% 
    # MENENDEZ, Robert appointed to senate on jan 16, 2006
    filter(!(bioname == "MENENDEZ, Robert" & chamber == "House" & DATE > as.Date("2006-01-16"))) %>% #  not in house after 2006
    filter(!(bioname == "MENENDEZ, Robert" & chamber == "Senate" & DATE < as.Date("2006-01-16"))) %>%  # not in senate before 2006
    filter(bioname != "PAYNE, Donald Milford" | DATE < as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
    filter(bioname != "PAYNE, Donald, Jr." | DATE > as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
    # mutate(icpsr = ifelse(last_name == "PAYNE" & DATE < as.Date("2012-06-03"), 15619, icpsr) ) %>% 
    # mutate(icpsr = ifelse(last_name == "PAYNE" & DATE > as.Date("2012-06-03"), 31103, icpsr) ) %>% 
    filter(!(bioname == "SPECTER, Arlen" & DATE > as.Date("2009-04-28") & party_name == "Republican Party")) %>% # SPECTER, Arlen changed to DEM
    filter(!(bioname == "SPECTER, Arlen" & DATE < as.Date("2009-04-28") & party_name == "Democratic Party")) %>% # however, voteview has two icpsr #s, so may not need to delete obs if successful merge
    filter(!(bioname == "GOODE, Virgil H., Jr." & DATE < as.Date("2000-01-01") & party_name == "Republican Party")) %>% # Virgil H. Goode, Jr.
    filter(!(bioname == "GOODE, Virgil H., Jr." & DATE > as.Date("2000-01-01") & party_name == "Independent")) %>% 
    filter(bioname != "MARKEY, Edward John" | chamber != "House" | DATE < as.Date("2013-06-25")) %>% # # Rep Ed Markey elected to Senate in special election June 25, 2013
    filter(bioname != "MARKEY, Edward John" | chamber != "Senate" | DATE > as.Date("2013-06-25")) %>% 
    filter(!(bioname == "KIRK, Mark Steven" & DATE > as.Date("2010-11-29") & chamber == "House")) %>% # Went from House to Senate, filled in Obama's vacancy in Senate when he was president elect
    filter(!(bioname == "KIRK, Mark Steven" & DATE < as.Date("2010-11-29") & chamber == "Senate")) %>% 
    filter(!(bioname == "HELLER, Dean" & DATE > as.Date("2011-05-09") & chamber == "House")) %>% # Went from House to Senate, filled a Senate vacancy 
    filter(!(bioname == "HELLER, Dean" & DATE < as.Date("2011-05-09") & chamber == "Senate")) %>% 
    filter(!(bioname == "WICKER, Roger F." & DATE > as.Date("2007-12-31") & chamber == "House")) %>% # Went from House to Senate, filled a Senate vacancy 
    filter(!(bioname == "WICKER, Roger F." & DATE < as.Date("2007-12-31") & chamber == "Senate")) %>% 
    filter(!(bioname == "GRIFFITH, Parker" & DATE > as.Date("2009-12-22") & party_name == "Democratic Party")) %>% # GRIFFITH, Parker changed to GOP
    filter(!(bioname == "GRIFFITH, Parker" & DATE < as.Date("2009-12-22") & party_name == "Republican Party")) %>% 
    filter(!(bioname == "GILLIBRAND, Kirsten" & DATE > as.Date("2009-01-26") & chamber == "House")) %>% # GILLIBRAND APPOINTED TO SENATE FROM HOUSE January 26, 2009
    filter(!(bioname == "GILLIBRAND, Kirsten"& DATE < as.Date("2009-01-26") & chamber == "Senate")) %>% 
    # December 19, 2019, Van Drew announced that he would be joining the Republican Party
    filter(!(bioname == "VAN DREW, Jefferson" & DATE < as.Date("2019-12-19") & party_name == "Republican Party")) %>% # not gop before 2019
    filter(!(bioname == "VAN DREW, Jefferson" & DATE > as.Date("2019-12-19") & party_name == "Democratic Party")) %>% # not dem after 2019
    # Jul 4, 2019 - Michigan Rep. Justin Amash announced Thursday he was leaving the Republican
    filter(!(bioname == "AMASH, Justin" & DATE > as.Date("2019-07-04") & party_name == "Republican Party")) %>% # not gop after 2019
    filter(!(bioname == "AMASH, Justin" & DATE < as.Date("2019-07-04") & party_name == "Independent")) %>% # not ind before 2019
    # On May 24, 2001, Jeffords left the Republican Party, with which he had always been affiliated, and announced his new status as an independent.
    filter(!(bioname == "JEFFORDS, James Merrill" & DATE > as.Date("2001-05-24") & party_name == "Republican Party")) %>% # not gop after 2001
    filter(!(bioname == "JEFFORDS, James Merrill" & DATE < as.Date("2001-05-24") & party_name == "Independent")) # not ind before 2001
    # 
    

  # LIEBERMAN Indepedent in Committees, Democrat in voteview data. Voteview data will override, which is fine (no need to fix)
  # 
}

# congress bioname                 party_code icpsr
# <chr>    <chr>                   <chr>      <chr>
#   1 107      JEFFORDS, James Merrill 328;;;200  94240;;;14240
# 2 111      SPECTER, Arlen          100;;;200  94910;;;14910
# 3 111      GRIFFITH, Parker        200;;;100  90901;;;20901
# 4 116      VAN DREW, Jefferson     200;;;100  91980;;;21980
# 5 116      AMASH, Justin           328;;;200  91143;;;21143
