fix.member.date.coding <- function(data){
  data %<>% 
    mutate(bioname = ifelse(is.na(bioname), "", bioname)) %>% 
    mutate(party_name = ifelse(is.na(party_name), "", party_name)) %>% 
    mutate(chamber = ifelse(is.na(chamber), "", chamber)) %>% 
    filter(bioname != "PAYNE, Donald Milford" | DATE < as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
    filter(bioname != "PAYNE, Donald, Jr." | DATE > as.Date("2012-06-03")) %>% # PAYNE Sr. died, replaced by PAYNE Jr.
    # mutate(icpsr = ifelse(last_name == "PAYNE" & DATE < as.Date("2012-06-03"), 15619, icpsr) ) %>% 
    # mutate(icpsr = ifelse(last_name == "PAYNE" & DATE > as.Date("2012-06-03"), 31103, icpsr) ) %>% 
    filter(!(bioname == "SPECTER, Arlen" & DATE > as.Date("2009-04-28") & party_name == "Republican Party")) %>% # SPECTER, Arlen changed to DEM
    filter(!(bioname == "SPECTER, Arlen" & DATE < as.Date("2009-04-28") & party_name == "Democratic Party")) %>% # however, voteview has two icpsr #s, so may not need to delete obs if successful merge
    filter(!(bioname == "GOODE, Virgil H., Jr." & DATE < as.Date("2000-01-01") & party_name == "Republican Party")) %>% # Virgil H. Goode, Jr.
    filter(!(bioname == "GOODE, Virgil H., Jr." & DATE > as.Date("2000-01-01") & party_name == "Democratic Party")) %>% 
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
    filter(!(bioname == "GILLIBRAND, Kirsten"& DATE < as.Date("2009-01-26") & chamber == "Senate"))
  
  # LIEBERMAN Indepedent in Committees, Democrat in voteview data. Voteview data will override, which is fine (no need to fix)
  # 
}