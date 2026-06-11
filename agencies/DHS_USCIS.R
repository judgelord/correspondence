# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# source("setup.R")
#file.name <- "DHS_USCIS" # for testing

clean <- function(file.name) {
  data_raw <- gs_title(file.name) %>% gs_read()
  
  # LetterID = sheet row number
  data_raw$LetterID <- 1:nrow(data_raw)
  
  # select distinct observations 
  data_distinct <- data_raw %>% select(-LetterID) %>% distinct()

  ##########################################################
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data_raw) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  data$ID <- seq(1:nrow(data))
  
  data %<>% 
    mutate(FROM = `Primary Member of Congress`,
           DATE = `Received Date`,
           SUBJECT = paste(
             `Inquiry Type`,
             `Contact Method`,
             `Primary Inquiry Issue`,
             `Business Unit (Owning User) (User)`,
             `Primary Inquiry Sub-Issue`,
             `Secondary Inquiry Issue`,
             `Secondary Inquiry Sub-Issue`,
             sep = ";;;")
           )
  # test to make sure we did not creat a bunch of NAs (str_c propegates NAs )
 #  data %>% count(is.na(SUBJECT))
  

  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }  
  
  
  # TYPE CODING 
  
  # There should be a bunch of NA's at the start, since we have not hand coded much
  #   data %>% count(is.na(TYPE))
  
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Briefing Request"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Invitation/Meeting Request"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Legislation"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Technical Guidance Request"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                           str_detect(SUBJECT, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                           str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                           str_detect(SUBJECT, "H2-A"), 2, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Naturalization and Citizenship"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Infopass Assistance"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Permanent Residence"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "B-2 Tourist Visa"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Privacy Release Form"), 1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Policy"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Outreach Event"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "OLA Contact Sheet"), 5, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "H2-B"), 2, TYPE)) %>%
    #this is not working for some reason - may just find the observations that aren't getting captured
    #and hand-code them in the Google sheet
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Deferred Action for Childhood Arrivals (DACA)"), 1, TYPE))
  
  
  # CONSTITUENT_TYPE CODING
  
  #need to code multiple constituent types by hand
  
  #at some point we will want to decide whether some of these employment categories should be 1s or 2s
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "EB-5"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) &str_detect(SUBJECT, "I-140 Immigration Petition for Alien Worker"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-485 (Employment-based) Application to Register Permanent Residence or Adjust Status"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-765 and I-131 Employment Authorization and Advance Parole Combo Card"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-612-J1 Waiver of the Foreign Residence Requirement"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Initial Filing - DACA EAD"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-765 Application for Employment"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Renewal - DACA EAD"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Renewal - Expedite (related to I-765)"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "H1-B Cap"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Non-delivered EADs"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "First Preference EB-1"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Second Preference EB-2"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Adoption"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "EOIR-29 Notice of Appeal to the BIA"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-485 (Family Based) Application to Register Permanent Residence or Adjust Status"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-130 Petition for Alien Relative"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-129F Petition for Alien Fiance(e)"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-730 Refugee/Asylee Relative Petition"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Cuban Family Reunification Program"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-817 Application for Family Unity Benefits"),
                                     "Immigrant-Family", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Asylum"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Refugee"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "T/U Visas"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "VAWA/Battered Spouses"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Humanitarian"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Temporary Protected Status (TPS)"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-918 Petition for U Nonimmigrant Status"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-914 Application for T Nonimmigrant Status"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-131 Application for Travel Document/Advance Parole"),
                                     "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
    #Immigrant-Special types are not showing up - these must already be captured by others?
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-360 Petition for Amerasian, Widow(er), or Special Immigrant"),
                                     "Immigrant-Special", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Religious Worker"),
                                     "Immigrant-Special", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Employment Authorization"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Employment Based Immigration"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE))
  
  
  # TYPE coding based on CONSTITUENT_TYPE
  
  #this should only get a few additional observations - most of the TYPE coding is done above
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Employment"),
                         1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Family"),
                         1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Humanitarian"),
                         1, TYPE))
  
  
  # CONSTITUENT_TYPE coding based on TYPE coding
  
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1") &
                                       str_detect(SUBJECT, "Deferred Action for Childhood Arrivals (DACA)"),
                                     "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1") &
                                       str_detect(SUBJECT, "Deferred Action"),
                                     "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
    #should we include an Immigrant-General type as well??
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1"),
                                     "Immigrant-General", CONSTITUENT_TYPE)) %>%
    #do we want to code all 2s as Immigrant-Employment?
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "2"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE))
  
  # additional TYPE coding - tricky/vague cases
  
  data %<>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Unsettled Immigration Issue"),
                         1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Biometrics"),
                         1, TYPE)) %>%
    #Processing Times - not clear if this is on behalf of indiv. constituent or general complaint?
    #mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Processing Times"),
    #   1, TYPE)) %>%
    #these are also coding a bunch of observations that include "Processing Times" but have already been coded
    #as type 1 because of other phrases - does that matter? how do we use the CERTAINTY and ALT_TYPE columns in analyses?
    #mutate(CERTAINTY = ifelse(is.na(CERTAINTY) & str_detect(SUBJECT, "Processing Times") & TYPE == 1,
    #   3, CERTAINTY)) %>%
    #mutate(ALT_TYPE = ifelse(is.na(ALT_TYPE) & str_detect(SUBJECT, "Processing Times") & TYPE == 1,
    #     5, ALT_TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Travel Documents"),
                         1, TYPE)) %>%
    mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE))
  
  #then need to do CONSTITUENT_TYPE coding again based on TYPE to capture observations coded above
  
  data %<>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1"),
                                     "Immigrant-General", CONSTITUENT_TYPE)) %>%
    mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "2"),
                                     "Immigrant-Employment", CONSTITUENT_TYPE))
  
  # count(data, is.na(CONSTITUENT_TYPE))

  return(data)
}


# FOR TESTING 
if(F){
  
  # if you want to test with a smaller sample

    data %<>% 
      group_by(SUBJECT) %>%
      slice_sample(n = 1) %>% 
      ungroup()
    
    data %>% distinct(SUBJECT,    `Inquiry Type`,
                      `Contact Method`,
                      `Primary Inquiry Issue`,
                      `Business Unit (Owning User) (User)`,
                      `Primary Inquiry Sub-Issue`,
                      `Secondary Inquiry Issue`,
                      `Secondary Inquiry Sub-Issue`) %>% 
      kablebox()
  
  
  
  class(data$district_code)
  
  unique(data$district_code)
  
  class(d1$district_code)
  
  class(members$district_code)
  
  
  
 data %>% count(is.na(icpsr)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()



# TYPE CODING 
  
  # There should be a bunch of NA's at the start, since we have not hand coded much
#   data %>% count(is.na(TYPE))

data %<>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Briefing Request"), 5, TYPE)) %>%
 mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Invitation/Meeting Request"), 5, TYPE)) %>%
 mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Legislation"), 5, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Technical Guidance Request"), 5, TYPE)) %>%
 mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                         str_detect(SUBJECT, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE)) %>%
 mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                         str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"), 2, TYPE)) %>%
 mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework") &
                         str_detect(SUBJECT, "H2-A"), 2, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Casework"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Naturalization and Citizenship"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Infopass Assistance"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Permanent Residence"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "B-2 Tourist Visa"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Privacy Release Form"), 1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Policy"), 5, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Outreach Event"), 5, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "OLA Contact Sheet"), 5, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "H2-B"), 2, TYPE)) %>%
  #this is not working for some reason - may just find the observations that aren't getting captured
  #and hand-code them in the Google sheet
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Deferred Action for Childhood Arrivals (DACA)"), 1, TYPE))
  

# CONSTITUENT_TYPE CODING

#first code observations that are more than one immigrant type:
data %<>%
  #Immigrant-DACA, Immigrant-Employment
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Deferred Action for Childhood Arrivals.*I-765 Application for Employment|I-765 Application for Employment.*Deferred Action for Childhood Arrivals"),
                                   "Immigrant-DACA, Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Deferred Action for Childhood Arrivals.*Employment Authorization|Employment Authorization.*Deferred Action for Childhood Arrivals"),
                                   "Immigrant-DACA, Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Initial Filing - DACA EAD"),
                                   "Immigrant-DACA, Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  #Immigrant-DACA, Immigrant-Humanitarian
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Deferred Action.*Humanitarian|Humanitarian.*Deferred Action"),
                                   "Immigrant-DACA, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  #Immigrant-Employment, Immigrant-Family
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*Family Based|Family Based.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "I-140 Immigration Petition for Alien Worker.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*I-140 Immigration Petition for Alien Worker"),
                                   "Immigrant-Employment, Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment-based.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*Employment-based"),
                                   "Immigrant-Employment, Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment-based.*Family Based|Family Based.*Employment-based"),
                                   "Immigrant-Employment, Immigrant-Family", CONSTITUENT_TYPE)) %>%
  #Immigrant-Employment, Immigrant-Humanitarian
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*Humanitarian|Humanitarian.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*I-918 Petition for U Nonimmigrant Status|I-918 Petition for U Nonimmigrant Status.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*Asylum|Asylum.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Employment Authorization.*Refugee|Refugee.*Employment Authorization"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Humanitarian.*Non-delivered EADs|Non-delivered EADs.*Humanitarian"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "I-918 Petition for U Nonimmigrant Status.*Employment Authorization|Employment Authorization.*I-918 Petition for U Nonimmigrant Status"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Asylum.*Non-delivered EADs|Non-delivered EADs.*Asylum"),
                                   "Immigrant-Employment, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  #Immigrant-Family, Immigrant-Humanitarian
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "I-730 Refugee/Asylee Relative Petition.*Humanitarian|Humanitarian.*I-730 Refugee/Asylee Relative Petition"),
                                   "Immigrant-Family, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "I-130 Petition for Alien Relative.*Humanitarian|Humanitarian.*I-130 Petition for Alien Relative"),
                                   "Immigrant-Family, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "Family Based.*Humanitarian|Humanitarian.*Family Based"),
                                   "Immigrant-Family, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & 
                                     str_detect(SUBJECT, 
                                                "I-130 Petition for Alien Relative.*I-918 Petition for U Nonimmigrant Status|I-918 Petition for U Nonimmigrant Status.*I-130 Petition for Alien Relative"),
                                   "Immigrant-Family, Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, 
                                                                        "Family Based.*Asylum|Asylum.*Family Based"),
                                   "Immigrant-Family, Immigrant-Humanitarian", CONSTITUENT_TYPE))


#at some point we will want to decide whether some of these employment categories should be 1s or 2s
data %<>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "EB-5"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) &str_detect(SUBJECT, "I-140 Immigration Petition for Alien Worker"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-485 (Employment-based) Application to Register Permanent Residence or Adjust Status"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-765 and I-131 Employment Authorization and Advance Parole Combo Card"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-612-J1 Waiver of the Foreign Residence Requirement"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Initial Filing - DACA EAD"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-765 Application for Employment"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Renewal - DACA EAD"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Renewal - Expedite (related to I-765)"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "H1-B Cap"),
                                 "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Non-delivered EADs"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "First Preference EB-1"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Second Preference EB-2"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Adoption"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "EOIR-29 Notice of Appeal to the BIA"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-485 (Family Based) Application to Register Permanent Residence or Adjust Status"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-130 Petition for Alien Relative"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-129F Petition for Alien Fiance(e)"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-730 Refugee/Asylee Relative Petition"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Cuban Family Reunification Program"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-817 Application for Family Unity Benefits"),
                                   "Immigrant-Family", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Asylum"),
                                 "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Refugee"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "T/U Visas"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "VAWA/Battered Spouses"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Humanitarian"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Temporary Protected Status (TPS)"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-918 Petition for U Nonimmigrant Status"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-914 Application for T Nonimmigrant Status"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-131 Application for Travel Document/Advance Parole"),
                                   "Immigrant-Humanitarian", CONSTITUENT_TYPE)) %>%
  #Immigrant-Special types are not showing up - these must already be captured by others?
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "I-360 Petition for Amerasian, Widow(er), or Special Immigrant"),
                                   "Immigrant-Special", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Religious Worker"),
                                   "Immigrant-Special", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Employment Authorization"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(SUBJECT, "Employment Based Immigration"),
                                 "Immigrant-Employment", CONSTITUENT_TYPE))
  

# TYPE coding based on CONSTITUENT_TYPE

#this should only get a few additional observations - most of the TYPE coding is done above
data %<>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Employment"),
                       1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Family"),
                                   1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(CONSTITUENT_TYPE, "Immigrant-Humanitarian"),
                       1, TYPE))


# CONSTITUENT_TYPE coding based on TYPE coding

data %<>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1") &
                                     str_detect(SUBJECT, "Deferred Action for Childhood Arrivals (DACA)"),
                                   "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1") &
                                     str_detect(SUBJECT, "Deferred Action"),
                                   "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
  #should we include an Immigrant-General type as well??
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1"),
                                   "Immigrant-General", CONSTITUENT_TYPE)) %>%
  #do we want to code all 2s as Immigrant-Employment?
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "2"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE))

# additional TYPE coding - tricky/vague cases

data %<>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Unsettled Immigration Issue"),
                       1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Biometrics"),
                       1, TYPE)) %>%
  #Processing Times - not clear if this is on behalf of indiv. constituent or general complaint?
  #mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Processing Times"),
                    #   1, TYPE)) %>%
  #these are also coding a bunch of observations that include "Processing Times" but have already been coded
  #as type 1 because of other phrases - does that matter? how do we use the CERTAINTY and ALT_TYPE columns in analyses?
  #mutate(CERTAINTY = ifelse(is.na(CERTAINTY) & str_detect(SUBJECT, "Processing Times") & TYPE == 1,
                    #   3, CERTAINTY)) %>%
  #mutate(ALT_TYPE = ifelse(is.na(ALT_TYPE) & str_detect(SUBJECT, "Processing Times") & TYPE == 1,
                  #     5, ALT_TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "Travel Documents"),
                       1, TYPE)) %>%
  mutate(TYPE = ifelse(is.na(TYPE) & str_detect(SUBJECT, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE))

#then need to do CONSTITUENT_TYPE coding again based on TYPE to capture observations coded above

data %<>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "1"),
                                   "Immigrant-General", CONSTITUENT_TYPE)) %>%
  mutate(CONSTITUENT_TYPE = ifelse(is.na(CONSTITUENT_TYPE) & str_detect(TYPE, "2"),
                                   "Immigrant-Employment", CONSTITUENT_TYPE))

# count(data, is.na(CONSTITUENT_TYPE))
}


#Code to create csv files containing observations to hand-code for multiple immigrant types:
#ended up not needing this code because we can easily auto-code but I'll keep this here anyway

#Immigrant-DACA and Immigrant-Employment: 
test <- subset(d1, str_detect(SUBJECT, "Initial Filing - DACA EAD"))
test2 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Deferred Action for Childhood Arrivals.*I-765 Application for Employment|I-765 Application for Employment.*Deferred Action for Childhood Arrivals"))
test4 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Deferred Action for Childhood Arrivals.*Employment Authorization|Employment Authorization.*Deferred Action for Childhood Arrivals"))

#Immigrant-DACA and Immigrant-Humanitarian: 
test3 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Deferred Action.*Humanitarian|Humanitarian.*Deferred Action"))

#Immigrant-Employment and Immigrant-Family: 
test5 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*Employment Authorization"))
test8 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*Family Based|Family Based.*Employment Authorization"))
test17 <- subset(d1, 
                str_detect(SUBJECT, 
                           "I-140 Immigration Petition for Alien Worker.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*I-140 Immigration Petition for Alien Worker"))
test20 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment-based.*I-130 Petition for Alien Relative|I-130 Petition for Alien Relative.*Employment-based"))
test21 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment-based.*Family Based|Family Based.*Employment-based"))

#Immigrant-Employment and Immigrant-Humanitarian: 
test6 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*Humanitarian|Humanitarian.*Employment Authorization"))
test7 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*I-918 Petition for U Nonimmigrant Status|I-918 Petition for U Nonimmigrant Status.*Employment Authorization"))
test9 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*Asylum|Asylum.*Employment Authorization"))
test10 <- subset(d1, 
                str_detect(SUBJECT, 
                           "Employment Authorization.*Refugee|Refugee.*Employment Authorization"))
test13 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "Humanitarian.*Non-delivered EADs|Non-delivered EADs.*Humanitarian"))
test15 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "I-918 Petition for U Nonimmigrant Status.*Employment Authorization|Employment Authorization.*I-918 Petition for U Nonimmigrant Status"))
test19 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "Asylum.*Non-delivered EADs|Non-delivered EADs.*Asylum"))

#Immigrant-Family and Immigrant-Humanitarian: 
test11 <- subset(d1, 
                str_detect(SUBJECT, 
                           "I-730 Refugee/Asylee Relative Petition.*Humanitarian|Humanitarian.*I-730 Refugee/Asylee Relative Petition"))
test12 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "I-130 Petition for Alien Relative.*Humanitarian|Humanitarian.*I-130 Petition for Alien Relative"))
test14 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "Family Based.*Humanitarian|Humanitarian.*Family Based"))
test16 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "I-130 Petition for Alien Relative.*I-918 Petition for U Nonimmigrant Status|I-918 Petition for U Nonimmigrant Status.*I-130 Petition for Alien Relative"))
test18 <- subset(d1, 
                 str_detect(SUBJECT, 
                            "Family Based.*Asylum|Asylum.*Family Based"))

