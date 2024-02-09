# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# source("setup.R")
#file.name <- "DHS_USCIS" # for testing

clean <- function(file.name) {
  data_raw <- gs_title(file.name) %>% gs_read()
  
  # if you want to test with a smaller sample
  if(F){
    data_raw %<>% 
      group_by(`Inquiry Type`, `Primary Inquiry Issue`) %>%
      slice_sample(n = 1) %>% 
      ungroup()
      
  } 
  
  
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
           SUBJECT = str_c(
             `Inquiry Type`,
             `Contact Method`,
             `Primary Inquiry Issue`,
             `Business Unit (Owning User) (User)`,
             `Primary Inquiry Sub-Issue`,
             `Secondary Inquiry Issue`,
             `Secondary Inquiry Sub-Issue`,
             sep = ";;;")
           )
  
  data$DATE %<>% 
    str_replace_all("-", "/") %>% 
    as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  library(legislators)
  data %<>% 
    legislators::extractMemberName("FROM",
                                   congress = "congress")
  

  return(data)
}


# FOR TESTING 
if(F){
  
  class(data$district_code)
  
  unique(data$district_code)
  
  class(d1$district_code)
  
  class(members$district_code)
  
  
  
 data %>% count(is.na(icpsr)) 
  
  data %>% filter(is.na(icpsr)) %>% distinct(FROM, congress, DATE)
  
  data %>% filter(is.na(icpsr),
                  str_detect(FROM, "vacant")) %>% view()



# TYPE CODING

data %<>%
 mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") &
                      str_detect(SUBJECT, "Briefing Request"), 5, TYPE)) %>%
 mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") &
                        str_detect(SUBJECT, "I-129 Petition for Nonimmigrant Worker"), 2, TYPE)) %>%
 mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") &
                        str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"), 2, TYPE)) %>%
 mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") &
                        str_detect(SUBJECT, "H2-A"), 2, TYPE)) %>%
#next section should be last because it will only code type 1s correctly if they're not already coded as type 2s??
 mutate(TYPE = ifelse(!str_detect(TYPE, "[0-9]") &
                        str_detect(SUBJECT, "Casework"), 1, TYPE))

# CONSTITUENT_TYPE CODING

data %<>%
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(TYPE, "1") &
                        str_detect(SUBJECT, "Deferred Action for Childhood Arrivals (DACA)"),
                      "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(TYPE, "1") &
                        str_detect(SUBJECT, "Deferred Action"),
                      "Immigrant-DACA", CONSTITUENT_TYPE)) %>%
at some point we will want to decide whether some of these employment categories should be 1s or 2s
so for now, the CONSTITUENT_TYPE coding will not depend on the coding in TYPE
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(SUBJECT, "EB-5"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(SUBJECT, "I-140 Immigration Petition for Alien Worker"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(SUBJECT, "I-485 (Employment-based) Application to Register Permanent Residence or Adjust Status"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE)) %>%
 mutate(CONSTITUENT_TYPE = ifelse(str_detect(SUBJECT, "I-129CW Petition for CNMI-Only Nonimmigrant Transitional Worker"),
                                  "Immigrant-Employment", CONSTITUENT_TYPE))



}