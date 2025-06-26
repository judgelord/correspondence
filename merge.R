# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive
packageVersion("dplyr")
drive_auth(email = "correspondenceresearch@gmail.com")
gs4_auth(email = "correspondenceresearch@gmail.com")

# until we totally get rid of name methods loaded in setup, we need to specify the new version of extract member name from the legislators packge 
extractMemberName <- legislators::extractMemberName

# Vars from members data to keep and merge in
members %<>% dplyr::select(congress, pattern, bioname, 
                   first_name, last_name, icpsr, common_name,
                   party_name, party_code, state, state_abbrev, chamber, party_size,
                   seo_name, district_code, id, cqlabel, bioImgURL, 
                   district_code, nominate.dim2, nominate.dim1, nominate.geo_mean_probability) %>% 
  dplyr::distinct()

library(legislators)

# add committees and stuff
source("members/augmentMembers.R")


source(here("data_list.R"))
data_list


# if authorized, this should work
drive_get("RRB")


# check that each agency matches exactly one file on google drive
if(F){
map_dfr(
    paste(data_list$agency, data_list$coders) %>% str_remove(" NA"), 
    gs_title) %>% 
  add_count(name) %>%  
  filter(n != 1) %>% 
  select(name, path)
}


######CLEAN ############
# clean one file #
##################

# Test one agency
i <- which(data_list$agency == 
             #"DHS_USCIS")
             #"DOL_ETA")
             "DHS_USCIS_2016")
             #"DOJ_USMS")
             #"ABMC")


i

# clean.agency is the function that pulls in the google sheet, runs the clean script (which includes matching legislator names)
d1 <- clean.agency(
  agency = as.character(data_list[i, 1]),
  status = as.character(data_list[i, 2]),
  coders = as.character(data_list[i, 3])
  )

# this is only needed because it is change to chr in clean.r
#TODO go back and fix the code that changes icpsr to chr
d1$icpsr %<>% as.numeric()
members$icpsr %<>% as.numeric()


# # This should be empty; there should be no cases where bioname is NA and the name pattern matched is not 404error (i.e., no name is matched)
# d1 %>% filter(pattern != "404error", is.na(bioname)) %>% count(pattern, congress, sort = T)

  d1 %<>% 
    # join once to make sure we have bioname
    left_join(members) %>% 
    # select the core variables we get from all data  
    select(any_of(c("LetterID", "ID", 
           "DATE", "year", "congress", 
           "FROM", "pattern", "bioname", "agency", 
           "SUBJECT", "TYPE", "ALT_TYPE", "CERTAINTY", "POLICY_EVENT", "EVENT_NAME", "EVENT_DATE", 
           "CONSTITUENT_TYPE", "CONSTITUENT_CLASS", 
           "NOTES", "ERROR"))) %>% 
    # get rid of duplicates
    distinct() %>% 
    # join back in additional members data 
    # (THIS MIGHT CREATE TWO OBSERVATIONS FOR CHAMBER SWITCHERS)
    left_join(members) %>% 
    distinct()
  

d1$DATE %<>% as.Date()


# post-hoc corrections to be fixed in legislators https://github.com/judgelord/legislators/issues/5

d1 %<>% filter( !(FROM == "murphy, patrick" & bioname == "MURPHY, Patrick"),
                !(FROM == "lujan michelle lujan grisham" & bioname == "LUJÁN, Ben Ray"),
                !(FROM == "graves, garret" & bioname == "GRAVES, Tom")
                )

# check for over-matches
d1 %>% add_count(LetterID) %>% filter(n > 1) %>% kablebox()

# check unmatched 
d1 %>% filter( is.na(icpsr) ) %>% 
  distinct(FROM, DATE) %>% 
  group_by(FROM) %>% 
  top_n(1) %>% 
  kablebox()
               
# check how many unmatched observations per congress
d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(congress, NAs) %>% spread(key = NAs, value = n)

# check how many unmatched per agency 
d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(agency, NAs) %>% spread(key = NAs, value = n) %>% kable()

# # if this yields anything, something is wrong (obs are failing to match in the members file)
# d1 %>% filter(is.na(chamber), pattern != "404error") %>% count(pattern, congress)

missing_data <- d1 %>% 
  # create variable for whether matched or missing 
  mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% 
  add_count(agency, NAs) %>% 
  filter(NAs == "missing")

# should be the same as the unmatched observations 
nrow(missing_data)

# redo extractmembernames
missing_data %<>% select(agency, DATE, FROM,congress, LetterID, ID, ERROR) %>% 
  legislators::extractMemberName("FROM", congress = "congress")

# Inspect for things that should have matched but did not for some reason
head(missing_data)

# bad names (if any)
missing_data %>% count(FROM, congress, sort = TRUE)  %>% 
  filter(!str_detect(FROM, "Staff|ommittee"), congress != 0) %>% 
  top_n(20) %>% kable()

# bad dates (if any)
missing_data %>% count(FROM, congress, sort = TRUE)  %>% 
  filter(!str_detect(FROM, "Staff|ommittee"), congress == 0) %>% 
  top_n(20) %>% kable()

####################
####################
# Save 
file.name <- str_c("data/agencies/", 
                   unique(d1$agency), 
                   ".Rdata")

save(d1, file = file.name)



##################################
# Repeat merge while successful: #
##################################
# FIXME use purrr safely() to capture warnings as a few obs are being dropped due to parse failures


## Resume merge if it stopped 
if(F){
  # data_list %<>% filter(row_number() >= which(data_list$agency == "DHS_USCIS")) 
  data_list %<>% filter(row_number() > which(data_list$agency == "DOL_ETA")) #FIXME NO SCRIPT FOR DOL_ETA YET issue #203
  data_list %<>% filter(row_number() > which(data_list$agency == "DOT_SLSDC")) #FIXME error in DOT_SLSDC issue #207
  data_list %<>% filter(row_number() >= which(data_list$agency == "USDA_NIFA")) #FIXME error in DOT_SLSDC issue #207
  
  
}
data_list


# subset by date if you want to only update agencies that have not been updated recently 
if(F){
  # get file metadata 
  files <- str_c("data/agencies/", list.files(here("data/agencies"))) %>% 
    set_names(list.files(here("data/agencies"))) %>%
    file.info() %>% 
    as_tibble(rownames = "file") %>% 
    # mtime = date/time modified 
    filter(mtime < as.Date("2024-06-28")) %>% # date criteria
    distinct() 
  
  files$file
  
  data_list %<>% filter(!agency %in% str_remove_all(files$file, ".*/|.Rdata"))
}

head(data_list)

# A function to loop over all agecies and run the clean script on them, saving Rdata files for each 
i <- 1 # FIXME with purr walk + error handling
while(!is.na(data_list[i,1])) {
  
  # print the agency 
  base::message(inverse("----", data_list$agency[i], "----"))
  
  # clean the agency 
  d1 <- clean.agency(
    agency = as.character(data_list[i, 1]),
    status = as.character(data_list[i, 2]),
    coders = as.character(data_list[i, 3]))
  
  # post hoc fix 
  d1$icpsr %<>% as.numeric() #FIXME in clean 
  
  # join in bioname and select min set of variables to reduce duplicates 
  suppressMessages(
  d1 %<>% 
    left_join(members) %>% 
    select(ID, LetterID, agency, 
           DATE, year, congress, 
           FROM, bioname, 
           SUBJECT, TYPE, ALT_TYPE, CERTAINTY, 
           CONSTITUENT_TYPE, CONSTITUENT_CLASS,
           POLICY_EVENT, EVENT_NAME, EVENT_DATE, 
           NOTES, ERROR) %>% 
    left_join(members)%>% 
    distinct()
  )
  
  # select the things we care about 
  d1 %<>% select(LetterID, ID, agency, DATE, year, congress, FROM, pattern, bioname, 
                SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR, 
                CONSTITUENT_TYPE, CONSTITUENT_CLASS,
                first_name, last_name, icpsr, party_name, party_code, state, state_abbrev, chamber, 
                district_code, nominate.dim2, nominate.dim1)
  
  # post hoc correction 
  d1$DATE <- as.Date(d1$DATE)
  
    file.name <- str_c("data/agencies/", 
                       unique(d1$agency), 
                       ".Rdata")
  
  save(d1, file = file.name)
  
  i <- i + 1
}

stopped <- data_list$agency[i]

base::message(white(paste("merge stopped at", stopped)))
 





