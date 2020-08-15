# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive



# Vars from members data to keep and merge in
members %<>% select(congress, pattern, bioname, 
                   first_name, last_name, icpsr, common_name,
                   party_name, party_code, state, state_abbrev, chamber, party_size,
                   seo_name, district_code, id, cqlabel, bioImgURL, 
                   district_code, nominate.dim2, nominate.dim1, nominate.geo_mean_probability) %>% 
  distinct()

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
i <- which(data_list$agency == "ABMC")
i

d1 <- clean.agency(
  agency = as.character(data_list[i, 1]),
  status = as.character(data_list[i, 2]),
  coders = as.character(data_list[i, 3]))

# this is only needed because it is change to chr in clean.r
d1$icpsr %<>% as.numeric()
members$icpsr %<>% as.numeric()

d1 %>% filter(pattern != "404error", is.na(bioname)) %>% count(pattern, congress, sort = T)

  d1 %<>% 
    left_join(members) %>% 
    select(LetterID, ID, 
           DATE, year, congress, 
           FROM, pattern, bioname, agency, 
           SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, 
           CONSTITUENT_TYPE, CONSTITUENT_CLASS, 
           NOTES, ERROR) %>% 
    left_join(members)%>% 
    distinct()

d1$DATE %<>% as.Date()


d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(congress, NAs) %>% spread(key = NAs, value = n)

d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(agency, NAs) %>% spread(key = NAs, value = n) %>% kable()

# if this yeilds anything, something is wrong (obs are failing to match in the members file)
d1 %>% filter(is.na(chamber), pattern != "404error") %>% count(pattern, congress)

missing_data <- d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% 
  add_count(agency, NAs) %>% 
  filter(NAs == "missing")

# redo extractmembernames
missing_data %<>% select(agency, DATE, FROM,congress, LetterID, ID, ERROR) %>% extractMemberName(members, "FROM")

# bad names
missing_data %>% count(FROM, congress, sort = TRUE)  %>% 
  filter(!str_detect(FROM, "Staff|ommittee"), congress != 0) %>% 
  top_n(20) %>% kable()

# bad dates 
missing_data %>% count(FROM, congress, sort = TRUE)  %>% 
  filter(!str_detect(FROM, "Staff|ommittee"), congress == 0) %>% 
  top_n(20) %>% kable()

# if this yeilds anything, something is wrong (obs are failing to match in the members file)
missing_data %>% filter(!pattern %in% c("Date out of range", "404error"))

sum(!is.na(d1$icpsr))
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


# data_list <- data_list[i:nrow(data_list),]
# data_list %<>% filter(!(agency %in% d$agency)) # to add new agencies without updating old ones or restart interrupted merge

## Resume 
# data_list %<>% filter(row_number() >= which(data_list$agency == "RRB")) 
data_list
# subset by date
if(F){
  files <- str_c("data/agencies/", list.files(here("data/agencies"))) %>% 
    set_names(list.files(here("data/agencies"))) %>%
    file.info() %>% 
    as_tibble(rownames = "file") %>% 
    filter(mtime < as.Date("2020-06-28")) %>% # date criteria
    distinct() 
  
  files$file
  
  data_list %<>% filter(agency %in% str_remove_all(files$file, ".*/|.Rdata"))
}

head(data_list)

i <- 1 # FIXME with purr walk + error handeling
while(!is.na(data_list[i,1])) {
  
  base::message(inverse("----", data_list$agency[i], "----"))
  
  d1 <- clean.agency(
    agency = as.character(data_list[i, 1]),
    status = as.character(data_list[i, 2]),
    coders = as.character(data_list[i, 3]))
  
  d1$icpsr %<>% as.numeric() #FIXME in clean 
  
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
  
  d1 %<>% select(LetterID, ID, agency, DATE, year, congress, FROM, pattern, bioname, 
                SUBJECT, TYPE, ALT_TYPE, CERTAINTY, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR, 
                CONSTITUENT_TYPE, CONSTITUENT_CLASS,
                first_name, last_name, icpsr, party_name, party_code, state, state_abbrev, chamber, 
                district_code, nominate.dim2, nominate.dim1)
  
  d1$DATE <- as.Date(d1$DATE)
  
    file.name <- str_c("data/agencies/", 
                       unique(d1$agency), 
                       ".Rdata")
  
  save(d1, file = file.name)
  
  i <- i + 1
}

stopped <- data_list$agency[i]

base::message(white(paste("merge stopped at", stopped)))
 





