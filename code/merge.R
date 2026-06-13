# This script combines clean log/letter files and merges in other data sources, creating the correspondence.Rdata file used in markdown

# load required functions
source("setup.R") # clean.agency() cleans data and adds a sheet of unresolved intercoder discrepencies to google drive
packageVersion("dplyr")

if(F){
drive_auth(email = NA)
gs4_auth(email = NA)
}

# until we totally get rid of name methods loaded in setup, we need to specify the new version of extract member name from the legislators package 
extractMemberName <- legislators::extractMemberName

# for debugging, load members data from legislators repo as the merge runs 
here::here("data", "members.rda") |> str_replace("correspondence_data", "legislators-data") |> load()
members |> arrange(nchar(pattern)) |> distinct(pattern, bioname)
members |> filter(is.na(pattern))
# add committees and stuff
#FIXME move this to merge2 
# source("members/augmentMembers.R")


source(here("data_list.R"))
data_list |> kable()


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
             "DOJ_FBI")


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
d1$DATE %<>% as.Date()

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
missing_data %<>% select(any_of(c("agency", "DATE", "FROM","congress", "LetterID", "ID", "ERROR"))) %>% 
  legislators::extractMemberName("FROM", congress = "congress")

# Inspect for things that should have matched but did not for some reason
head(missing_data |> distinct(DATE, FROM))

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
  #data_list %<>% filter(row_number() > which(data_list$agency == "DOL_ETA")) #FIXME NO SCRIPT FOR DOL_ETA YET issue #203 # Seems to be fixed as of March 2026
  #data_list %<>% filter(row_number() > which(data_list$agency == "DOT_SLSDC")) #FIXME error in DOT_SLSDC issue #207 # Seems to be fixed as of March 2026
  # data_list %<>% filter(row_number() >= which(data_list$agency == "USDA_NIFA")) #FIXME error in DOT_SLSDC issue #207 # Seems to be fixed as of March 2026
}



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
  
  head(data_list)
}


#######################################################################



library(logr)
log_open(show_notes = F)
data_list |> kable() 

######################################################################################


# A function to loop over all agecies and run the clean script on them, saving Rdata files for each 
i <- 1 # FIXME with purr walk + error handling
# i = i-1

while(!is.na(data_list[i,1])) {
  
  # for debugging, load members data from legislators repo as the merge runs 
  here::here("data", "members.rda") |> str_replace("correspondence_data", "legislators-data") |> load()
  
  # print the agency 
  base::message(inverse("----", data_list$agency[i], "----"))
  
  agency = as.character(data_list[i, 1])
  status = as.character(data_list[i, 2])
  coders = as.character(data_list[i, 3])
  
  # clean the agency 
  d1 <- clean.agency(
    agency,
    status,
    coders
    )
  
  # FIXME in clean 
  d1$DATE <- as.Date(d1$DATE)
  d1$icpsr <- as.numeric(d1$icpsr)

  # check how many unmatched observations per congress
  d1 %>% mutate(NAs = ifelse(is.na(icpsr), "missing", "matched with member")) %>% count(congress, NAs) %>% spread(key = NAs, value = n)
  
  # bad dates 
  bad_dates <- d1 %>% 
    filter(is.na(DATE), nchar(FROM) > 6 ) %>% 
    count(FROM) %>% 
    ungroup() %>% 
    mutate(agency = agency)
  
  head(bad_dates, 100) |> filter(nchar(FROM)< 150 ) |>  kable(caption = "Missing dates; can we infer from context?") |> print()
  
  # check unmatched 
  missing <- d1 %>% 
    filter( is.na(icpsr), nchar(FROM) > 6 ) %>% 
    mutate(congress = year_congress(year) ) %>% 
    # drop bad date
    drop_na(congress) %>%
    count(FROM, congress) %>% 
    arrange(-n) %>% 
    ungroup() %>% 
    mutate(agency = agency)

  head(missing, 100) |> filter(n>1) |> filter(nchar(FROM)< 150 ) |> kable(caption = "missing/unmatched with a member of congress") |> print() 
  
  # missing |> filter(nchar(FROM)< 150 ) |> group_by(FROM) |>  tally(n, sort = T) |> filter(n>1) |> head(20) |> kable(caption = "These people are affilliated with US congress. What is their affiliation? If they served in congress, bold which congresses?") |> print() 
  
  # check letters with multiple authors (or possible false matches) 
  multi <- d1 %>% 
    distinct(LetterID, FROM, congress, bioname) %>% 
    group_by(LetterID, FROM) %>% 
    add_count() %>% 
    filter(n>1, nchar(FROM) > 6, nchar(FROM)<150 )  %>% 
    ungroup() %>% 
    select(-n, -LetterID) %>% 
    count(FROM, congress, bioname) %>% 
    arrange(-n) %>% 
    ungroup() %>% 
    mutate(agency = agency)
  
  head(multi, 100) |> filter(n>1) |>  kable(caption = "Multi-author letters (or possible false matches) ") |> print() 
  
    file.name <- here::here("data" , "agencies", paste0(agency, ".Rdata"))
  
    # save data 
  save(d1, file = file.name) 
  
  # save unmatched, multi-matched, and missing date data 
  save(missing, file = str_replace(file.name, ".Rdata", "-missing.rda") )
  save(multi, file = str_replace(file.name, ".Rdata", "-multi.rda") )
  save(bad_dates, file = str_replace(file.name, ".Rdata", "-bad_dates.rda") )
  
  i <- i + 1
}

#TODO look at bad dates in acf 


stopped <- data_list$agency[i]

base::message(white(paste("merge stopped at", stopped)))


log_close()

