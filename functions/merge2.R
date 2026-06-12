# this script merges cleaned data files 
source(here::here("setup.R"))
source(here::here("data_list.R"))

##########################
# load saved Rdata files #
# created by merge.R     #
##########################
files <- str_c("data/agencies/", list.files(here("data/agencies"))) %>% 
  set_names(list.files(here("data/agencies"))) 

files <- list.files(here::here("data", "agencies"), pattern = "Rdata", full.names = T) 

files <- files[!str_detect(files, "missing|date|multi")]

agencies_data <- files |> str_remove_all(".*/|.Rdata")

agencies_data %in% data_list$agency |> sum()

data_list$agency %in% agencies_data  |> sum()

#TODO check dates 

# a function to combine rdata files? 
combine <- function(file){
  load(file)
  message(unique(d1$agency))
  # FIXME in clean 
  d1$DATE <- as.Date(d1$DATE)
  d1$icpsr <- as.numeric(d1$icpsr)
  
  return(d1)
}

# COMBINE FILES 
d <- map_dfr(files, combine)

d_temp <- d

#########################################################

d <- d_temp 

d %<>% distinct()
dim(d)
write(names(d), "column_names.txt")
write(nrow(d), "n_total_letters.txt")

sum_na <- . %>% is.na() %>% {. %in% (F)} %>% sum()

d |> group_by(agency) |> summarise_all(sum_na) |> write_csv("vars_by_agency.csv")

#FIXME USCIS batches are processed in two scripts as if it is two different agencies 
d %<>% mutate(agency = str_remove(agency, "_2016"))
data_list %<>% filter(!str_detect(agency, "_2016"))

here::here("data", "members.rda") |> str_replace("correspondence_data", "legislators-data") |> load()

#FIXME just dropping these people who went independent for too short a time to get a nominate score for now 
# this should be added to fix.member.date.coding
members %<>% filter(!icpsr %in% c(90915,91737))

d |> filter(!is.na(icpsr) & !chamber %in% unique(members$chamber)) |> count(agency)

# we only want to merge on icpsr, congress -- we deal with chamber switchers in the corrections below (chamber in the data is incomplete)
names(d)[names(d) %in% names(members)]

to_keep <- c("LetterID", "agency", "ID", "DATE", "FROM", "data_id", "icpsr", "congress", #"chamber",
             "TYPE", "CERTAINTY", "ALT_TYPE", "SUBJECT",
             "POLICY_EVENT", "EVENT_NAME", "EVENT_DATE", 
             "NOTES", "ERROR",  
             "CONSTITUENT_TYPE", "CONSTITUENT_CLASS")

d %<>% select(any_of(to_keep))  %>% distinct()


# join in member covariates 
d %<>% left_join(members |>
                   select(congress, icpsr, bioname, chamber, party_code)) %>%
  distinct()

d <- d |>
  mutate(
    party_name = case_when(
      party_code == 100 ~ "Democratic Party",
      party_code == 200 ~ "Republican Party",
      party_code == 328 ~ "Independent",
      is.na(party_code) ~ NA_character_
    )
  )

# people that need fixing in fix.memberdate.coding() from MemberNameDateCorrections.R 
# party corrections 
d |> distinct(icpsr, bioname,congress, party_name) |> 
  group_by(congress, bioname) |> 
  add_count() |> 
  filter(congress >104, n > 1)

# chamber switchers 
d |> distinct(icpsr, bioname,congress, chamber) |> 
  group_by(congress, bioname) |> 
  add_count() |> 
  filter(congress >104, n > 1)

# other corrections 
d |> distinct(icpsr, bioname) |> 
  filter(str_detect(bioname, "PAYNE"))

# fixes applied to members who left or joined congress, etc
# n should go down
nrow(d)
d %<>% fix.member.date.coding() %>% distinct()
nrow(d)

# confirm that fix date worked 
GILLIBRAND <- filter(d, bioname == "GILLIBRAND, Kirsten") |> arrange(DATE) |> distinct(DATE, chamber)
MARKEY <- filter(d, bioname == "MARKEY, Edward John") |> arrange(DATE) |> distinct(DATE, chamber)


# check for party switchers 
d %>% 
  count(agency, LetterID, ID, data_id,
        DATE, congress, 
        FROM, bioname, agency) %>% 
  filter(n >1) %>% 
  ungroup() %>% 
  select(bioname, congress) %>% distinct() 

# problems (mostly chamber and party switchers?)
# AS OF June 2026, all problems were with non-voting members
look <- d %>% count(LetterID, ID,# data_id, # data_id is created by extractmembername
               DATE, congress, 
               FROM, bioname, 
               #SUBJECT,
               agency) %>% 
  filter(n >1)


count(look, bioname, sort = T)
count(look, FROM, sort = T)
count(look, agency, wt = n, sort = T) 

# post ho corrections 
d %<>% filter( !( FROM == "johnson, timothy peter" & 
                   bioname == "JOHNSON, Timothy Peter (Tim)") 
               )

## Missing agencies:
data_list %>% filter(!(agency %in% d$agency)) %>% select(agency)

# Check for NAs in LetterID
d %>% filter(is.na(LetterID)) %>% count(agency) %>% arrange(agency) #%>% kablebox()
d %>% filter(is.na(data_id)) %>% count(agency) %>% arrange(agency) #%>% kablebox()
d %>% filter(is.na(LetterID)) %>% count(agency, sort = T)
d %>% filter(is.na(ID)) %>% count(agency, sort = T)


# check for consistent ID digits (unclear if this is still needed)
unique(nchar(d$LetterID))

d %>% distinct(agency, nchar(LetterID))

# just CDC and USCIS
filter(d, nchar(LetterID) != 6) %>% select(agency) %>% distinct()

#########################################################################################
# COMPARE TO LAST RUN 
nrow(d)
load("draw.Rdata")
nrow(draw)

change <- full_join(d %>% 
                      group_by(agency) %>% 
                      filter(!is.na(icpsr)) %>% 
                      count(name = "d"),
                    draw %>% 
                      group_by(agency) %>% 
                      filter(!is.na(icpsr)) %>% 
                      count(name = "draw") ) %>%
  mutate(
    d = replace_na(d, 0),
    draw = replace_na(draw, 0),
    change = d-draw) %>% 
  arrange(change) %>% 
  filter(change != 0) 

change %>%  kablebox()

write_csv(change, file =  here::here("log", paste0("change", Sys.Date(), ".csv")) )
save(change, file = here::here("log", paste0("change", Sys.Date(), ".rda")) )


changed <- full_join(draw %>%
                       filter(!is.na(icpsr)) %>% 
                       select(agency, FROM) %>% 
                       distinct() %>% mutate(in_draw = TRUE),
                     d %>%
                       filter(!is.na(icpsr)) %>% 
                       select(agency, FROM) %>% 
                       distinct() %>% mutate(in_d = TRUE) )

changed

save(changed, file = here::here("log", paste0("changed", Sys.Date(), ".rda")) )


missing <- changed %>% filter(is.na(in_d))

save(missing, file = here::here("log", paste0("missing", Sys.Date(), ".rda")) )

# actual problems 
missing %>% filter(agency %in% (data_list %>% 
                                  #filter(row_number() <= which(data_list$agency == "DOI_SOL")) %>% 
                                  .$agency ) ) %>% 
  count(agency, str_sub(FROM, 1, 40), sort = T) %>% 
  filter(agency %in% (data_list %>% 
                        #filter(row_number() <= which(data_list$agency == "DOI_SOL")) %>% 
                        .$agency ) )%>%
  slice_head(n = 200) %>%
  arrange(agency) %>% 
  kablebox()

# broken
missing %>% 
  add_count(agency, sort = T, name = "per_agency") %>% count(per_agency, agency, FROM, sort = T) %>% 
  #write_csv("changed_names.csv")
  slice_max(n, n= 100)  %>% arrange(-per_agency, n) %>%   kablebox()

# fixed 
changed %>% filter(is.na(in_draw)) %>% select(agency, FROM, in_d)
#FIXME We should drop all unecessary vars and add them back in later to make post-merge processing go faster

# if things look good, save new raw file
# archive raw version of merged data 
draw <- d
nrow(draw)

update = F
if(update){
save(draw, file = "draw.Rdata")
}
# load("draw.Rdata")
# d <- draw

###############
# FIX ERRORS #

#######################
# ERRORS we can't fix #
#######################

# Reoccurring problem names (these people are frequently in the data but not members of Congress)
# FIXME 
# Rewrite with purrr
# We are not using this right now (though useful in the future, so turning it off for now)
if(F){
names <- list(a= c("Eleanor","Norton"),
              b= c("Sally",'Jewell'),
              c= c('Gregorio','Sablan'), 
              d= c('Stacey|Stacy','Plaskett'),
              e= c('Amata','Radewagen'),
              f= c("Donna",'Christensen|Christianson'),
              g= c('Pedro','Pierluisi'),
              h= c('Madeleine','Bordallo'),
              i= c('Eni','Faleomavaega'),
              j= c('(^| )Tia( |$)','Johnson'), 
              k=c('Nelson','Peacock'),
              l=c('Brian','De Va(|ll)ance'),
              m=c('Peggy','Sherry'),
              n=c('Donald', 'Kent'), 
              o=c('Ann','Schneider'), 
              p=c('Katherine', 'Archuleta'), 
              q=c('Tom|Thomas','Vilsack'), 
              r=c('Luis','Fortuno'))

for(i in 1:length(names)){
  d %<>%
    mutate(ERROR = ifelse(grepl(names[[i]][1], FROM, ignore.case=T)&grepl(names[[i]][2], FROM, ignore.case=T), "Don't include", ERROR))
}


# other errors
d %<>% 
  group_by(agency, ID, DATE, FROM, SUBJECT, icpsr) %>% mutate(n = n()) %>% 
  mutate(ERROR = ifelse(n >1 & (bioname == "ROGERS, Mike Dennis" | bioname == "ROGERS, Mike"), "FOIA 2 Mike Rogers", ERROR)) %>%  # 2 different members with name Mike Rogers
  mutate(ERROR = ifelse(n >1 & (bioname == "JOHNSON, Timothy Peter (Tim)" | bioname == "JOHNSON, Timothy V."), "FOIA 2 Tim Johnsons", ERROR)) %>% 
  # these are commented out because they risk matching real observations---can be more precise by looking at bad names 2
  #mutate(ERROR =  ifelse(grepl("(^| )Biden(,| |$)", FROM)& DATE > as.Date('2009-01-19'), "Joe is VP", ERROR)) %>% 
  #mutate(ERROR = ifelse((grepl("Eleanor|Holmes", FROM)&grepl("Norton", FROM))|(grepl("Eleanor", FROM)&grepl("Holmes", FROM)), "Non-voting DC Rep", ERROR)) %>% 
  # These are specific enough, that they are fine errors
  mutate(ERROR = ifelse(grepl("^White House$", FROM, ignore.case=T), "White House", ERROR)) %>% 
  mutate(ERROR = ifelse(grepl("^Miscellaneous$", FROM, ignore.case=T), "Miscellaneous", ERROR)) %>% 
  ungroup()
}


#########################
# ERRORS to investigate #
#########################

d$year <- d$DATE |> str_sub(1,4) |> as.numeric()

# date typos 
bad.dates <- d %>% 
  filter(is.na(ERROR)) %>% 
  filter(!is.na(FROM) & FROM != "") %>% 
  filter(year > 2026 | year < 1999) %>% 
  arrange(DATE) %>% 
  select(LetterID, ID, agency, DATE, FROM, bioname, SUBJECT, TYPE, NOTES, ERROR)
nrow(bad.dates)

bad.dates |> head(25) |> distinct(agency, DATE, bioname, SUBJECT) |> kable()


##### OPTOINAL 
if(update){
# names that match more than one member - potential false positives, but they also may just be letters with multiple members
bad.names.1 <- d %>% 
  ungroup() %>%
  distinct() %>% 
  filter(is.na(ERROR), !is.na(icpsr)) %>% 
  group_by(agency, LetterID,  DATE, FROM) %>% 
  mutate(n = n()) %>% filter(n>1) %>% ungroup() %>%
  group_by(agency) %>% mutate(n = n()) %>% ungroup() %>% arrange(n) %>% 
  select(agency, LetterID, FROM, party_code, chamber, congress) 
bad.names.1
bad.names.1 %>% head() %>% kable()
bad.names.1 %>% count(agency)
bad.names.1 %>% count(FROM, sort = T)
bad.names.1 %>% count(FROM, pattern, sort = T)
bad.names.1 %>% count(FROM, pattern, agency, sort = T)


bad.id <- d %>% select(agency, ID) %>% filter(str_detect(ID, " ")) 
bad.id %>% group_by(agency) %>% top_n(1) %>% distinct() %>% kable()
bad.id %>% count(agency)

bad.id <- d %>% select(agency, LetterID) %>% filter(str_detect(LetterID, " ")) 
bad.id %>% group_by(agency) %>% top_n(1) %>% distinct() %>% kable()
bad.id %>% count(agency)
# names that don't match - potentially typos / false negatives
bad.names.2 <- d %>% 
  ungroup() %>% 
  filter(is.na(ERROR)) %>% 
  filter(is.na(bioname) | bioname == "") %>% 
  select(LetterID, ID, agency, DATE, congress, FROM, chamber, TYPE, NOTES)

worst.agencies <- bad.names.2 %>% ungroup() %>% drop_na(FROM) %>% count(agency)  %>%  arrange(-n) %>% top_n(10)
worst.agencies

worst.names <- bad.names.2 %>% 
  ungroup() %>% drop_na(FROM) %>% filter(FROM != "NA", FROM != "") %>% 
  mutate(FROM = str_squish(FROM)) %>% select(FROM, agency, congress) %>% 
  group_by(FROM) %>% 
  count(sort = T) # new n

worst.names.sheet <- gs_title("worst.names") %>% 
  gs_read()  %>%
  select(-n) %>%  # drop old n, but keep old problems
  mutate(congress = str_split(congress, ";")) %>%
  unnest(congress) %>%
  mutate(congress = as.numeric(congress)) %>%
  mutate(agency = str_split(agency, ";")) %>% 
  unnest(agency) 

worst.names %<>% full_join(worst.names.sheet)



worst.names %<>% 
  group_by(FROM) %>% 
  summarise_all(combine_strings) %>% 
  ungroup() %>% 
  distinct() %>%
  # filter(!str_detect(problem, "^other|^not unique|not in congress")) %>% # if we do this we lose info
  mutate(n = as.numeric(n)) %>% 
  arrange(-n)   %>% 
  filter(n>5) # 5 mismatches 
worst.names

# push to google drive
sheet_write(worst.names, gs_title("worst.names"), sheet = as.character(Sys.Date()))

}


####################################################################################
# If things look good, go on to creating the master data set 
####################################################################################
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#


d %<>% ungroup()
nrow(d)
######################################################################






################################################################
# Drop observations that failed to match in Voteview
###########################################################
d %<>% filter(!is.na(icpsr)) 
nrow(d) # SHOULD GO DOWN 
write(nrow(d), "n_total_matched.txt")


d %<>% filter(!is.na(year))
nrow(d) # SHOULD NOT GO DOWN 
class(d)
# chamber errors?
chamber_errors <- d %>% filter(!chamber %in% c("House", "Senate", "President"))
nrow(chamber_errors)
chamber_errors$bioname %>% unique()

pres <- d %>% filter(chamber %in% c("President"))


d %<>% filter(chamber %in% c("House", "Senate"))
nrow(d) # DROPPING PRESIDNETS 

# look for duplicates 
duplicates <- d %>% 
  group_by(DATE, agency, 
           data_id, # without this, you just get agencies where subjects are repeated and members wrote more than one letter on a date (still informative)
           bioname, SUBJECT) %>% # with the same icpsr and date
  add_count() %>% 
  filter(n>1) %>% 
  summarise_all(combine_strings)  %>%
  distinct() %>% 
  ungroup()

duplicates %>% count(agency, sort = T) 

duplicates %>% count(agency, SUBJECT,sort = T) 

# These are suspicious, but also subjects repeated and members wrote more than one letter on a date (still informative)
suspicious <- d %>% 
  #filter(agency == "VA") %>% 
  count(agency, DATE, SUBJECT, bioname, sort = T) %>% filter(n>1)

head(duplicates)
max(duplicates$n) 
nrow(duplicates)
unique(duplicates$n)
duplicates$n %<>% as.numeric()
sum(duplicates$n)

# inspect potential problems with coding
duplicate_coding <- duplicates %>%  
  filter(str_detect(TYPE, ";;;")|str_detect(ALT_TYPE, ";;;") ) ## |str_detect(CERTAINTY, ";;;")) #|str_detect(POLICY_EVENT, ";;;")|str_detect(EVENT_NAME, ";;;")|str_detect(NOTES, ";;;"))
duplicate_coding %<>% 
  select(DATE, agency, bioname, SUBJECT, TYPE, ALT_TYPE) %>% distinct() %>% arrange(agency)
duplicate_coding


if(update){
  # write_csv(duplicate_coding %>% filter(agency != "DOE_FERC"), path = "duplicate_coding.csv")
  sheet_write(duplicate_coding, gs_title("duplicate_coding"), as.character(Sys.Date()))
}


# check to see where duplicates are coming form other sources 
duplicate_chambers <- duplicates %>%  
  filter(str_detect(chamber, ";;;") )
duplicate_chambers %>% select(congress, bioname, party_code, icpsr, chamber) %>% distinct()

duplicate_party <- duplicates %>%  
  filter(str_detect(party_code, ";;;") )
duplicate_party %>% select(congress, bioname, party_name, icpsr) %>% distinct()
d$party_name %>% unique()

duplicate_icpsr <- duplicates %>%  
  filter(str_detect(icpsr, ";;;") )
duplicate_icpsr  %>% select(congress, bioname, party_name, icpsr)  %>% distinct()

duplicates %>% 
  arrange(DATE) %>% 
  select(bioname, DATE, agency, SUBJECT, n)
max(duplicates$n)

if(update){
  write_csv(duplicates, file = here("data/likely_duplicates.csv"))
}




##TODO Collapse unique name, Date, agency, subject?
## Can't do this because it over-collapses some agences with no SUBJECT or short subjects that are not in fact duplicates 
## there are true cases where a member wrote more than one letter on a date...sometimes a lot (e.g. Jeff Sessoins sent 44 letters about a rulemaking to CMS one day)
# d %>% group_by(DATE, agency, SUBJECT, icpsr, chamber) %>% top_n(1, TYPE) %>%  summarise_all(combine_strings)
if(F){ # WE WANT TO TAKE ONE OBSERVATION FOR DOUBLE-CODED - WE ALREADY DID THIS, SO THIS IS REDUNDENT 
# IF N GOES DOWN HERE, IT WILL GO DOWN WHEN WE COMBINE STRINGS, should be the same n
nrow(d)
d %>% count(LetterID, ID, DATE, agency, SUBJECT, icpsr, chamber, sort = T)
                  
# THIS IS SOMEWHAT COMPUTATIONALLY INTENSE but an important check for duplicates 
d2 <- d %>% group_by(LetterID, ID, DATE, agency, SUBJECT, icpsr, chamber) %>% top_n(1, TYPE) %>% 
  summarise_all(combine_strings)
nrow(d2) # MIGHT GO DOWN

# DROP DUPLICATE CODING
d$TYPE %<>% str_remove(";;;.*")

d %<>% select(-n)
}



#FIXME constituent type and class codes from google sheet - this is a bit convoluted at the moment
# issue #196 
source("functions/constituent_types.R")

# OPTIONALLY UPDATE CONSTITUENT CODING SHEET 
if(F){ 
# inspect observations successfully coded 
constituent_coding <- d %>% 
  ungroup() %>% 
  filter(!is.na(CONSTITUENT_TYPE)|!is.na(CONSTITUENT_CLASS)) %>% 
  select(agency, SUBJECT, TYPE, 
         CONSTITUENT_TYPE, CONSTITUENT_CLASS,NOTES, ERROR) %>% 
  group_by(agency, SUBJECT) %>% add_count() %>%
  summarise_all(combine_strings) %>% arrange(agency) 

constituent_coding

# FIXME split and recombine unique 
constituent_coding %>% mutate(n = as.numeric(n))%>% count(agency, CONSTITUENT_TYPE, wt = n, sort = T) %>% kable()

if(update){
  sheet_write(constituent_coding, gs_title("constituent_coding"), as.character(Sys.Date()))
}

}

# check that we have still complete data 
unique(d$agency) %in% data_list$agency
data_list$agency %in% unique(d$agency) 

data_list$agency[!data_list$agency %in% unique(d$agency)  ]


# save if all data sources merged, save data files
if(length(unique(d$agency)) == length(unique(data_list$agency))){
  
  all_contacts <- d
  save(all_contacts, 
       file = here::here("data", "all_contacts.rda"))
  
  # create and save annual count data (THIS TAKES A BIT TO RUN, CONSIDER CLEARING MEMORY TO MAKE IT RUN FASTER)
  source(here("functions/count.R"))
  
  # and monthly counts 
  #source(here("functions/count-month.R"))
  
  
  write_csv(bad.names.1, here("data/bad.names.1.csv"))
  save(bad.names.2, file = here("data/bad.names.2.csv"))
  bad.names.2 %>% 
    drop_na(TYPE, FROM, SUBJECT) %>% #FIXME when this is smaller, we can preview more on github limit 500kb csv preveiw
    select(ID, agency, DATE, FROM, TYPE, SUBJECT, NOTES) %>% 
    arrange(agency) %>% 
    write_csv(here("data/bad.names.2.csv"))
  worst.agencies %>% write_csv(here("data/worst.agencies.csv"))
  worst.names %>% write.csv(here("data/worst.names.csv"))
  bad.dates %>% write_csv(here("data/bad.dates.csv"))
  bad.party %>% write.csv(here("data/bad.party.csv"))
  #FIXME
  # save(bad.committees.1, file = "data/bad.committees.1.RData")
  # save(bad.committees.2, file = "data/bad.committees.2.RData")
  d %>% filter(str_detect(NOTES, "FOIA")) %>%
    select(ID, agency, FROM, DATE, SUBJECT, NOTES) %>% write_csv(path = here("data/LETTERS_TO_FOIA.csv"))
}

# counts per agency - check if this matches google sheet 
look <- d %>% count(agency, department) %>% full_join(data_list %>% select(agency))
look %>% filter(is.na(Department))

# Check that FERC data is complete:
d %>% filter(agency == "DOE_FERC") %>% count(year)

# If everything looks good, update data summary table 
# source("agencies/_FOIA_response_table.R")

data_complete()

if(F){
  all_contacts %<>% mutate(agency = str_remove(agency, "_2016"))
  
  save(all_contacts, 
       file = here::here("data", "all_contacts.rda"))
}