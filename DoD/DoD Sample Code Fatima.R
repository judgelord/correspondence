###########First Part/Data##################
load(here("data/all_contacts.RData"))
# one obs per member per letter for all agencies
d <- all_contacts %>% 
  ungroup() %>% 
  # define d as just FERC
  filter(agency=="DOE_FERC") %>% 
  select(-SUBJECT) %>% 
  mutate(majority = ifelse(majority == 1, "Majority", "Minority")) %>%
  # FIXME 
  # GOODE needs to be fixed in party switchers portion of merge.R
  filter(!(last_name == "GOODE"&party == "(I)"))


# one obs per member per letter per committee for all agencies
load(here("data/all_contacts_committees.RData"))
# define dcommittees as just FERC
dcommittees <- all_contacts_committees %<>% filter(agency == "DOE_FERC")

# Coded letters, one obs per member per letter 
load(here("data/DOE_FERC-letters-coded.RData")) 
# TO DO: replace with DOE_FERC-letters-corps.Rdata when corps added
FERC_letters %<>% 
  select(ID, SUBJECT, 
         Freelancer, Cosigned_House, Cosigned_Senate,
         text_clean, Constituent, year,
         Place_State, Place_District, Place_District, 
         ProBusiness, AntiBusiness, ProProject, AntiProject) %>% 
  distinct()

# merge in coded letters
d %<>% left_join(FERC_letters) %>% filter(year <2019)

# Add hand-coding to auto-coding of letter type 
d %<>%  
  mutate(Type = as.character(Type)) %>% 
  mutate(Type = ifelse(tolower(Constituent) == "yes", "Indiv. Constituent", Type)) %>% 
  mutate(Type = ifelse(!is.na(ProBusiness)|!is.na(ProProject), "Corp. Constituent", Type))%>%
  # define Constituent as "Indiv. Constituent"
  mutate(Constituent = ifelse(Type == "Indiv. Constituent", "Indiv. Constituent", "Non_constituent"),
         Constituent = replace_na(Constituent, "Not coded") ) 


# For this analysis, treat corp policy letters as Policy
all_contacts$Type %<>% fct_recode(Policy = "Corp. Policy")
d$Type %<>% fct_recode(Policy = "Corp. Policy")

# make a variable for the pro or anti business position of each letter
d %<>% 
  mutate(letter_position = ifelse(!is.na(ProBusiness)|!is.na(ProProject), "ProBusiness", "Other"),
         letter_position = ifelse(!is.na(AntiBusiness)|!is.na(AntiProject), "AntiBusiness", letter_position),
         letter_position = ifelse(!is.na(AntiBusiness)&!is.na(ProBusiness), "Both", letter_position)) 

d %<>% 
  # FIXME
  # rounding years up is approximate, rewrite to go Nov-Nov?
  mutate(cycle = congress*2+1786 ) %>% 
  mutate(icpsryear = str_c(icpsr, cycle)) 

################Second Part/ By Party#########################

d %>% 
  mutate(For = Constituent,
         For = ifelse(letter_position == "ProBusiness", "Pro-Business", For),
         For = ifelse(For == "Non_constituent", "Other", For)) %>% 
  count(party_name, For, majority) %>% 
  filter(party_name != "Independent",
         !is.na(For),
         For != "Not coded",
         For != "Other",
         !is.na(majority)) %>% 
  group_by(party_name, majority) %>% 
  mutate(percent = n / sum(n) * 100 ) %>% 
  ggplot() + 
  aes(y = percent, x = "", fill = For) + 
  geom_col(position = "dodge") + 
  #geom_text(aes(label = n)) +
  facet_grid(majority ~ party_name) + 
  scale_fill_viridis_d(option = "C", end = .8) + 
  labs(x = "",
       fill = "Advocating for",
       y = "Percent") + 
  theme_minimal() + 
  theme(panel.grid.major.x  = element_blank())

#############Third Part/Chunk#################

d %>% 
  count(party_name, letter_position, majority) %>% 
  filter(party_name != "Independent",
         letter_position != "Other",
         !is.na(letter_position),
         !is.na(majority)) %>% 
  group_by(party_name, majority) %>% 
  mutate(percent = n / sum(n) * 100 ) %>% 
  ggplot() + 
  aes(y = percent, x = "", fill = letter_position) + 
  geom_col(position = "dodge") + 
  facet_grid(majority ~ party_name)+ 
  scale_fill_viridis_d(option = "C", end = .8) + 
  labs(x = "",
       fill = "",
       y = "Percent") + 
  theme_minimal() + 
  theme(panel.grid.major.x  = element_blank())

#######Coding of Constituent Type/Part 4################

all_percent <- all_contacts %>% 
  filter(Type != "To be coded") %>%  
  mutate(total = n()) %>% 
  group_by(Type, total) %>% summarise(nT = n()) %>% 
  mutate(percent = 100*round(nT/total, 2) ) %>% 
  mutate(agency = "Overall") %>% 
  ungroup()


d_percent <- d%>% 
  filter(Type != "To be coded") %>%  
  mutate(total = n()) %>% 
  group_by(Type, total) %>% summarise(nT = n()) %>% 
  mutate(percent = 100*round(nT/total, 2) ) %>% 
  mutate(agency = "DOE FERC") %>% 
  ungroup()

full_join( all_percent, d_percent)%>% 
  ggplot() + 
  aes(x = Type, y = percent, fill = agency, color = agency, label = nT) + 
  geom_col(position = "dodge") + 
  geom_text(vjust = -.1,
            aes(hjust = ifelse(agency == "Overall", 0,1))) +
  labs(x = "",
       fill = "",
       color = "",
       y = "Percent", #paste("Number of Contacts, N =", sum(nrow(all_contacts))),
       title = "Legislator Contacts with FERC") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x.top = element_text()) + 
  scale_color_viridis_d(option = "C", end = .8) +
  scale_fill_viridis_d(option = "C", end = .8) + 
  theme_minimal() + 
  theme(panel.grid.major.x  = element_blank())

#################Part 5/Second Chunk####################

d  %>% 
  filter(Constituent != "Not coded") %>% 
  ggplot() + 
  aes(x = letter_position) + 
  geom_bar() + 
  facet_wrap("Constituent") +
  labs(x = "",
       y = paste("Letters 2000-2018, N =", as.character(d%>%drop_na(letter_position, Constituent) %>% nrow() %>% as.character() )))

#######Part 6/Third Chunk###########

d  %>% 
  ggplot() + 
  aes(x = letter_position) + 
  geom_bar() + 
  facet_wrap("Constituent") +
  labs(title = "Including letters without constituent coding", 
       x = "",
       y = paste("Letters 2000-2018, N =", as.character(d %>% nrow() %>% as.character() )))

################Member/Part 7##############

d %>% 
  filter(chamber == "Senate") %>% 
  group_by(member_state, year, pop2010) %>% summarise(n = n()) %>%
  group_by(member_state, pop2010) %>% summarise(mean = mean(n)) %>% ungroup() %>% 
  ggplot() + 
  geom_point(aes(x = log(pop2010), y = mean), color = "light blue") + 
  geom_smooth(aes(x = log(pop2010), y = mean)) + 
  geom_text(aes(x = log(pop2010), 
                y = mean, 
                label = ifelse(mean > mean(mean)*1.5 | mean < mean(mean)*0.5, 
                               member_state, "")), 
            check_overlap = TRUE, 
            size = 2.5, 
            hjust = 0) + 
  theme_bw() +
  labs(title = "Senator Requests per Year by State Population",
       x = "Log State Population",
       y = "Average Number of Requests per Year")

###################Part 8/Another Chunk####################

Chamber = "Senate"

d %>% 
  ungroup() %>%
  filter(chamber == Chamber) %>% 
  group_by(state) %>% summarise(n = n()) %>%
  # map_id creates the aesthetic mapping to the state name column
  ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Total Number of Contacts from members of the", Chamber)) +
  scale_fill_viridis_c(option = "C", end = .8) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank()) # + facet_grid(. ~ Constituent)

#################part 9################

d %>% 
  ungroup() %>% 
  filter(chamber == Chamber) %>% 
  group_by(state, pop2010) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(Per_Capita = n/pop2010*1000000) %>% 
  # map_id creates the aesthetic mapping to the state name column in your data
  ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = Per_Capita), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Contacts Per Million Residents from Members of the", Chamber)) +
  scale_fill_viridis_c(option = "C", end = .8) +
  theme(legend.position = "bottom", 
        legend.title = element_blank() )#+facet_grid(. ~ Constituent)

###################Part 10#################

# By type2
d %>% 
  filter(chamber == Chamber) %>% 
  filter(Constituent != "Not coded") %>% 
  group_by(state, Constituent) %>% summarise(n = n()) %>%
  # map_id creates the aesthetic mapping to the state name column
  ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Total Number of Contacts from members of the", Chamber)) +
  scale_fill_viridis_c(option = "C", end = .8) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank()) + facet_grid(. ~ Constituent)

############Part 11#########

d %>% 
  filter(chamber == Chamber) %>%
  filter(Constituent != "Not coded") %>% 
  group_by(state, pop2010, Constituent) %>% summarise(n = n()) %>% ungroup() %>% 
  mutate(Per_Capita = n/pop2010*1000000) %>% 
  # map_id creates the aesthetic mapping to the state name column in your data
  ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = Per_Capita), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Contacts Per Million Residents from Members of the", Chamber)) +
  scale_fill_viridis_c(option = "C", end = .8) +
  theme(legend.position = "bottom", 
        legend.title = element_blank() )+
  facet_grid(. ~ Constituent)

############## Members Who Contact FERC More #############

# barcode plot of committee members 

# member by year by agency 
d %>% 
  filter(chamber == Chamber) %>% 
  group_by(name_state) %>% mutate(n = n()) %>% ungroup() %>% 
  filter(n > mean(n)) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(name_state, n) ), 
    # alpha = .6,
    shape = 73,
    size=2
  ) +  
  labs(title = paste(Chamber),
       y = paste("Members by", "Total Number of Letters"), 
       x = "Date of Correspondence" ) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_text(size=5)
  ) + 
  guides(fill=guide_legend(ncol=1)) 

################ Another Cool Chunk#############

Chamber <- "House" # "House" #  

# member by year by agency 
d %>% 
  filter(chamber == Chamber) %>% 
  group_by(name_state) %>% mutate(n = n()) %>% ungroup() %>% 
  filter(n > mean(n)) %>%
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = reorder(name_state, n) ), 
    # alpha = .6,
    shape = 73,
    size=2
  ) +  
  labs(title = paste(Chamber),
       y = paste("Members by", "Total Number of Letters"), 
       x = "Date of Correspondence" ) +
  theme(
    legend.title = element_blank(),
    axis.text.y = element_text(size=5)
  ) + 
  guides(fill=guide_legend(ncol=1)) 

#############Members Who Contact FERC More###################

all_contacts %>%
  mutate(FERC = agency == "DOE_FERC") %>%
  group_by(chamber, FERC, name_state, state) %>%
  summarise(n = n() ) %>%
  ungroup() %>%
  spread(key = "FERC", value = "n") %>% 
  mutate(ShareToFERC = round(`TRUE`/(`TRUE`+`FALSE`),2) ) %>% 
  arrange(-ShareToFERC) %>% 
  mutate(total = `TRUE` + `FALSE`) %>% 
  select(name_state, chamber, total, ShareToFERC) %>% 
  filter(ShareToFERC>.3) %>%
  knitr::kable()

###################Another Chunk#############

all_contacts %>%
  mutate(FERC = agency == "DOE_FERC") %>%
  group_by(congress, chamber, committees, FERC, name_state, state) %>%
  summarise(n = n() ) %>%
  ungroup() %>%
  spread(key = "FERC", value = "n") %>% 
  mutate(ShareToFERC = round(`TRUE`/(`TRUE`+`FALSE`),2) )%>% 
  arrange(-ShareToFERC) %>% 
  mutate(total = `TRUE` + `FALSE`) %>% 
  select(name_state, congress, chamber, committees, total, ShareToFERC) %>% 
  filter(ShareToFERC>.5, total>5) %>%
  knitr::kable()

#############Committee Leadership##############

dcommittees %>%  
  mutate(chair = str_remove(chair,"[0-9]* |^NA "),
         committee = paste(chamber, committee))%>% 
  dplyr::select(committee, position, chair, DATE, assigneddate, terminationdate) %>% 
  distinct() %>% 
  filter(str_detect(committee, "ENERGY$|ENVIRONMENT|NATURAL")) %>% 
  mutate(position = ifelse(position == "Other", NA, position)) %>%
  #filter(# !is.na(position),chair_since_2007 == T) %>%
  drop_na(chair) %>% 
  dplyr::select(DATE, chair, position, assigneddate, terminationdate, committee) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(
    aes(x = DATE, 
        y = chair),
    shape = 73,
    size=2 
  ) +
  geom_segment(aes(y = chair, yend = chair, 
                   x = assigneddate, xend = terminationdate, 
                   linetype = factor(position)),
               position = position_nudge(y = -0.3)) +
  labs(title = paste("Letters to FERC from Committee Leadership"),
       y = "", 
       x = "",
       linetype = "Leadership position") +
  scale_y_discrete(position = "right") +
  theme(
    strip.text.y = element_text(angle = 180, size = 5),
    # legend.title = element_blank(),
    axis.text.y = element_text(size=5),
    axis.text.x = element_text(angle = 0)
  ) + 
  facet_grid(committee ~ ., scales = "free_y", space = "free_y", switch = "both") 

###########Cosigned Letters#########

#A rough under-estimate based on whether "et al" or "&" appears in the summary:

FERC_letters %>% 
  filter(year>1999,
         year<2019) %>% 
  group_by(ID) %>% 
  top_n(1) %>% 
  mutate(probably_cosigned = ifelse(str_detect(SUBJECT, "et al"), "et al",
                                    ifelse(str_detect(SUBJECT, "&"), "&", " neither"))) %>%
  group_by(year, probably_cosigned) %>% 
  tally %>% 
  ggplot() + 
  aes(x = year, y = n, fill = probably_cosigned) +
  geom_col() +
  scale_fill_viridis_d(option = "C", end = .8)

#############Again#############

FERC_letters %>% 
  filter(!is.na(Freelancer),
         year>1999,
         year<2019) %>% 
  mutate(Cosigned = ifelse(!is.na(Cosigned_House)|!is.na(Cosigned_Senate),
                           T, F)) %>%
  group_by(year, Cosigned) %>% 
  tally %>% 
  ggplot() + 
  aes(x = year, y = n, fill = Cosigned) +
  geom_col() +
  scale_fill_viridis_d(option = "C", end = .8)

############Companies##########

# Split out to one obs per member per letter per business
dcorps <- d %>%
  mutate(ProBusiness = ProBusiness %>% str_split(";")) %>%
  unnest(ProBusiness)

dcorps %<>% mutate(ProBusiness = ProBusiness %>%
                     str_replace("RTO", 
                                 "Regional Transmission Organization") %>%  
                     str_remove_all(" Company.*| \\(.*| Inc.*| LLC.*| Energy.*") )%>%
  mutate(AntiBusiness = AntiBusiness %>% 
           str_replace("RTO", 
                       "Regional Transmission Organization") %>%  
           str_remove_all(" Company.*| \\(.*| Inc.*| LLC.*| Energy.*") )

dcorps %>%
  filter(!is.na(ProBusiness),
         !str_detect(ProBusiness, "Regional Transmission Organization") ) %>%
  group_by(ProBusiness) %>%
  select(id, last_name, ProBusiness, ID) %>%
  distinct() %>%
  summarise( n = n() , 
             members = str_c(unique(last_name), collapse = "; "),
             ID = str_c(unique(ID), collapse = "; ")) %>%
  filter(n>quantile(n,.9))%>%
  arrange(-n) %>%
  knitr::kable()

################FERC##########

load(here("data", "DOE_FERC-corps.Rdata"))
FERC_corps %<>% ungroup() %>% group_by(company_short) %>% mutate(industry = str_c(unique(industry), collapse = ";")) %>% select(-company, -Fortune500) %>% distinct() 

## Make a single string of short corp names
ferc_short <- unique(FERC_corps$company_short) %>% 
  str_c(collapse = "|") %>% 
  str_remove_all("||")

# bad string? 
# str_detect("", ferc_short)

save(ferc_short, file = here("data/ferc_short.Rdata"))

#####################Pro-Business############

dcorps %>%
  mutate(company_short = str_extract(ProBusiness, ferc_short)) %>%
  left_join(FERC_corps) %>% 
  distinct() %>%
  group_by(company_short_refs) %>%
  select(ID, last_name, company_short, company_short_refs) %>%
  filter(company_short != "")%>%
  distinct() %>%
  summarise( n = n() , 
             company_short = str_c(unique(company_short), collapse = "; "),
             members = str_c(unique(last_name), collapse = "; "),
             ID = str_c(unique(ID), collapse = "; ")) %>%
  filter(n>quantile(n,.9))%>%
  select(company_short, n, everything()) %>%
  arrange(-n) %>%
  knitr::kable()

## Longer list with company full names
# d %>%
#   mutate(company_short = str_extract(ProBusiness, ferc_short)) %>%
#   drop_na(company_short) %>%
#   mutate(members = paste(CongressPerson, Cosigned_House, Cosigned_Senate, sep = ";")) %>%
#   mutate(members = str_remove_all(members, ";NA")) %>%
#   group_by(company_short) %>%
#   mutate(members = str_c(unique(members), collapse = ";")) %>%
#   select(company_short, members) %>%  distinct() %>%
#   left_join(FERC_corps) %>% distinct() %>%
#   knitr::kable(caption = "Pro Businesses matching those registered with FERC")

#############Anti-Business##########

dcorps %>%
  mutate(company_short = str_extract(AntiBusiness, ferc_short)) %>%
  left_join(FERC_corps) %>% 
  distinct() %>%
  group_by(company_short_refs) %>%
  select(ID, last_name, company_short, company_short_refs) %>%
  filter(company_short != "")%>%
  distinct() %>%
  summarise( n = n() , 
             company_short = str_c(unique(company_short), collapse = "; "),
             members = str_c(unique(last_name), collapse = "; "),
             ID = str_c(unique(ID), collapse = "; ")) %>%
  filter(n>quantile(n,.9))%>%
  select(company_short, n, everything()) %>%
  arrange(-n) %>%
  knitr::kable()

## Longer list with company full names
# d %>% 
#   mutate(company_short = str_extract(AntiBusiness, ferc_short)) %>% 
#   drop_na(company_short) %>% 
#   mutate(members = paste(CongressPerson, Cosigned_House, Cosigned_Senate, sep = ";")) %>%
#   mutate(members = str_remove_all(members, ";NA")) %>% 
#   group_by(company_short) %>% 
#   mutate(members = str_c(unique(members), collapse = ";")) %>% 
#   select(company_short, members) %>%  distinct() %>%
#   left_join(FERC_corps) %>% distinct() %>% 
#   knitr::kable(caption = "Anti Businesses matching those registered with FERC")


##################Company##########

# Split out to one obs per member per letter per business
dcorps <- d %>%
  select(ID, bioname, ProBusiness, AntiBusiness, ProProject, AntiProject) %>% 
  gather(value = "company", key = "position", -ID, -bioname) %>%
  mutate(company = company %>% str_split(";")) %>%
  unnest(company) %>% distinct()

dcorps %<>% mutate(company = company %>%
                     str_replace("RTO", 
                                 "Regional Transmission Organization") %>%  
                     str_remove_all(" Company.*| \\(.*| Inc.*| LLC.*| Energy.*") )

dcorps$company %<>%
  trimws()  # trim white space before and after company names

dcorps %<>%   filter(nchar(company)>1) %>%# drop names less than 1 character
  distinct() # drop duplicates

## For matching, remove endings like ", Inc." etc. 
trim <- function(d){
  d %>%
    str_remove_all("Village of |\\(.*|,.*| -.*|^Proposed | Pipe.*| pipe.*| project$| Proiects$| Company.*| Comnany| Corp.*| Co$| Co.$| Energy$| energy$| power$| Power marketing$| Holdings.*| Group$| Gas$| Project.*| LLC| L.L.C.|  L.P.|^The | Inc.| L.P.| Ltd.| LP| Limited Partnership| Limited Liability|  Facility| Transportation and Storage| Storage and Transportation") %>% 
    str_remove_all("^, ") %>%
    trimws()  # filter out names less than 2 characters 
}



dcorps$company_short <- trim(dcorps$company) %>% trim()

# Problem Names
dcorps %>% filter(nchar(company_short)<2) %>% knitr::kable()

dcorps %<>% filter(nchar(company_short)>1)

## Correct over-trimmed names
dcorps %<>% 
  mutate(company_short = ifelse(company == "Columbia Energy Power Marketing Corporation", "Columbia Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Michigan Energy Exchange, LLC", "Michigan Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Midwest Energy, Inc.", "Midwest Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Northwest Pipeline LLC", "Northwest Pipeline", company_short))%>%
  mutate(company_short = ifelse( str_detect(company, "^Dominion"), 
                                 "Dominion", company_short))%>%
  mutate(company_short = ifelse(company == "Peak Energy, Inc.", "Peak Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Public Service Company of New Mexico", "Public Service Company of New Mexico", company_short))%>%
  mutate(company_short = ifelse(company == "Salt Lake Energy Systems, LLC", "Salt Lake Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Natural Gas Pipeline Company of America", "Natural Gas Pipeline Company of America", company_short))%>%
  mutate(company_short = ifelse(company == "Natural Gas Trading Corporation", "Natural Gas Trading Corporation", company_short))%>%
  mutate(company_short = ifelse(company == "Energy, USA-TPC Corp.", "TPC", company_short))%>%
  mutate(company_short = ifelse(company == "New Energy Partners, L.L.C.", "New Energy Partners", company_short))%>%
  mutate(company_short = ifelse(company == "New Energy Service, LLC", "New Energy Service", company_short))%>%
  mutate(company_short = ifelse(company == "North Energy Associates, A Limited Partnership", "North Energy Associates", company_short))%>%
  mutate(company_short = ifelse(company == "Pipeline Company LLC", "Pipeline Company LLC", company_short))%>%
  mutate(company_short = ifelse(company == "Express Pipeline LLC", "Express Pipeline LLC", company_short))%>%
  mutate(company_short = ifelse(company == "American Energy Savings, Inc.", "American Energy Savings", company_short))%>%
  mutate(company_short = ifelse(company == "Electric Energy, Inc.", "Electric Energy, Inc.", company_short))%>%
  mutate(company_short = ifelse(company == "System Energy Resources, Inc., Llano Estacado Wind, LLC, System Energy Resources, Inc., Northern Iowa Windpower, LLC, and RS Cogen, L.L.C.", "System Energy Resources", company_short))%>%
  mutate(company_short = ifelse(company == "Public Service Company of Colorado", "Public Service Company of Colorado", company_short))%>%
  mutate(company_short = ifelse(company == "Southern Company Services, Inc.", "Southern Company", company_short))%>%
  mutate(company_short = ifelse(company == "International Energy Consultants", "International Energy Consultants", company_short))%>%
  mutate(company_short = ifelse(company == "Jackson Pipeline Company", "Jackson Pipeline", company_short))%>%
  mutate(company_short = ifelse(company == "Southern Energy California", "Southern Energy California", company_short))%>%
  mutate(company_short = ifelse(company == "Tennessee Gas Pipeline Company, L.L.C.", "Tennessee Gas Pipeline", company_short))%>%
  mutate(company_short = ifelse(company == "Select Energy New York, Inc.", "Select Energy New York", company_short))%>%
  mutate(company_short = ifelse(company == "Inland Corporation", "Inland Corporation", company_short))%>%
  mutate(company_short = ifelse(company == "Advanced Energy Systems, Inc.", "Advanced Energy Systems", company_short))%>%
  mutate(company_short = ifelse(company == "Consumers Energy Company", "Consumers Energy Company", company_short))%>%
  mutate(company_short = ifelse(company == "Boston Energy Trading and Marketing LLC", "Boston Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Complete Energy Services, Inc.", "Complete Energy Services", company_short))%>%
  mutate(company_short = ifelse(company == "El Paso Energy Intrastate, L.P.", "El Paso Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Houston Pipe Line Company LP", "Houston Pipe Line", company_short))%>%
  mutate(company_short = ifelse(company == "North American Energy, Inc.", "North American Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Spokane Energy, LLC", "Spokane Energy", company_short))%>%
  mutate(company_short = ifelse(company == "Salem Energy Systems, LLC", "Salem Energy", company_short))%>%
  mutate(company_short = ifelse(company_short == "Direct", "Direct Energy", company_short))%>%
  mutate(company_short = ifelse(company_short == "DC", "DC Energy", company_short))%>%
  mutate(company_short = ifelse(company_short == "PS", "PS Energy Group", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Coastal", "Coastal Pipeline", company_short)) %>%
  mutate(company_short = ifelse(company_short == "CT", "CT Corporation", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Forest", "Forest Pipeline", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Minnesota", "Minnesota Pipe Line", company_short)) %>%
  mutate(company_short = ifelse(company_short == "NW", "NW Pipeline", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Ohio River", "Ohio River Pipe Line", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Rio Grande", "Rio Grande Pipeline", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Plains", "Plains Pipeline", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Portland", "Portland Pipe Line", company_short)) %>%
  mutate(company_short = ifelse(company_short == "AP", "AP Holdings", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Ohio River", "Ohio River Pipe Line", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Boundary", "Boundary Gas", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Columbia", "Columbia Energy", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Independence", "Independence Energy Group", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Lakeside", "Lakeside Energy", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Mitchell", "Mitchell Energy", company_short)) %>%
  mutate(company_short = ifelse(company_short == "Stream", "Stream Energy", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, "^Northwest Pipeline"), "Northwest Pipeline", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, "^Tennessee Gas"), "Tennessee Gas", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, "Juniper Hills Country Club"), "Juniper Hills Country Club", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Columbia Gas", ignore_case = T)), "Columbia Gas", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Algonquin", ignore_case = T)), "Algonquin", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Spectra", ignore_case = T)), "Spectra", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Jordan Cove", ignore_case = T)), "Jordan Cove", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Enron", ignore_case = T)), "Enron", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, regex("Nexus", ignore_case = T)), "Nexus", company_short))%>%
  
  mutate(company_short = ifelse(company == "Express Pipeline", "Express Pipeline", company_short))%>%
  mutate(company_short = ifelse(company %in% c("American Energy", "American energy"), "American Energy", company_short))%>%
  mutate(company_short = ifelse(str_detect(company, "PG&E|Pacific Gas & Electric|Pacific Gas and Electric"), "PG&E", company_short)) %>% 
  mutate(company_short = ifelse(company %in% c("Pacific Corp.", "Pacific Corps"), "PacifiCorp", company_short)) %>% 
  mutate(company_short = ifelse(company_short == "AI", "AI Energy", company_short)) %>% 
  mutate(company_short = ifelse(company_short %in% c("N/A","Na","NO",
                                                     "Clean Water Act","Na",
                                                     "Energy", "West", "NA",
                                                     "Central", "Power", "Natural",
                                                     "Consumers", "LNG"),
                                "NOT A COMPANY?",  company_short))



#FIXME
# ADD company_short_refs 
dcorps %<>% 
  group_by(company_short) %>% 
  mutate(company_short_refs = str_c(unique(company), collapse = "; ")) %>% ungroup()

## Inspect over-trimmed company names
dcorps %>% filter(company_short %in% c("", "NA", "na", "Na", "LLC", "Energy", "Pipeline", "Natural", "North", "New", "Express", "gas", "American", "Electric", "System", "Direct", "DC", "PS", "CT", "Forest", "International", "Tennessee", "Salt Lake", "Lakeside", "NW", "Jackson", "Public Service", "Peak", "Independence", "Select", "Portland", "Michigan", "Boundary", "Coastal", "Minnesota", "Midwest", "Northeast", "Columbia", "Plains", "Stream", "Mitchell", "AP", "Southern", "Ohio River", "Northwest", "Rio Grande", "AI", "Transportation", "12/20/2018", "Salem", "Boston", "Spokane", "El Paso", "Houston", "North American", "Consumers", "Advanced", "Complete", "Inland", "Limited Partnership", "Sun", "RTO", "Pacific", "LNG")) %>% select(company_short, company_short_refs, ID) %>% 
  group_by(company_short) %>%
  summarise(company_short_refs = str_c(unique(company_short_refs), collapse = "; "),
            IDs = str_c(unique(ID), collapse = "; ")) %>%
  distinct() %>% knitr::kable()

# top corps
topcorps <- dcorps %>% group_by(company_short) %>% tally() %>% arrange(-n) %>% top_n(10)

filter(dcorps, company_short %in% topcorps$company_short) %>%   
  group_by(company_short) %>%
  summarise(#IDs = str_c(unique(ID), collapse = "; ")
    company_short_refs = str_c(unique(company_short_refs), collapse = "; ") ) %>%
  distinct() %>% knitr::kable() 

# corps with short names 
shortcorps <- dcorps %>% 
  ungroup() %>% 
  group_by(company_short) %>% 
  tally() %>% 
  arrange(nchar(company_short)) %>% 
  mutate(nchar = nchar(company_short))%>%
  top_n(10, -nchar) %>% distinct()

filter(dcorps, company_short %in% shortcorps$company_short) %>% 
  group_by(company_short) %>%
  summarise(company_short_refs = str_c(unique(company_short_refs), collapse = "; "),
            IDs = str_c(unique(ID), collapse = "; ")) %>%
  distinct() %>% knitr::kable()

#dcorps %>% filter(!position %in% c("ProProject", "AntiProject")) %>%  group_by(company_short) %>% tally() %>% arrange(-nchar(company_short))  %>% top_n(20) %>% knitr::kable()

library(googlesheets)
library(httpuv)

# log in to google drive
gs_auth() 

notcorps <- gs_title("not_companies") %>% gs_read() 


dcorps %<>% ungroup() %>% 
  mutate(company_short = ifelse(company_short %in% notcorps$not_companies,
                                "NOT A COMPANY?", company_short) )

#FIXME
# ADD company_short_refs again after corrections
dcorps %<>% 
  group_by(company_short) %>% 
  mutate(company_short_refs = str_c(unique(company), collapse = "; ")) %>% ungroup()

dcorps %>% 
  filter(position %in% c("ProProject", "ProBusiness")) %>% 
  select(ID, company_short, company)%>% 
  group_by(company_short) %>% 
  distinct() %>%
  add_count() %>% 
  arrange(-n) %>% 
  #write_csv(path = here("FERC/ProBusinesses.csv"))
  head()

# sum(is.na(unique(dcorps$company_short)))

## Make a single string of short corp names
dcorp_short <- unique(dcorps$company_short) %>% 
  str_c(collapse = "|") %>% 
  str_remove_all("||")

# bad string? 
# str_detect("", dcorp_short)


save(dcorp_short, file = here("data/dcorp_short.Rdata"))
save(dcorps, file = here("data/dcorps.Rdata"))

# notcorps$company

notcorps <- filter(dcorps, company_short == "NOT A COMPANY?")


sum(!is.na(d$ProBusiness))

d %<>% 
  mutate(ProBusiness = ifelse(ProBusiness %in% notcorps$company, NA, ProBusiness)) %>% 
  mutate(ProProject = ifelse(ProProject %in% notcorps$company, NA, ProProject))

sum(!is.na(d$ProBusiness))

##############PAC Contribution#########

library(tidyverse)
library(dplyr)
# load data 
load(here("data/contrib_total.rds"))

contrib_total %<>% 
  mutate(cycle = str_sub(ICPSR, -4)) %>% 
  filter(cycle < 2014) %>% distinct()


# inspect coverage 
contrib_total %>% 
  group_by(cycle, ICPSR) %>% 
  summarise(total = sum(value, na.rm = TRUE)/1000000) %>% 
  ungroup() %>% 
  group_by(cycle) %>% 
  summarise(mean = mean(total)) %>% 
  ggplot() + 
  aes(x = factor(cycle), y = mean) + 
  geom_col() + 
  labs(y = "Average Energy-sector PAC contributions (millions) \n (ommitting energy extraction companies)", 
       x = "FEC Electoral Cycle")

# merge with letters data, d
contrib_total %<>% 
  dplyr::rename(icpsryear = ICPSR,
                PAC_contributions = value) %>% 
  mutate(cycle = str_sub(icpsryear, -4),
         icpsr = as.numeric(str_remove(icpsryear, cycle)),
         cycle = as.numeric(cycle)) %>% 
  filter(cycle > 1989) %>% 
  left_join(d %>% dplyr::select(icpsr, name_state, party) %>% distinct())

# inspect for coverage
contrib_total %>% 
  drop_na(party) %>% 
  group_by(cycle, party, icpsr) %>% 
  summarise(total = sum(PAC_contributions, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(cycle, party) %>% 
  summarise(mean = mean(total)) %>% 
  ggplot() + 
  aes(x = factor(cycle), y = mean, fill = party) + 
  geom_col() + 
  scale_fill_grey() + 
  labs(y = "Average Energy-sector PAC contributions \n (ommitting energy extraction companies)", 
       fill = "Party", 
       x = "FEC Electoral Cycle")

letters_money <- d %>% 
  #FIXME
  filter(cycle <2014) %>% 
  filter(!is.na(ProBusiness)|!is.na(ProProject))%>% 
  #filter(Type %in% c("Corp. Constituent", "Policy")) %>% 
  group_by(icpsryear, cycle, name_state, party, congress, icpsr, chamber, oversight_committee) %>%
  summarise(ProBusiness_Letters = n()) %>% 
  #FIXME
  filter(cycle <2014) %>% 
  distinct()

nrow(letters_money)

# join letters and DIME data by ICPSR, adding a sum of sector PAC contributions per member
letters_money %<>% 
  ungroup() %>% 
  full_join(contrib_total) %>% 
  group_by(icpsr) %>% 
  # FIXME
  # one party per member, need to deal with party switchers
  mutate(party = paste(unique(party), collapse = ";")) %>% 
  mutate(party = str_remove(party, ";.*")) %>% 
  ungroup()%>% distinct()

nrow(letters_money)

# top 10 by contributions in a cycle 
letters_money %>% 
  filter(!is.na(ProBusiness_Letters)) %>% 
  dplyr::select(-icpsryear, -icpsr, -congress) %>% 
  top_n(10, PAC_contributions) %>% knitr::kable()

letters_money %<>% 
  mutate(ProBusiness_Letters = ifelse(is.na(ProBusiness_Letters) & cycle > 1997, 0, ProBusiness_Letters)) %>% distinct()

nrow(letters_money)

##############Ten PAC Recipients#########

letters_money_total <- letters_money %>% 
  filter(!is.na(PAC_contributions)) %>% 
  group_by(icpsr, name_state, party) %>% 
  summarise(ProBusiness_Letters_Per_Cycle = mean(ProBusiness_Letters, na.rm = TRUE),
            PAC_contributions = sum(PAC_contributions, na.rm = TRUE)) %>% 
  ungroup()

# add zeros 
letters_money_total %<>% 
  mutate(ProBusiness_Letters_Per_Cycle = ifelse(is.na(ProBusiness_Letters_Per_Cycle), 0, ProBusiness_Letters_Per_Cycle)) %>% distinct()

nrow(letters_money_total)

letters_money_total %>% filter(!is.na(name_state)) %>%  top_n(10, PAC_contributions) %>% knitr::kable()

Antiletters_money <- d %>%
  #FIXME
  filter(cycle <2014) %>% 
  filter(!is.na(AntiBusiness)|!is.na(AntiProject))%>% 
  #filter(Type %in% c("Corp. Constituent", "Policy")) %>% 
  group_by(icpsryear, cycle, name_state, party, congress, icpsr, chamber, oversight_committee) %>% distinct() %>% 
  summarise(AntiBusiness_Letters = n())

nrow(Antiletters_money)

# join letters and DIME data by ICPSR, adding a sum of sector PAC contributions per member
Antiletters_money %<>% 
  ungroup() %>% 
  full_join(contrib_total) %>% 
  group_by(icpsr) %>% 
  # FIXME
  # one party per member, need to deal with party switchers
  mutate(party = paste(unique(party), collapse = ";")) %>% 
  mutate(party = str_remove(party, ";.*")) %>% 
  ungroup() %>% distinct()

nrow(Antiletters_money)

unique(Antiletters_money$party)

# Add zero-letter-cycle observations:
Antiletters_money %<>% 
  mutate(AntiBusiness_Letters = ifelse(is.na(AntiBusiness_Letters) & cycle > 1997, 0, AntiBusiness_Letters)) 

Antiletters_money_total <- Antiletters_money %>% 
  filter(!is.na(PAC_contributions)) %>% 
  group_by(icpsr, name_state, party) %>% 
  summarise(AntiBusiness_Letters_Per_Cycle = mean(AntiBusiness_Letters, na.rm = TRUE),
            PAC_contributions = sum(PAC_contributions, na.rm = TRUE)) %>% 
  ungroup()%>% distinct()

nrow(Antiletters_money)

# constituent letters
All_Letters_money <- d %>% 
  #FIXME
  filter(cycle <2014) %>% 
  group_by(icpsryear, cycle, name_state, party, congress, icpsr, chamber, oversight_committee) %>%
  summarise(All_Letters = n()) %>% distinct()

# join letters and DIME data by ICPSR, adding a sum of sector PAC contributions per member
All_Letters_money %<>% 
  ungroup() %>% 
  full_join(contrib_total) %>% 
  group_by(icpsr) %>% 
  # FIXME
  # one party per member, need to deal with party switchers
  mutate(party = paste(unique(party), collapse = ";")) %>% 
  mutate(party = str_remove(party, ";.*")) %>% 
  ungroup()%>% distinct()

# Add zero-letter-cycle observations:
All_Letters_money %<>% 
  mutate(All_Letters = ifelse(is.na(All_Letters) & cycle > 1997, 0, All_Letters)) 

# Sum all donations since 1990
All_Letters_money_total <- All_Letters_money %>% 
  drop_na(PAC_contributions) %>% 
  group_by(icpsr, name_state, party) %>% 
  summarise(All_Letters_Per_Cycle = mean(All_Letters, na.rm = TRUE),
            PAC_contributions = sum(PAC_contributions, na.rm = TRUE)) %>% 
  ungroup()

# constituent letters
Non_Constituent_money <- d %>% 
  #FIXME
  filter(cycle <2014) %>% 
  drop_na(Constituent) %>% 
  #filter(Type %in% c("Corp. Constituent", "Policy")) %>% 
  group_by(icpsryear, cycle, name_state, party, congress, icpsr, chamber, oversight_committee) %>%
  summarise(Non_Constituent_Letters = n()) %>% distinct()

# join letters and DIME data by ICPSR, adding a sum of sector PAC contributions per member
Non_Constituent_money %<>% 
  ungroup() %>% 
  full_join(contrib_total) %>% 
  group_by(icpsr) %>% 
  # FIXME
  # one party per member, need to deal with party switchers
  mutate(party = paste(unique(party), collapse = ";")) %>% 
  mutate(party = str_remove(party, ";.*")) %>% 
  ungroup()%>% distinct()

# Add zero-letter-cycle observations:
Non_Constituent_money %<>% 
  mutate(Non_Constituent_Letters = ifelse(is.na(Non_Constituent_Letters) & cycle > 1997, 0, Non_Constituent_Letters)) %>% distinct()

Non_Constituent_money_total <- Non_Constituent_money %>% 
  drop_na(PAC_contributions) %>% 
  group_by(icpsr, name_state, party) %>% 
  summarise(Non_Constituent_Letters_Per_Cycle = mean(Non_Constituent_Letters, na.rm = TRUE),
            PAC_contributions = sum(PAC_contributions, na.rm = TRUE)) %>% 
  ungroup()%>% distinct()

###########PAC Specific Contributions###########

# load DIME data 
load(here("data/contrib_matrix.rds"))

contrib_matrix %<>% 
  mutate(cycle = str_sub(ICPSR, -4)) %>% 
  filter(cycle < 2014)

contrib_matrix %>% 
  mutate(cycle = str_sub(ICPSR, -4)) %>% 
  # FIXME 
  filter(cycle <2014) %>% 
  group_by(cycle, ICPSR) %>% 
  summarise(total = sum(value, na.rm = TRUE)/1000000) %>% 
  ungroup() %>% 
  group_by(cycle) %>% 
  summarise(mean = mean(total)) %>% 
  ggplot() + 
  aes(x = factor(cycle), y = mean) + 
  geom_col() + 
  labs(y = "Average Energy-sector PAC contributions per member (millions) \n (ommitting energy extraction companies)", 
       x = "FEC Electoral Cycle")

contrib_matrix %>% 
  mutate(cycle = str_sub(ICPSR, -4)) %>% 
  # FIXME
  filter(cycle < 2014) %>% 
  group_by(cycle, PACShort) %>% 
  summarise(total = sum(value, na.rm = TRUE)/1000000) %>%  # millions 
  ungroup() %>% 
  group_by(PACShort) %>% 
  mutate(PACtotal = sum(total, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(PAC = ifelse(PACtotal > quantile(PACtotal, .98), PACShort, NA)) %>% 
  ggplot() + 
  aes(x = factor(cycle), y = total, fill= PAC) + 
  geom_col() + 
  labs(y = "Energy-sector PAC contributions (millions) \n (ommitting energy extraction companies)", 
       x = "FEC Electoral Cycle",
       fill = "PACs in top 2%") + 
  scale_fill_viridis_d(option = "C", na.value = "grey50")

contrib_matrix %>% 
  mutate(cycle = str_sub(ICPSR, -4)) %>% 
  # FIXME
  filter(cycle < 2014) %>% 
  group_by(cycle, PACShort) %>% 
  summarise(PACcycle = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(PACShort) %>% 
  mutate(PACtotal = sum(PACcycle, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(PACtotal > quantile(PACtotal, .98)) %>%  
  arrange(-as.numeric(cycle)) %>% 
  kable()

# mere with letters data, d
contrib_matrix %<>% 
  dplyr::rename(icpsryear = ICPSR,
                PAC_contributions = value) %>% 
  mutate(cycle = str_sub(icpsryear, -4),
         icpsr = as.numeric(str_remove(icpsryear, cycle)),
         cycle = as.numeric(cycle)) %>% 
  filter(cycle > 1990)%>% 
  left_join(d %>% select(icpsr, name_state, party) %>% distinct())

###########Top PAC Recipient in a cycle###########

letters_PACs <- d %>% 
  #FIXME
  filter(cycle <2014) %>% 
  filter(!is.na(ProBusiness)|!is.na(ProProject))%>% 
  group_by(icpsryear, cycle, name_state, party) %>%
  summarise(ProBusiness_Letters = n()) %>% 
  # join by ICPSR, adding a sum of sector PAC contributions per member
  left_join(contrib_matrix) %>% 
  ungroup() %>% 
  group_by(icpsr) %>% 
  mutate(party = paste(unique(party), collapse = ";")) %>% 
  mutate(party = str_remove(party, ";.*")) %>% 
  ungroup()

letters_PACs_cycle <- letters_PACs %>% 
  group_by(cycle, icpsr, name_state, party) %>%
  # delete PACs that did not give from list, but keep members that recieved no contributions
  mutate(PACShort = ifelse(PAC_contributions > 0, PACShort, NA)) %>% 
  summarise(PAC_contributions = sum(PAC_contributions),
            PACs = paste(unique(PACShort), collapse = ";")) %>% 
  ungroup() %>% 
  arrange(-PAC_contributions)

letters_PACs_cycle %>% top_n(1, PAC_contributions) %>% knitr::kable()

#########Overall########

letters_PACs_total <- letters_PACs %>% 
  ungroup() %>% 
  group_by(icpsr, name_state) %>% 
  summarise(PAC_contributions = sum(PAC_contributions),
            PACs = str_c(unique(PACShort), collapse = ";")) %>% 
  filter(!is.na(name_state)) %>% 
  ungroup()

letters_PACs_total %>% top_n(1, PAC_contributions) %>% knitr::kable()

#########Pro-Business#########

top1 <- top_n(letters_money_total %>% 
                filter(!is.na(name_state)), 5, PAC_contributions) %>%
  .$name_state

top2 <- top_n(letters_money_total, 5, ProBusiness_Letters_Per_Cycle) %>% 
  .$name_state

letters_money_total %>% 
  mutate(name_state = as.character(name_state)) %>%
  ggplot() + 
  aes(x = PAC_contributions, 
      y = ProBusiness_Letters_Per_Cycle) + 
  geom_point(alpha = .5) + 
  geom_text(aes(label = ifelse(name_state %in% top2,
                               name_state, NA)), 
            check_overlap = TRUE, 
            hjust = 0, 
            color = "blue") + 
  labs(x = "PAC Contributions 1990-2014",
       y = "Average Pro-Business Letters sent to FERC per Cycle")

letters_money_total %>% 
  mutate(name_state = as.character(name_state)) %>%
  ggplot() + 
  coord_flip() + 
  aes(x = PAC_contributions, 
      y = ProBusiness_Letters_Per_Cycle) + 
  geom_point(alpha = .5) + 
  geom_text(aes(label = ifelse(name_state %in% top1, name_state, NA)), 
            check_overlap = TRUE, 
            hjust = 0, 
            color = "blue") + 
  labs(x = "PAC Contributions 1990-2014",
       y = "Average Pro-Business Letters sent to FERC per Cycle")

library(dplyr)

# MERGE IN LETTERS DATA TO PRO-BUSINESS LETTERS
# FIXME 
# CALLE THIS Pro_letters and Antiletters Anti_letters 
nrow(letters_money)
letters_money %<>% left_join(d %>% 
                               dplyr::select(icpsryear, # nominate.dim1, #chair_since_2007,
                                             #oversight_committee, 
                                             chamber) %>% distinct() #%>% dplyr::rename(chair_since_2000 = chair_since_2007)
) %>% distinct()
nrow(letters_money)

nrow(All_Letters_money)
All_Letters_money %<>% distinct() %>% 
  left_join(d %>% 
              dplyr::select(icpsryear, #nominate.dim1, #chair_since_2007,
                            #oversight_committee, 
                            chamber) %>% distinct() #%>% dplyr::rename(chair_since_2000 = chair_since_2007)
  ) %>% distinct()
nrow(All_Letters_money)

nrow(Antiletters_money)
Antiletters_money %<>% left_join(d %>% 
                                   dplyr::select(icpsryear, #nominate.dim1, chair_since_2007, 
                                                 #oversight_committee, 
                                                 chamber) %>% distinct() #%>% dplyr::rename(chair_since_2000 = chair_since_2007)
)%>% distinct()
nrow(Antiletters_money)

# MERGE IN LETTERS DATA TO NONCONSTITUENT LETTERS COUNTS  

###############Models##########

mplot <- function(m){m %>%
    tidy(conf.int = TRUE) %>%
    filter(term!="(Intercept)") %>%
    ggplot() + 
    aes(x = term,
        y = estimate, 
        ymin = conf.low, 
        ymax = conf.high)+ 
    geom_hline(yintercept = 0, linetype =2) + 
    geom_pointrange() + coord_flip() }

All_Letters_money %<>% 
  mutate(Log_PAC_contributions = log(PAC_contributions+1))  

mAll <- glm(All_Letters ~ PAC_contributions, data = All_Letters_money %>%
              mutate(PAC_contributions = PAC_contributions/1000000), 
            family = poisson)  
tidy(mAll)

mAll %>% mplot()+
  labs(x="", y="Total additional letters per congress per million dollars")

letters_money %<>% 
  #drop_na(ProBusiness_Letters, PAC_contributions) %>%
  mutate(Log_PAC_contributions = log(PAC_contributions+1))  

mPro <- glm(ProBusiness_Letters ~ PAC_contributions, data = letters_money %>% mutate(PAC_contributions = PAC_contributions/1000000), family = poisson)  
tidy(mPro)

mPro %>% mplot()+
  labs(x="", y="Additional pro-business letters per congress per million dollars")

mProLog <- glm(ProBusiness_Letters ~ Log_PAC_contributions, data = letters_money, family = poisson  ) 
tidy(mProLog)

mProLog %>% mplot()+
  labs(x="", y="Additional letters per congress per Log(PAC Contributions)")

mNonConstituent <- glm(Non_Constituent_Letters ~ PAC_contributions, data = Non_Constituent_money, family = poisson  ) 
tidy(mNonConstituent)

mNonConstituent %>% mplot()+
  labs(x="", y="Additional non-constituent letters per congress per million dollars")

mNonConstituentLog <- glm(Non_Constituent_Letters ~ Log_PAC_contributions, data = Non_Constituent_money %>% mutate(Log_PAC_contributions = log(PAC_contributions+1)), family = poisson  ) 
tidy(mNonConstituent)

mNonConstituent %>% mplot()+
  labs(x="", y="Additional non-constituent letters per congress per logged million dollars")

##########Party Time#########

mProParty <- glm(ProBusiness_Letters ~ party, data = letters_money %>% filter(!party %in% c("NA","(I)", "Independent")), family = "poisson" ) 

tidy(mProParty)

mplot(mProParty) +
  labs(x="", y="Additional pro-business letters per congress")

# PAC + party:
mPro_pac_party<- glm(ProBusiness_Letters ~ PAC_contributions + party, 
                     data = letters_money %>% 
                       filter(!party %in% c("NA","(I)", "Independent"))%>% 
                       mutate(PAC_contributions = PAC_contributions/1000000), family = "poisson" )

tidy(mPro_pac_party)

mplot(mPro_pac_party)+
  labs(x="", y="Additional letters per congress")

# PAC*party interaction:
mPro_pacXparty<-glm(ProBusiness_Letters ~ PAC_contributions * party, 
                    data = letters_money %>% 
                      filter(!party %in% c("NA","(I)", "Independent"))%>%
                      mutate(PAC_contributions = PAC_contributions/1000000), family = "poisson" )  

tidy(mPro_pacXparty)


# letter count per pac ~ pac contributions (lots of 0$-0letter pairs )
# 1. id pacs per letter - spit letters matching several pacs 
# 2. count up per member 
# 3. add 0s for all other pac-member-cycle compinations 
# merge in with contributions per pac per member 

# First, clean up the contributions data a bit

# a helper function to concat unique strings
unique_string <- . %>% 
  str_split(";") %>% 
  # select unique ones
  unlist() %>% 
  unique() %>% 
  trimws() %>% 
  # paste them back together to retrun a single value 
  paste(collapse = ";") %>%
  str_remove_all("na;|;na$|NA;|;NA$")

# Just the PAC names and IDs from the contributions matrix (will merge in relevent contributions later)
PACs <- contrib_matrix %>% 
  select(comid, UltOrg,Affiliate,PACShort) %>% distinct() %>% 
  # make a full list of pac-affiliated orgs
  group_by(comid) %>%
  mutate(UltOrg2 = str_c(UltOrg,Affiliate,PACShort, sep = ";") %>% 
           tolower() %>% 
           # select unique ones
           unique_string())

# A function to get matching names
get_pac <- function(corp, PAC){
  ifelse(str_detect(tolower(PAC), tolower(corp)), PAC, NA)
}

# A function to get IDs where names match 
get_pacID <- function(corp, PAC, PACid){
  ifelse(str_detect(tolower(PAC), tolower(corp)), PACid, NA) 
}


# corp = "Exxon"

# A function to augment the PAC data with any matching company names 
get_pacs <- function(corp){
  PACs %>% 
    mutate(pacs = get_pac(tolower(corp), UltOrg2),
           pacIDs = get_pacID(tolower(corp), UltOrg2, comid)) %>% 
    ungroup() %>% 
    select(pacs, pacIDs) %>% 
    distinct() %>% 
    summarise_all(unique_string) %>% 
    mutate(company_short = corp)
}

# TESTING 
# corp = "Exxon"
# get_pacs(corp)
# corp <- dcorps$company_short[1]
# get_pacs(corp)

crosswalk <- map_dfr(dcorps$company_short, get_pacs)

crosswalk %<>% distinct()

# use crosswalk to merge in contribution data 
dpacs <- dcorps %>% 
  full_join(crosswalk) %>% 
  # letters per year per pac
  select(ID, position, company_short, position, pacs, pacIDs) %>% 
  distinct() %>% 
  mutate(pacIDs = str_split(pacIDs, ";")) %>% 
  unnest(pacIDs) %>% 
  distinct() %>% 
  # merge in pac totals
  dplyr::rename(comid = pacIDs) %>% 
  # merge in contribution data
  left_join(contrib_matrix) # %>% group_by(comid, position) %>% summarise(PAC_contributions)

dpacs %<>% 
  ungroup() %>% 
  filter(!is.na(ID), !is.na(comid)) %>% 
  select(ID, icpsryear, cycle, name_state, position, company_short, pacs, comid, PAC_contributions) %>% 
  distinct()
Pro-business letters linked to PAC contributions:
  dpacs %>% 
  filter(position %in% c("ProBusiness", "ProProject")) %>% 
  # select letters with an identified person
  drop_na(name_state, ID) %>%
  group_by(name_state, cycle, position, company_short, pacs, comid) %>% 
  distinct()%>%  summarise(PAC_contributions = sum(PAC_contributions, na.rm = T),
                           # FIXME 
                           # SHOULD NOT NEED UNIQUE HERE 
                           Letter_IDs = paste(unique(ID), collapse = ";")) %>% 
  ungroup() %>% 
  group_by(position)  %>%
  top_n(10, PAC_contributions) %>% 
  knitr::kable()
Anti-business letters linked to PAC contributions:
  dpacs %>% 
  filter(position %in% c("AntiBusiness", "AntiProject")) %>% 
  left_join(d %>% select(ID, Constituent) %>% distinct() ) %>% 
  filter(Constituent == "No") %>%
  # select letters with an identified person
  drop_na(name_state, ID) %>%
  group_by(name_state, cycle, position, company_short, pacs, comid) %>% 
  distinct()%>%  summarise(PAC_contributions = sum(PAC_contributions, na.rm = T),
                           # FIXME 
                           # SHOULD NOT NEED UNIQUE HERE 
                           Letter_IDs = paste(unique(ID), collapse = ";")) %>% 
  ungroup() %>% 
  group_by(position)  %>%
  top_n(10, PAC_contributions) %>% 
  knitr::kable()

#Before connecting companies to the PAC donations of their parent companies, we find `r sum(!is.na(dpacs$ID))` letters with a pack donation.
################################################   
# merge in and split out parent corps 


# residuals 


#############Letters on behalf of projects in their districts##############


d %>% 
  mutate(Place_District = tolower(Place_District)) %>% 
  # FIXME 
  # dropping odd observations, will correct in DOE_FERC.R script 
  filter(Place_District %in% c("yes", "no", NA)) %>% 
  filter(letter_position %in% c("AntiBusiness", "ProBusiness")) %>% 
  filter(Constituent != "Not coded") %>% 
  count(Place_District, letter_position, Constituent) %>% 
  ggplot() +
  aes(x = Place_District, y = n, fill = letter_position) +
  geom_col() + 
  facet_grid(Constituent ~ .)+ 
  labs(x = "Is the proposed project in the member's district?",
       y = "Number of letters",
       fill = "")

