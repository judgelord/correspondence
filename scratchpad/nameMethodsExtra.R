
# FIXME 
# REWRITE WITH purrr


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
m <- members %>% 
  select(congress, bioname, pattern) %>% distinct()

# A function to get matching names
get_bioname <- function(FROM, pattern){
  ifelse(str_detect(tolower(FROM), tolower(pattern)), bioname, NA)
}

# A function to get IDs where names match 
get_bioname <- function(FROM, pattern, bioname){
  ifelse(str_detect(FROM, pattern), bioname, NA) 
}


get_bionames <- function(FROM){
  members %>% 
    mutate(bioname = get_bioname(FROM, pattern, bioname)) %>% 
    ungroup()
  return(members)
}

crosswalk <- map_dfr(data$FROM, get_bionames)

crosswalk %<>% distinct()








# git one 
extractName <- function(data){
  ifelse(str_detect(data, str_c(members$common_last, members$first_last, sep = "|")), 
         members$bioname, 
         NA)
}

# data %>% mutate(FROM2 = map_chr(FROM,  extractName))
str_extract_1 <- . %>% {str_extract_all(string = string, pattern = .) %>% 
    unlist() %>% 
    str_c(collapse = ";")}

getbioname <- function(members, person){
  m <- as.tibble(members)
  
  ifelse(str_detect(person, m$pattern),
         m$bioname,
         NA) %>% 
    unique()
}

map(members, .f = getbioname, person = "Peter Welch") 

str_extract_ignore <- function(string, pattern){
  str_extract(string, pattern)
}

str_extract_1 <- function(.x, string){
  str_extract(string = string, pattern = .x)
}

# git all 
extractMembers <- function(data){
  
  data %<>% 
    mutate(from = tolower(FROM)) %>%
    mutate(names = purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern) %>% as.list(), 
                              .f= str_extract_1,
                              string = from) %>% 
             unlist() %>%
             na.omit() %>% 
             unique()  %>%
             str_c(collapse = ";") )
  
  return(data)
}

data %>% extractMembers() %>% select(FROM, names)





















# 6-19

str_extract_1 <- function(.x, string){
  str_extract(string = string, pattern = .x)
}


extractName <- function(from){
purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern), 
           .f= str_extract_1,
           string = from) %>% 
  unlist() %>%
  na.omit() %>% 
  unique()  %>%
  str_c(collapse = ";") 
}

extractName(from = "foo peter welch bar ")

data %<>% 
  mutate(from = tolower(FROM))

data <- head(data)
data$FROM
data$from

map_chr(data$from, extractName)

data %>% 
  mutate(from = tolower(FROM),
         name = map(from, extractName) ) %>% 
  select(FROM, from, name)

# Rewrite with join? 
data %<>% 
  mutate(first_last = str_extract(str_c(members$f)))
  
# data %>% mutate(FROM2 = map2(FROM, members, extractNames))


















#####################################################
# THIS ONE WORKS

data$Summary <- data$FROM

# A helper function to return the full regex pattern string (so that we can join on pattern) where it finds a match
str_detect_replace <- function(string, pattern){
  out <- ifelse(str_detect(string, pattern), pattern, "404error")
}



# A function to map over members 
# (assumes that memmbers object contains congress and pattern)
# (assumes that data data contains congress and from)
extractName <- function(from){
  purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern), 
             .f= str_detect_replace,
             string = from) %>% 
    unlist() %>%
    unique() %>% 
    str_c(collapse = ";") %>%
    str_remove(";404error|404error;")
}

data %<>%
  ungroup() %>%
  #top_n(10) %>% 
  # map function to detect members over lower case version of FROM 
  mutate(from = tolower(Summary),
         pattern = map_chr(from, extractName) ) %>% # select(from,matches)
  # split out multiple members into separate rows 
  mutate(pattern = str_split(pattern, ";")  ) %>% 
  unnest() %>% 
  # join in members data by pattern 
  left_join(members %>% select(pattern, first_name, last_name, congress)) %>% 
  select(-from) # %>% select(FROM, pattern, first_name, last_name)

