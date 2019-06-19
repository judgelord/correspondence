
# FIXME 
# REWRITE WITH purrr
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


extractName <- function(from){
purrr::map(.x = members %>% filter(congress %in% data$congress) %>% select(pattern), 
           .f= str_extract_1,
           string = from) %>% 
  unlist() %>%
  na.omit() %>% 
  unique()  %>%
  str_c(collapse = ";") 
}

extractName("adfad Peter Welch a dfd ")

data %>% 
  mutate(from = tolower(FROM),
         name = extractName(from)) %>% 
  select(FROM, from, name)

# Rewrite with join? 
data %<>% 
  mutate(first_last = str_extract(str_c(members$f)))
  
  
  # data %>% mutate(FROM2 = map2(FROM, members, extractNames))










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