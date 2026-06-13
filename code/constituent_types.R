# Auto-code constituent_type

# get constituent_types google sheet
constituent_types <- gs_title(".constituent_types") %>% gs_read() 

blank_na <- . %>% replace_na(replace = " ")

constituent_types %<>% 
  select(agency, keywords, constituent_type, constituent_class, if_type, TYPE) %>% 
  mutate_all(blank_na)

# multiple keywords that indicate the same constituent type and class are separated by ";"
constituent_types %<>% mutate(keywords = str_split(keywords, ";|,") ) %>% 
  unnest(keywords) %>% mutate(keywords = str_squish(keywords) %>% str_to_lower())

constituent_types %<>% filter(nchar(keywords) > 3)

constituent_types$keywords

# a function to add constituent_type (from constituent_types sheet) to CONSTITUENT_TYPE (in the data)
#FIXME WITH PURRR
add_constituent_type <- function(d, keyword, constituent_type, constituent_class, type, iftype, Agency){
  
  d %<>% mutate(CONSTITUENT_TYPE = ifelse(str_detect(str_to_lower(SUBJECT), keyword) & agency %in% Agency & (TYPE %in% iftype | iftype == " "), # if keword in subject
                                          paste(constituent_type, CONSTITUENT_TYPE, sep = ";"), # add constituent_type
                                          CONSTITUENT_TYPE) %>% 
                  str_remove_all(";NA|;NULL|^NULL|^;")) 

  d %<>% mutate(CONSTITUENT_CLASS = ifelse(str_detect(str_to_lower(SUBJECT), keyword) & agency %in% Agency & (TYPE %in% iftype | iftype == " "), # if keword in subject
                                          paste(constituent_class, CONSTITUENT_CLASS, sep = ";"), # add constituent_type
                                          CONSTITUENT_CLASS) %>% 
                  str_remove_all(";NA|;NULL|^NULL|^;")) 
  
  d %<>% mutate(TYPE = ifelse(str_detect(str_to_lower(SUBJECT), keyword) & agency %in% Agency & is.na(TYPE), # if keword in subject
                              type, 
                              TYPE))
                              
return(d)
}

#FIXME with purrr
for(i in 1:dim(constituent_types)[1]){
d %<>% 
    add_constituent_type(constituent_types$keywords[i], 
                         constituent_types$constituent_type[i], 
                         constituent_types$constituent_class[i],
                         constituent_types$TYPE[i],
                         constituent_types$if_type[i],
                         constituent_types$agency[i])
  
  d %<>% mutate_at(vars(CONSTITUENT_CLASS, CONSTITUENT_TYPE), str_squish)
  }

# # inspect 
# d %>% select(SUBJECT, CONSTITUENT_TYPE, agency) 
# d %>% filter(!is.na(CONSTITUENT_TYPE)) %>% .$CONSTITUENT_TYPE %>% unique()
# d %>% filter(!is.na(CONSTITUENT_CLASS)) %>% .$CONSTITUENT_CLASS %>% unique()
# 
# # FOR TESTING 
# d$CONSTITUENT_TYPE <- NA
# d$CONSTITUENT_CLASS <- NA

## testing with NARA
# keywords = "father"
# constituent_type = "military family"



 