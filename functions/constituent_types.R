# Auto-code constituent_type

# get constituent_types google sheet
constituent_types <- googledrive::drive_ls() %>% 
  filter(name == ".constituent_types") %>% 
  googlesheets4::read_sheet()

# multiple keywords that indicate the same constituent type and class are separated by ";"
constituent_types %<>% mutate(keywords = str_split(keywords, ";|,") ) %>% 
  unnest(keywords) %>% mutate(keywords = str_squish(keywords))

# a function to add constituent_type (from constituent_types sheet) to CONSTITUENT_TYPE (in the data)
add_constituent_type <- function(d, keywords, constituent_type){
  
d %<>% mutate(CONSTITUENT_TYPE = ifelse(str_detect(str_to_lower(SUBJECT), str_to_lower(keywords) ), # if keword in subject
         paste(constituent_type, CONSTITUENT_TYPE, sep = ";"), # add constituent_type
         CONSTITUENT_TYPE)) %>% select(SUBJECT, CONSTITUENT_TYPE)# otherwise leave as is

return(d)

}

# testing with NARA
keywords = "father"
constituent_type = "military family"

d %<>% select(SUBJECT, CONSTITUENT_TYPE) 

for(i in dim(constituent_types)[1]){
d %<>% 
    add_constituent_type(keywords[i], constituent_type[i])
}







# Failed
## Not run:
constituent_types %<>% as.list()

d %>% purrr::map(constituent_types, .f = add_constituent_type)









d$CONSTITUENT_TYPE <- NA





case_when(str_detect(str_to_lower(SUBJECT), str_to_lower(keywords) ), # if keword in subject)
          