look1 <- data %>% top_n(100, DATE) %>% extractMemberName(members, "FROM") %>% mutate(method = 1, ID = as.numeric(ID))
look2 <- data %>% top_n(100, DATE) %>% extractMemberName2(members, "FROM") %>% mutate(method = 2, ID = as.numeric(ID))

look <- full_join(look1, look2)

problems <- look %>% 
  select(ID, FROM, typos, correct, first_name, last_name, string, pattern, congress) %>% 
  # drop observations that matched or did not match in both methods
  distinct() %>% 
  filter(!is.na(FROM), FROM != "", FROM != "NA") %>% 
  count(ID) %>% 
  # subset to observations that were distinct
  filter(n >1) %>% 
  # join back in data 
  left_join(look)%>% 
  arrange(FROM) %>% 
  select(FROM, string, pattern, first_name, last_name, method,  congress) 

should_be_matching <- problems %>% 
  filter(is.na(last_name), 
         pattern != "404error") %>% 
  drop_na(pattern) %>% 
  select(FROM, string, pattern, congress) %>% 
  left_join(members2) %>% # merge on common variables (may differ)
  select(FROM, string, pattern, bioname, congress) %>% 
  distinct()

