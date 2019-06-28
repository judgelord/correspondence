look1 <- data %>% extractMemberName(members, "FROM") %>% mutate(method = 1)
look2 <- data %>% extractMemberName2(members, "FROM") %>% mutate(method = 2)

problems <- full_join(look1,# %>% select(-FROM2), 
                      look2) %>% # %>% select(-pattern)) %>% 
  select(FROM, typos, correct, first_name, last_name, pattern, congress) %>% 
  distinct() %>% 
  filter(!is.na(FROM), FROM != "", FROM != "NA") %>% 
  arrange(FROM)


members2 <- full_join(members, members_106to109th) 


p <- problems %>% # and merge with voteview data
  left_join(members2) %>% # merge on common variables (may differ)
  select(congress, FROM, bioname) %>% 
  #left_join(members) %>% # merge again now that we have selected only certian bits of agency data 
  left_join(members2) %>% # merge on common variables (may differ)
  distinct()

