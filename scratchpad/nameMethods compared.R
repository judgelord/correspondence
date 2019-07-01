look1 <- data %>% extractMemberName(members, "FROM") %>% mutate(method = 1)
look2 <- data %>% extractMemberName2(members, "FROM") %>% mutate(method = 2)

problems <- full_join(look1,
                      look2) %>% 
  select(FROM, typos, correct, first_name, last_name, string, pattern, congress) %>% 
  distinct() %>% 
  filter(!is.na(FROM), FROM != "", FROM != "NA") %>% 
  arrange(FROM)

p <- problems %>% # and merge with voteview data
  left_join(members2) %>% # merge on common variables (may differ)
  select(congress, FROM, bioname) %>% 
  #left_join(members) %>% # merge again now that we have selected only certian bits of agency data 
  left_join(members2) %>% # merge on common variables (may differ)
  distinct()

