look1 <- data %>% top_n(100, DATE) %>% extractMemberName(members, "FROM") %>% mutate(method = 1)
look2 <- data %>% top_n(100, DATE) %>% extractMemberName2(members, "FROM") %>% mutate(method = 2)

problems <- full_join(look1,
                      look2) %>% 
  select(ID, FROM, typos, correct, first_name, last_name, string, pattern, congress) %>% 
  # drop observations that matched or did not match in both methods
  distinct() %>% 
  filter(!is.na(FROM), FROM != "", FROM != "NA") %>% 
  count(ID) %>% 
  # subset to observations that were distinct
  filter(n >1) %>% 
  # join back in data 
  left_join(look1) %>% 
  left_join(look2) %>% 
  arrange(FROM)

p <- problems %>% # and merge with voteview data
  left_join(members2) %>% # merge on common variables (may differ)
  select(congress, FROM, bioname, method) %>% 
  #left_join(members) %>% # merge again now that we have selected only certian bits of agency data 
  left_join(members2) %>% # merge on common variables (may differ)
  distinct()

