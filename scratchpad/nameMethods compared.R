
# a 5% sample 
sample <- sample_frac(data, .05)

# run both methods
method_1 <- sample %>% extractMemberName(members, "FROM") %>% mutate(method = 1, ID = as.numeric(ID))
method_2 <- sample %>% extractMemberName2(members, "FROM") %>% mutate(method = 2, ID = as.numeric(ID))

# combine results
methods_compared <- full_join(method_1, method_2) %>% arrange(FROM)

method_problems <- methods_compared %>% 
  select(ID, FROM, first_name, last_name, congress) %>% 
  # drop observations that matched or did not match in both methods
  distinct() %>% 
  filter(!is.na(FROM), FROM != "", FROM != "NA") %>% 
  count(ID) %>% 
  # subset to observations that were distinct in the two methods
  filter(n >1) %>% 
  # join back in data for these observations
  left_join(methods_compared)%>% 
  arrange(FROM) %>% 
  select(FROM, string, pattern, first_name, last_name, method,  congress) 

# is extractMemberName2 better? 
nrow(method_problems %>% filter(method == 1, is.na(last_name))) > nrow(method_problems %>% filter(method == 1), is.na(last_name)) 

method_2_should_be_matching <- method_problems %>% 
  filter(is.na(last_name), 
         pattern != "404error") %>% 
  drop_na(pattern) %>% 
  select(FROM, string, pattern, congress) %>% 
  left_join(members2) %>% # merge on common variables (may differ)
  select(FROM, string, pattern, bioname, congress) %>% 
  distinct()

