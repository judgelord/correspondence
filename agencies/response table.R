


# get FOIAed agency totals from FOIA List on drive 
data <- gs_title("FOIA List") %>% gs_read() %>% select(Department, Bureau, sample, data, on_drive) %>% filter(!is.na(sample)) %>% distinct() #%>% group_by(agency) %>% tally()

data$data <-gsub("yes.*", 1, data$data)
data$data <-gsub("no.*", 0, data$data)
data$data1 <- data$data
data$data1 %<>% as.numeric()

data$on_drive <-gsub("yes.*", 1, data$on_drive)
data$on_drive <-gsub("no.*", 0, data$on_drive)
data$on_drive %<>% as.numeric()
data$on_drive


data %<>% mutate(Bureau = ifelse(is.na(Bureau), Department, Bureau))

data %<>% mutate(Department = ifelse(!grepl("Department of", Department), "Independent Agencies", Department))

data %<>% group_by(Department) %>% mutate(Components = n(), Records = sum(data1), Coded = sum(on_drive)) %>% distinct()

data %<>% group_by(Department, Components, Records, Coded) %>% tally() %>% select(-n)

# 
# # get letter totals from df 
# dfdata <- df %<>% mutate(Department = ifelse(!grepl("Department of", Department), "Independent Agencies", Department))
# 
# dfdata %<>% group_by(Department) %>% summarise(n = n() ) %>% distinct()
# 
# # join foia sheet data with r data 
# data %<>% full_join(dfdata) 
# 
# data %<>% mutate(n = ifelse(is.na(n), 0, n))

# totals 
data %<>% ungroup() %>%
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

library(stargazer)

stargazer(data,  summary=F, rownames=FALSE)

