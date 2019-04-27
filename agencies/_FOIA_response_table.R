# corrections
df %<>% mutate(Department = ifelse(department == "DHS", "Department of Homeland Security", Department))
df %<>% mutate(Department = ifelse(department == "DOC", "Department of Commerce", Department))
df %<>% mutate(Department = ifelse(department == "DOD", "Department of Defense", Department))
df %<>% mutate(Department = ifelse(department == "DOT", "Department of Transportation", Department))


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



# # get letter totals from df
dfdata <- df %>% mutate(Department = ifelse(!grepl("Department of", Department), "Independent Agencies", Department))

# count
# dfdata %<>% group_by(Department) %>% summarise(n = paste(unique(agency), collapse = ":")) %>% distinct()
dfdata %<>% group_by(Department) %>% summarise(N =n()) %>% distinct()
#
# join foia sheet data with r data
data %<>% full_join(dfdata)
#
data %<>% mutate(N = ifelse(is.na(N), 0, N))

# totals 
data %<>% ungroup() %>%
  bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))

write.csv(data, file = "data/_FOIA_response_table.csv")

# library(stargazer)
# stargazer(data,  summary=F, rownames=FALSE)

