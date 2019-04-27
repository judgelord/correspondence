write.csv(gs_title("FOIA List") %>% gs_read() %>% select(Department, agency) %>% filter(!is.na(agency)) %>% distinct(), 
          file = "data/_FOIA_list.csv",
          row.names = F) 
