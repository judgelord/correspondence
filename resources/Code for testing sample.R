source("nameCongress.R") #loads congress members
source("setup.R")      #set up
# # Testing 
look<-data %>%
    filter(is.na(last_name)) %>%
    count(FROM,congress) %>%
    arrange(-n)
d1 <- look %>% filter(congress>109) %>% extractMemberName(members = members, col_name = "FROM")
d2 <- look %>% filter(congress<110) %>% extractMemberName(members = members_106to109th, col_name = "FROM")
look <- full_join(d1, d2)