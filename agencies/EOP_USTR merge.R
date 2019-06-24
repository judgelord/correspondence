source("setup.R")

d1 <- read_csv("EOP_USTR/EOP_USTR.csv")

d1 %<>% 
  mutate(DATE = as.Date(DATE, "%d-%b-%y"))

d2 <- read_csv("EOP_USTR/USTR 2017 and 2018.csv")

# this is better done in the clean maybe? 
# no, we'll do it here and then use this script again for new data 
d2 %<>% separate(Name, 
                 into = c("name", "topic", "date_letter"),
                 sep = " - ",
                 extra = "merge",
                 remove = F)

d2 %<>% 
  mutate(topic = ifelse(is.na(topic), 
                        str_extract(name, "-.*") %>% str_remove("^-"),
                        topic)) %>% 
  mutate(date_letter = ifelse(is.na(date_letter), 
                        str_extract(topic, "-.*")%>% str_remove("^-"),
                        date_letter)) %>% 
  mutate(topic = paste(topic, str_remove(date_letter, "[0-9].*"))) %>% 
  mutate(date_letter = str_remove(date_letter, ".*-")) %>% 
  mutate(name = str_remove(name, "-.*")) %>% 
  mutate(topic = str_remove(topic, "-.*")) %>% 
  mutate(date_letter2 = str_extract(Name, "[0-9]{1,2}\\.[0-9]{1,2}.[1-2][0,7-9]*")) %>%
  mutate(date_letter2 = ifelse(is.na(date_letter2),
                               str_extract(Name, "[0-9]{1,2}-[0-9]{1,2}-[1-2][0,7-9]*"),
                               date_letter2))  %>% 
  mutate(date_letter2 = ifelse(is.na(date_letter2),
                               str_extract(Name, "[0-9]{1,2} [0-9]{1,2} [1-2][0,7-9]*"),
                               date_letter2))  %>% 
  select(date_letter2, everything()) %>% 
  mutate(year = str_sub(Name, "201[7-9]")) %>% 
  mutate(year = ifelse(is.na(year), 
                       str_c("20", str_sub(date_letter2, -2)),
                       year)) %>% 
  mutate(date_letter2 = str_replace_all(date_letter2, "-| ", "\\.")) %>% 
  separate(date_letter2,
           into = c("month", "day"),
           sep = "\\.",
           remove = F) %>% 
  mutate(date_letter3 = str_extract(Name, "1[7-9][0-1][1-9][0-2][0-9]")) %>%
  mutate(date_letter3 = ifelse(is.na(date_letter3),
                               str_extract(Name, "1[7-9].[0-1]{0,1}[0-9].[0-2]{0,1}[0-9]"),
                               date_letter3)) %>%
  mutate(date_letter3 = ifelse(is.na(date_letter3),
                               str_extract(Name, "1[7-9] [0-1][0-9] [0-2][0-9]") %>% 
                                 str_remove_all("-"),
                               date_letter3)) %>%
  mutate(day = ifelse(is.na(day), str_sub(date_letter3, -2), day)) %>%
  mutate(month = ifelse(is.na(month), str_sub(date_letter3, 3,4), month)) %>%
  mutate(year = ifelse(is.na(year)|str_detect(year, " |.|-"), str_c(20, str_sub(date_letter3, 2)), year)) %>%
  select(date_letter, date_letter2, date_letter3,day, month, year, everything()) 

d2 %<>% 
  mutate(DATE = as.Date(str_c(year, month, day, sep = "-"), "%Y-%m-%d")) %>% 
  select(Name, DATE, everything()) %>% 
  arrange(!is.na(DATE))
  
unique(d2$year)

sum(is.na(d2$DATE))

full_join(d1,d2)
