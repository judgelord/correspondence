# helper 
combine <- . %>% na.omit() %>% paste(collapse = " ")


sheet <- "IRS part 1.csv"

read_irs <- function(sheet){
  read_csv(str_c(here("irs/"), sheet), col_types = "ccccccccccccccccc") %>%
  unite("SUBJECT", everything(),  na.rm = TRUE, remove = FALSE) %>% 
    mutate(extra = SUBJECT) %>% 
  mutate(DATE = str_extract(SUBJECT, "[0-9]*/[0-9]*/[0-9]*") %>% str_remove(" .*")) %>% 
    select(DATE, everything()) %>%
  mutate(obs = str_detect(`Case Number`, "^20[0-1][0-9]-[0-9]") %>% replace_na(FALSE)) %>% 
  mutate(SUBJECT = ifelse(obs, Topic, SUBJECT)) %>%
  rownames_to_column("id") %>%
  mutate(id = ifelse(obs, paste(sheet, str_pad(id, 5, side = "left", pad = 0)), NA)) %>%
  select(-obs) %>%
  fill(id) %>% group_by(id) %>%
  summarise_all(combine)
}

d<-read_irs(sheet)
  
d <- list.files(here("irs")) %>% map_dfr(read_irs)

# align with sheet
d %<>% select(id, DATE,	SUBJECT,	`Case Number`, `Received Date`, `Due Date`, Correspondence,	`Case Type`,	`Status`,	`Members Due`, extra)

d %>% write.csv("irs.csv")


