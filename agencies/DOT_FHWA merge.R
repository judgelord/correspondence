data <- gs_title("DOT_FHWA Old") %>% gs_read()

data %<>% 
  mutate(FROM = paste(FName, LName, sep = " ")) %>%
           select(-FName, -LName)

data %<>% mutate(Sort = NA,
         X6 = NA, X7 = NA, X8 = NA, X9 = NA, X10 = NA, X11 = NA, X12 = NA, X13 = NA, X14 = NA, X15 = NA, X16 = NA, 
         X17 = NA, X18 = NA, X19 = NA, X20 = NA, X21 = NA, X22 = NA, X23 = NA, X24 = NA, X25 = NA, X26 = NA, 
         X27 = NA, X28 = NA, X29 = NA, X30 = NA, X31 = NA, X32 = NA, X33 = NA, X34 = NA, X35 = NA, X36 = NA, 
         X37 = NA, X38 = NA, X39 = NA, X40 = NA, X41 = NA, X42 = NA, X43 = NA, X44 = NA, X45 = NA, X46 = NA, 
         X47 = NA, X48 = NA, X49 = NA, X50 = NA, X51 = NA, X52 = NA, X53 = NA, X54 = NA, X55 = NA, X56 = NA)

data %<>% select(Sort, DATE, SUBJECT, ControlNumber, FROM, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16,
                 X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35,
                 X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54,
                 X55, X56, TYPE, CERTAINTY, ALT_TYPE, POLICY_EVENT, EVENT_NAME, EVENT_DATE, NOTES, ERROR, everything())
  
  write.csv(data, "DOT_FHWAold.csv")
  