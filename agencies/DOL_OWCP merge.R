library(dplyr)
library(magrittr)
library(readr)
d1 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 1)
d2 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 2)
d3 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 3)
d4 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 4)
d5 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 5)
d6 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 6)
d7 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 7)
d8 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 8)
d9 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 9)
d10 <- readxl::read_excel("DOL_OWCP.xlsx", sheet = 10)

d1 %<>% full_join(d2) %>% 
  full_join(d3) %>% 
  full_join(d4) %>% 
  full_join(d5) %>% 
  full_join(d6) %>% 
  full_join(d7) %>%   
  full_join(d8) %>%   
  full_join(d9) %>%   
  full_join(d10) 

write.csv(d1, "DOL_OWCP.csv")
