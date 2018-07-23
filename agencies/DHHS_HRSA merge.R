library(readxl)

DHHS_HRSA <- read_excel("Final Exec Sec  1.2007 to 12.2007.xlsx") %>% 
  full_join(read_excel("Final Exec Sec  1.2008 to 12.2008.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec  1.2009 to 12.2009.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec  1.2010 to 12.2010.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec  1.2011 to 12.2011.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec 1.2012 to 12.2012.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec  1.2013 to 12.2013.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec 1.2014 to 12.2014.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec 1.2015 to 12.2015.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec 1.2016 to 12.2016.xlsx")) %>% 
  full_join(read_excel("Final Exec Sec 1.2017 to 12.2017.xlsx"))

write.csv(DHHS_HRSA, "DHHS_HRSA.csv")