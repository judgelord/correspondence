library(readxl)

USPS <- plyr::join_all(list(
  read_excel("2010-2013 Cases - UofWis.xlsx"),
  read_excel("2014 Cases - UofWis.xlsx"),
  read_excel("2015 Cases - UofWis.xlsx"),
  read_excel("2016 Cases - UofWis.xlsx"),
  read_excel("2017 Cases - UofWis.xlsx")
))

write.csv(USPS, "USPS.csv")
