

data <- data.frame(FROM = c("a", "b ; c ; d", "e ; f", "g"), ID = c(1,2,3, 4))


for(i in 1:nrow(data)){
  if(grepl(";", data$FROM[i])) {
    
    new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
    new$FROM <- unlist(str_split(data$FROM[i], ";"))
    
    data <- rbind(data, new)
    

  }
}


d <- full_join(
  read.csv("FOIA 2018-02-106 Enclosures 2008 Redacted (Powell).csv"),
  read.csv("FOIA 2018-02-106 Enclosures 2009 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2010 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2011 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2012 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2013 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2014 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2015 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2016 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2017 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2018 Redacted (Powell).csv")) %>%
  full_join(read.csv("FOIA 2018-02-106 Enclosures 2007 Redacted (Powell).csv"))

write.csv(d, "Treasury_Fiscal.csv")
s