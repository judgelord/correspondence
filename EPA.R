library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)


#read in file and create agency column
file.name <- "Adam EPA - EPA-DJL.csv" 
data <- read.csv(file.name, stringsAsFactors = FALSE)
data$agency <- "EPA"



# Format date, year, Congress, member name etc. (things in all logs)

data$DATE %<>% as.Date("%d-%b-%y")
data$Received %<>% as.Date('%d-%b-%y')

#create year and congress variable
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


#create variable for last name of Sen/Rep
data %<>%
  mutate(last_name = gsub(pattern = "(.*),.*", replacement="\\1", x=FROM))

#create variable for first name of Sen/Rep
data %<>%
  mutate(first_name =  gsub(pattern = "(.*), (\\w+).*", replacement = "\\2", x=FROM))


#Create variable for position title (Senator or Representative)
data %<>%
  mutate(title = ifelse (grepl("Senate|SENATE", FROM), "Senator", NA)) %>% 
  mutate(title = ifelse(grepl("Representative|REPRESENTATIVE|Repesentatives", FROM), "Representative", title))  


#create party variable
data %<>%
  mutate(party = ifelse (grepl("\\w-D", FROM), "D", "FIXME")) %>% 
  mutate(party = ifelse (grepl("\\w-R", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("\\w-I", FROM), "I", party)) %>% 
  mutate(party = ifelse (grepl("BUCSHON, LARRY", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("AYOTTE, KELLY A", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("FINCHER, STEPHEN", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("ESTY, ELIZABETH", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("HARE, PHIL", FROM), "D", party)) %>%
  mutate(party = ifelse (grepl("PETERSON, JOHN", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("CRUZ, TED", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("HIRONO, MAZIE", FROM), "D", party)) %>%
  mutate(party = ifelse (grepl("HULTGREN, RANDY", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("VELAZQUEZ, NYDIA", FROM), "D", party)) %>%
  mutate(party = ifelse (grepl("GRAVES, TOM", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("BARRASSO, JOHN", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("FRANKEN, AL", FROM), "D", party)) %>%
  mutate(party = ifelse (grepl("BRALEY, BRUCE", FROM), "D", party)) %>%
  mutate(party = ifelse (grepl("BENISHEK, DAN", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("GIBBS, BOB", FROM), "R", party)) %>%
  mutate(party = ifelse (grepl("PERRY, SCOTT", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("SCHMIDT, JEAN", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("LIEBERMAN, JOSEPH", FROM), "I", party)) %>% 
  mutate(party = ifelse (grepl("HARTZLER, VICKY", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("SCHAKOWSKY, JAN", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("RUBIO, MARCO", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("NEWHOUSE, DAN", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("OWENS, BILL", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("MENG, GRACE", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("BRIDENSTINE, JIM", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("DONNELLY, JOE", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("KENNEDY, EDWARD", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("MATSUI, DORIS", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("WARREN, ELIZABETH", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("TONKO, PAUL", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("JOHNSON, BILL", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("PORTMAN, ROB ", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("MERKLEY, JEFF", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("WEBSTER, DANIEL", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("POMPEO, MIKE", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("VELA, FILEMON", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("HARRIS, ANDY", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("MIKULSKI, BARBARA", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("SWALWELL, ERIC", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("BILBRAY, BRAIN", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("DENHAM, JEFF", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("PITTS, JOSEPH", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("ROSKAM, PETER ", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("GRIFFITH, H. MORGAN", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("CRAVAACK, CHIP", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("MURPHY, SCOTT", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("BROUN, PAUL ", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("KAINE, TIM", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("GUINTA, FRANK", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("KLEIN, RON", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("GARDNER, CORY", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("GRIMM, MICHAEL", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("CRAWFORD, RICK", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("CHAFFETZ, JASON", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("QUIGLEY, MIKE", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("PAUL, RAND-", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("DELANEY, JOHN", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("MCKINLEY, DAVID", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("OLVER, JOHN", FROM), "D", party)) %>%   
  mutate(party = ifelse (grepl("JOHNSON, RON", FROM), "R", party)) %>%   
  mutate(party = ifelse (grepl("BERG, RICK", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("DESJARLAIS, SCOTT", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("JOLLY, DAVID", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("HOEVEN, JOHN", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("BARLETTA, LOU", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("HECK, DENNY", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("FLEISCHMANN, CHUCK", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("RIBBLE, REID", FROM), "R", party)) %>% 
  mutate(party = ifelse (grepl("PRICE, DAVID", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("CHU, JUDY", FROM), "D", party)) %>% 
  mutate(party = ifelse (grepl("FOSTER, BILL", FROM), "D", party)) 
  
  
#create STATE variable 
data %<>%
  mutate(state = gsub(pattern = ".*Senate-..(\\w{+})/DC.*", replacement = "\\1", x=FROM)) %>% 
  mutate(state = gsub(pattern = ".*House of Represent.*-..(\\w{+})/...*", 
                      replacement = "\\1", x = state)) 
data$state %<>% stateFromLower()

data %<>%
  mutate(state =  ifelse(grepl(pattern = "\\W+", x = state), NA, state))

#sum(grepl(pattern = "\\W+", x =data$state))
#sum(is.na(data$state))



#rearrange columns to the front
data %<>% select(X, FROM, first_name, last_name, title, party, everything())


#write.csv(data, paste("new", file.name)) # save as new file