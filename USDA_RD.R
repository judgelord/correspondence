library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)

# Much of this file will need to be customized for each agency
file.name <- "USDA_RD-DJL.csv" 
data <- read.csv(file.name)
data$agency <- "USDA RD"

# 1 GENERIC VARIABLES
# Format date, year, Congress, member name etc. (things in all logs)
# Dates may be in different formats.
data$DATE %<>% as.Date("%m/%d/%y")
data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001


# 2 AGENCY-SPECIFIC VARIABLES
# create new variables custome to the agency
# For example, in the USDA logs, they record the state of the property the letter is about:
data %<>% mutate(state = Property.State)
data$state %<>% stateFromLower() # function saved in main directory as stateFromLower.R


# In some cases, sparse SUBJECT fields can be consolidated
# or new summary subject varialbes can be created with "regular expression" matching. 
# For example, with the grepl() function:
?regex
?grepl
unique(data$SUBJECT) # view SUBJECT strings

# Consolidate and rename like subjects
data %<>%
  mutate(SUBJECT = ifelse (grepl("Credit", SUBJECT), "Credit", SUBJECT)) %>% 
  mutate(SUBJECT = ifelse (grepl("Foreclosure|foreclosure", SUBJECT), "Foreclosure", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Pay|pay|PAY", SUBJECT), "Payments", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Debt", SUBJECT), "Debt", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Delinquency", SUBJECT), "Delinquency", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Bankruptcy", SUBJECT), "Bankruptcy", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Insurance", SUBJECT), "Insurance", SUBJECT)) %>% 
  mutate(SUBJECT = ifelse (grepl("Moratorium", SUBJECT), "Deferment", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Recapture", SUBJECT), "Recapture Account", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("General|Regular", SUBJECT), "General Service", SUBJECT)) 

# More ways to consolidate
data %<>% 
  mutate(SUBJECT = ifelse (grepl("REO", SUBJECT), "Real Estate Property", SUBJECT)) %>% 
  mutate(SUBJECT = ifelse (grepl("Pay|pay|PAY|Loan", SUBJECT), "Loan Payments", SUBJECT)) %>%
  mutate(SUBJECT = ifelse (grepl("Tax", SUBJECT), "Taxes", SUBJECT)) 

# select only the common SUBJECTS, useful for plotting 
major.subjexts <- c("Credit", "Taxes",
                    "Real Estate Property", 
                    "Foreclosure","Debt", "Bankruptcy",
                    "Insurance", "Loan Payments", "Delinquency")
# even more limited sample, forcused on economic hardship
 major.subjects <- c( "Foreclosure",  "Delinquency")
 
# not run unless subsetting
#  data %<>% filter(SUBJECT %in% major.subjects)
 
 
# Assign type. In most cases, human coders will have done this already.
data %<>% mutate(TYPE =
                    ifelse(grepl(
                      # i.e. if SUBJECT contains:
                      # (& means "AND",  | means "OR")
                      "Payment|Delinquency|Insurance|Servicing|Foreclosure|Debt Settlement|Escrow|Recapture Receivable Account|Payoff", 
                      SUBJECT), 
                      2, TYPE))  # then make it TYPE 2, otherwise keep TYPE as is
 
# Notice how the odd spaces are not needed to return a match
# Also notice how "Payment Assistance" is matched with just "Payment"
 
unique(data$SUBJECT[which(is.na(data$TYPE))]) # see SUBJECTs yet uncoded

cbind(data$SUBJECT, data$TYPE) # see resulting auto-coded TYPE:

write.csv(data, paste("new", file.name)) # save as new file
