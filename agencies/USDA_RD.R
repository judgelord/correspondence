# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA_RD" # for testing

clean <- function(file.name) {
  # get data from google drive
  data <- gs_title(file.name) %>% gs_read()

  # LetterID = sheet row number
  data$LetterID <- 1:nrow(data)
  # select distinct observations 
  data_distinct <- data %>% select(-LetterID) %>% distinct()
  # join back in LetterID for distinct observations
  data <- data_distinct %>% left_join(data) %>% distinct()
  
  # create agency column
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%Y-%m-%d")
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001


  # apply extractmembername from legislators package 
  data %<>% extractMemberName(col_name = 'FROM', congress = "congress")
  
  # old ID still used in some places
  if(!"ID" %in% names(data)){
    data %<>% mutate(ID = data_id)
  }
  
  # arrange columns for hand coding
  data %<>% select(data_id, DATE,  FROM, everything())
  
  
  
  # Next, create variables custome to the agency
  # For example, in the USDA logs, they record the state of the property the letter is about:
  data$Property.State %<>% stateFromLower() # function saved in main directory as stateFromLower.R
  
  # In some cases, sparse SUBJECT fields can be consolidated
  # or new summary subject varialbes can be created with "regular expression" matching.
  # For example, with the grepl() function:
  unique(data$SUBJECT) # view SUBJECT strings
  
  # Consolidate and rename like SUBJECTs
  data %>%
    mutate(SUBJECT = ifelse (grepl("Credit", SUBJECT), "Credit", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (
      grepl("Foreclosure|foreclosure", SUBJECT),
      "Foreclosure",
      SUBJECT
    )) %>%
    mutate(SUBJECT = ifelse (grepl("Pay|pay|PAY", SUBJECT), "Payments", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Debt", SUBJECT), "Debt", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Delinquency", SUBJECT), "Delinquency", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Bankruptcy", SUBJECT), "Bankruptcy", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Insurance", SUBJECT), "Insurance", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Moratorium", SUBJECT), "Deferment", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Recapture", SUBJECT), "Recapture Account", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (
      grepl("General|Regular", SUBJECT),
      "General Service",
      SUBJECT
    )) %>% select(FROM, SUBJECT, Property.State)
  
  # More ways to consolidate
  data %>%
    mutate(SUBJECT = ifelse (grepl("REO", SUBJECT), "Real Estate Property", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Pay|pay|PAY|Loan", SUBJECT), "Loan Payments", SUBJECT)) %>%
    mutate(SUBJECT = ifelse (grepl("Tax", SUBJECT), "Taxes", SUBJECT))
  
  
  # not run unless subsetting:
  # select only the common SUBJECTS, useful for plotting
  # major.subjexts <- c("Credit", "Taxes", "Real Estate Property", "Foreclosure","Debt", "Bankruptcy", "Insurance", "Loan Payments", "Delinquency")
  
  # even more limited sample, forcused on economic hardship
  # major.subjects <- c( "Foreclosure",  "Delinquency")
  # data %<>% filter(SUBJECT %in% major.subjects)
  
  # Assign TYPE In most cases, human coders will have done this already.
  
  
  data %<>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("Payment|Delinquency|Insurance|Servicing|Foreclosure|Debt Settlement|Escrow|Recapture Receivable Account|Payoff|Debt|Deferment|RELEASE OF LIABILITY|GENERAL SERVICE|CREDIT|REAMORTIZATION|BANKRUPTCY|Service Complaint|UNA|Unauthroized Assistance|RECAPTURE|MAINTENANCE|Compliance|Refinance|FUNDS", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("Payment|Delinquency|Insurance|Servicing|Foreclosure|Debt Settlement|Escrow|Recapture Receivable Account|Payoff|Debt|Deferment|Release of LIability|GENERAL SERVICE|CREDIT|REAMORTIZATION|BANKRUPTCY|Service Complaint|UNA|Unauthorized Assistance|RECAPTURE|MAINTENANCE|Compliance|Refinance|FUNDS", SUBJECT, ignore.case = TRUE), "1", CERTAINTY)) %>%  
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("TOP", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("TOP", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("REAL ESTATE PROPERTY", SUBJECT, ignore.case = TRUE), "1", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("REAL ESTATE PROPERTY", SUBJECT, ignore.case = TRUE), "3", CERTAINTY)) %>%
    mutate(ALT.TYPE = ifelse (!grepl("[0-9]", ALT.TYPE) & grepl("REAL ESTATE PROPERTY", SUBJECT, ignore.case = TRUE), "2", ALT.TYPE)) %>%
    mutate(TYPE = ifelse (!grepl("[0-9]", TYPE) & grepl("MISDIRECTED CONGRESSIONAL|Misdirected", SUBJECT, ignore.case = TRUE), "6", TYPE)) %>%
    mutate(CERTAINTY = ifelse (!grepl("[0-9]", CERTAINTY) & grepl("MISDIRECTED CONGRESSIONAL|Misdirected", SUBJECT, ignore.case = TRUE), "1", CERTAINTY))
    
    
    
    
    
    
    # then make it TYPE 2, otherwise keep TYPE as is
  
  # Notice how the odd spaces are not needed to return a match
  # Also notice how "Payment Assistance" is matched with just "Payment"
  
  unique(data$SUBJECT[which(is.na(data$TYPE))]) # see SUBJECTs yet uncoded
  
  cbind(data$SUBJECT, data$TYPE) # see resulting auto-coded TYPE:
  
  
  
  # Errors for letters from non members of congress # May not be working
  data %<>%
    mutate(ERROR = ifelse(grepl("^White House Referral$",data$FROM), "White House Referral", ERROR)) %>% 
    mutate(ERROR = ifelse(grepl("^National Office Referral$",data$FROM), "National Office Referral", ERROR))
  
  # #Failing observations
  # Unfoundnames <- data %>%
  #   filter(is.na(last_name),
  #          is.na(ERROR)) 
  # Unfoundnames %>% select(congress, FROM, string) %>% distinct()%>% kable()
  
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  return(data)
}