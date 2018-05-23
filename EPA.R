# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EPA Adam" # for testing

clean <- function(file.name) {
  # get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  # create agency column
  data$agency <- file.name
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%d-%b-%y")
  data$Received %<>% as.Date('%d-%b-%y')
  
  # create year and congress variables
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001
  
  # create first and last name variables
  data <- getFirstLast.Comma(data, 'FROM')
  
  
  # create variable for chamber position  (Senator or Representative)
  data %<>%
    mutate(chamber = ifelse (grepl("Senate|SENATE", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(
      grepl("Representative|REPRESENTATIVE|Repesentatives", FROM),
      "House",
      chamber
    )) 
  
  # create state variable (if given)
  data %<>%
    mutate(state = gsub(
      pattern = ".*Senate-..(\\w{+})/DC.*",
      replacement = "\\1",
      x = FROM
    )) %>%
    mutate(
      state = gsub(
        pattern = ".*House of Represent.*-..(\\w{+})/...*",
        replacement = "\\1",
        x = state
      )
    )
  data %<>%
    mutate(state =  ifelse(grepl(pattern = "\\W+", x = state), NA, state))
  
  data$state %<>% stateFromLower()
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
}
