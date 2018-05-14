# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "DOD_DLA_Aviation" # for testing


clean <- function(file.name) {
  # get data from google drive
  data <- gs_title(file.name) %>% gs_read()
  
  # create agency column
  data$agency <- file.name
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # Format date, year, Congress, member name etc.
  data$DATE %<>% as.Date("%m/%d/%y")
  data$date.closed %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE, 1, 4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1) / 2)) + 107) # the 107th congress began in 2001

  #Create variable for position title (Senator or Representative)
  data %<>%
    mutate(title = ifelse (grepl("Sen", FROM), "Senator", NA)) %>%
    mutate(title = ifelse(grepl("Rep|ep ", FROM), "Representative", title))
  
  
  #create variable for first name of the Sen/Rep
  data %<>%
    mutate(
      first_name = gsub(
        pattern = "(Rep|Rep.|-Rep|Sen|Senator) (\\w+).*",
        replacement = "\\2",
        x = FROM
      )
    ) %>%
    mutate(first_name = ifelse(first_name == "J", "J. Randy", first_name)) %>%
    mutate(first_name = ifelse(is.na(title), NA, first_name)) %>%
    mutate(first_name = ifelse(grepl("Bill Huizenga", FROM), "Bill", first_name))
  
  
  #create variable for last name of the Sen/Rep
  data %<>%
    mutate(
      last_name = gsub(
        pattern = "(Rep|Rep.|-Rep|Sen|Senator) (\\w+) (\\w+|.. \\w+|. \\w+).*",
        replacement = "\\3",
        x = FROM
      )
    ) %>%
    mutate(last_name = gsub(
      pattern = ".* (\\w+)",
      replacement = "\\1",
      x = last_name
    )) %>%
    mutate(last_name = ifelse(is.na(title), NA, last_name)) %>%
    mutate(last_name = ifelse(grepl("(Buck)", FROM), "McKeon", last_name)) %>%
    mutate(last_name = ifelse(grepl("Robert C", FROM), "Scott", last_name)) %>%
    mutate(last_name = gsub(
      pattern = "(\\w+) .*",
      replacement = "\\1",
      x = last_name
    )) %>% 
    mutate(first_name = ifelse(grepl("(|) ", first_name), NA, first_name))
  
  
  #specific correction
  data %<>%
    mutate(last_name = ifelse(ID == 31, NA, last_name)) %>%
    mutate(first_name = ifelse(ID == 31, NA, first_name)) %>%
    mutate(title = ifelse(ID == 31, NA, title))
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
} # end function 