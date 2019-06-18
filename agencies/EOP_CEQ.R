# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "EOP_CEQ" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct() # get data
  
  #Create ID  
  data %<>%
    mutate(ID = row_number())
  
  #create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc.

  data$DATE %<>% as.Date("%d-%b-%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  

#chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Sen|Sen\\.|Senator|Sen |Senate- ") & ! str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman"),
                            "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman") & ! str_detect(FROM, "\\(Sen|Sen\\.|Senator"),
                            "House", chamber))
  #Split
  data %<>%
    mutate(FROM = str_split(FROM, "\\,| and|\\/")) %>%
    unnest(FROM)
  
  data %<>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Sen|Sen\\.|Senator|Senate- |Senate Majority Leader ") & ! str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman"),
                            "Senate", NA)) %>%
    mutate(chamber = ifelse(str_detect(FROM, "\\(Cong| Cong$|Member of Congress|Congressman|House |Congresswoman |Rep\\. |Rep |SoH: |House- | - House|Representative |Congress of the United States") & ! str_detect(FROM, "\\(Sen|Sen\\.|Senator"),
                            "House", chamber))
  
data %<>%
  mutate(FROM = str_remove(FROM, "\\(Sen|Sen\\.|Senator|Senate- |Senate Majority Leader|\\(Cong| Cong$|Member of Congress|Congressman|House |Congresswoman |Rep\\. |Rep |SoH: |House-| - House|Representative |Congress of the United States|Hon. "))
 
data <- getFirstLast.Comma(data, col_name = "FROM")



data %<>% select(ID, DATE, FROM, everything())  

Unfoundnames <- data %>%
  filter(is.na(last_name)) %>%
  extractMemberName(members = members, col_name = "FROM")
  

return(data)  
}