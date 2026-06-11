# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "USDA_FS" # for testing

clean <- function(file.name){
  
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
  
  data %<>%
    mutate(DATE = coalesce(DATE, DateReceived) )
  #            str_replace("/13", "/2013")|> 
  #            # str_replace("/14", "/2014") |> 
  #            # str_replace("/15", "/2015") |> 
  #            # str_replace("/16", "/2016") |> 
  #            # str_replace("/17", "/2017") |> 
  #            # str_replace("/18", "/2018") |> 
  #            # str_replace("/19", "/2019") |> 
  #            # #str_replace("/20", "/2020") |> 
  #            # str_replace("/21", "/2021") |> 
  #            # str_replace("/22", "/2022") |> 
  #            # str_replace("/23", "/2023")  |> 
  #            # str_replace("/24", "/2024")  |> 
  #            # str_replace("/25", "/2025")  |> 
  #            # str_replace("/26", "/2026")  |> 
  #            str_replace("/27", "/2027")  )
  data$DATE |> tail()
  
  # First, format date, year, Congress, member name etc. (things found in all logs)
  data$DATE %<>% as.Date("%m/%d/%Y")
  data$DATE |> tail(100)
  
  data %<>% mutate(year = as.integer(substr(DATE,1,4)))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  #checking for NA dates
  NOdate <- data %>%
    filter(is.na(DATE))
  
  
  data$FROM <- (gsub("+","",data$FROM)) # remove +
  data <- data[-which(is.na(data$FROM)&is.na(data$DATE)&is.na(data$Addressee)),]
  
  # create first and last name variables
  data %<>% extractMemberName(col_name = 'FROM', members = members, congress = "congress")
  
  # arrange columns for hand coding
  data %<>% select(data_id, DATE,  FROM, everything())
  
  
  ### Nearly 1500 names where extractMemberName() didn't match anything. These names were scanned through
  # and none of the names were recognizable as congressman. Possible there were a handful of lesser known
  # representatives excluded, but I think it's unlikely. 
  data %<>%
    mutate(ERROR = ifelse(is.na(last_name), "Probably not in congress. Worth checking again.", ERROR))

  return(data)
}





