# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# 178 mismatches on last name still

#file.name <- "NLRB" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create agency column
  data$agency <- file.name
  
  # remove unwanted rows
  data <- data[-which((is.na(data$FROM)&is.na(data$SUBJECT)&is.na(data$DATE))|
                        data$FROM == "Requestor (Last Name, First Name)"|data$FROM == "Signatories"),]
  data <- data[-which(grepl("Congressional Log|Office of the General", data$DATE)), ]
  data$ID <- seq(1:nrow(data))
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";", data$FROM[i])) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";", data$FROM),] # removes orginal row with all data
  data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  ################
  
 
  # Format date, year, Congress, member name etc. 
  data$DATE1 <- ifelse( grepl("/\\w{4}$",data$DATE), data$DATE, NA  )
  data$DATE1 %<>% as.Date("%m/%d/%Y")
  data$DATE2 <- ifelse( grepl("/\\w{2}$",data$DATE), data$DATE, NA  )
  data$DATE2 %<>% as.Date("%m/%d/%y")
  data$DATE <- data$DATE1
  data$DATE <- dplyr::if_else(is.na(data$DATE), data$DATE2, data$DATE)
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  
  
  data <- extractMemberName(data, members, 'FROM')

  
  # chamber
  data %<>%
    mutate(chamber = ifelse (grepl("(^S(-| ))|Senator|Sen\\.", FROM), "Senate", NA)) %>% 
    mutate(chamber = ifelse(grepl("(^(R|C)(-| ))|Repres|Congress|Rep", FROM), "House", chamber)) 
  
  
  
  
  
  # arrange columns for hand coding
  data %<>% select(ID, DATE, FROM, SUBJECT, everything())
  
  
}

