# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# FIXME - NEEDS TO HAVE MULTI-MEMBER LINES BROKEN OUT 

# file.name <- "DOC_EDA" # for testing

clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() # get data
  
  # create ID variable
  data$ID <- c(1:nrow(data))
  
  # create agency column
  data$agency <- file.name
  
  # Format date, year, Congress, member name etc. 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  # paste all relevent info into subject col
  data %<>% 
    mutate(SUBJECT = paste(SUBJECT)) 
  
  # chamber 
  data %<>% 
    mutate(chamber = ifelse(grepl("Senator|SENATOR|MAJORITY LEADER", FROM), "Senate", NA)) %>%
    mutate(chamber = ifelse(grepl("Representative|REPRESENTATIVE|REPRESENTATIAVE|^REP ", FROM), "House", chamber))
  
  data$FROM <- gsub("Senator|SENATOR|Representative|REPRESENTATIVE|REPRESENTATIAVE|MAJORITY LEADER|^REP ","", data$FROM)
  data$FROM <- gsub("^ ","", data$FROM)
  
  # state
  # data$state <- gsub(".*\\(|.*\\[","", data$FROM)
  # data$state <- gsub("\\).*|\\}.*|\\].*|-.*","", data$state)
  # data$state <- gsub(" ","", data$state)
  
  data$FROM <- gsub(pattern = ", Jr.| Jr.| Jr|, Jr|, III| III| II|, II| Ii|, IV| IV| ll| Jr,|, PhD", "", data$FROM, ignore.case = T)
  data$FROM <- gsub(pattern = ", Jr.,|, Jr. ,|, II ,|, CPA,|, M.D.|, M.D.,|, MD,|, M.C.,|, III,|, P.E.,|, P.E.| Ii,| \\(Il\\), Rep.",
                     replacement = ",", data$FROM, ignore.case = T)
  
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";| and |,", data$FROM[i], ignore.case = T)) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";| and |,|AND") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";| and |,|AND"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";| and |,", data$FROM, ignore.case = T),] # removes orginal row with all data
  # data$FROM <- gsub("^ |^  | $|  $", "", data$FROM)
  data <- data[!data$FROM == "",] # removes blank observations
  ########
  
  # extract member names
  data %<>% extractMemberName(members, "FROM")
  
  
  

  
  
  
  
  
}
