# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

# FIXME - NEEDS TO HAVE MULTI-MEMBER LINES BROKEN OUT 

#file.name <- "DOC_EDA" # for testing

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
  
  
  
  # FIX DUPLICATES
  
  # create separate dataset for observations with multiple members but not easily formatted with puncuation
  data2 <- data[grepl("\\w{2}) ",data$FROM),]
  # remove these from original dataset
  data <- data[!grepl("\\w{2}) ",data$FROM),]
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data2)){
    if(grepl("\\w{2}) ", data2$FROM[i], ignore.case = T)) {
      
      new <- data2 %>% dplyr::slice(rep(i, each = str_count(data2$FROM[i], pattern = "\\w{2}\\) ") + 1))
      new$FROM <- unlist(str_split(data2$FROM[i], "\\w{2}\\) "))
      
      data2 <- rbind(data2, new)
      
    }
  }
  data2 <- data2[-grep("\\w{2}) ", data2$FROM, ignore.case = T),] # removes orginal row with all data
  ########
  
  data2 <- extractMemberName(data2,members, "FROM")
  
  ###############    
  # Creates duplicate rows for lines with multiple representatives
  for(i in 1:nrow(data)){
    if(grepl(";| and |,|/", data$FROM[i], ignore.case = T)) {
      
      new <- data %>% dplyr::slice(rep(i, each = str_count(data$FROM[i], pattern = ";| and |,| AND |/") + 1))
      new$FROM <- unlist(str_split(data$FROM[i], ";| and |,| AND |/"))
      
      data <- rbind(data, new)
      
    }
  }
  data <- data[-grep(";| and |,|/", data$FROM, ignore.case = T),] # removes orginal row with all data
  ########
  
  # extract member names
  data %<>% extractMemberName(members, "FROM")
  
  
  

  
  
  
  
  
}
