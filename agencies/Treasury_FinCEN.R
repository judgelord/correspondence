# This script defines a function clean() for google sheets of correspondence logs that may have been hand coded
# It may also auto-code variables like TYPE based on agency-specific information

#file.name <- "Treasury_FinCEN" # for testing


clean <- function(file.name) {
  data <- gs_title(file.name) %>% gs_read() %>% distinct()# get data
  
  #create agency column
  data$agency <- file.name
  
  #Format date
  data %<>%
    mutate(DATE = str_replace(DATE, "200", "0")) %>%
    mutate(DATE = str_replace(DATE, "201", "1"))
  
  data$tempDATE<- data$DATE %>% as.Date("%m/%d/%y")
  data %<>%
    mutate(DATE = ifelse(is.na(tempDATE), `Due Date`, DATE))
  
  data %<>%
    mutate(DATE = str_remove(DATE, "Orig. "))
  
  data$DATE <- gsub("/201", "/1", data$DATE) 
  data$DATE <- gsub("/200", "/0", data$DATE)
 
  data$DATE %<>% as.Date("%m/%d/%y")
  
  data %<>%
    mutate(DATE = str_replace(DATE, "2030-03-04", "2015-03-04"))
  
 
  
  #Check for DATE NAs
  NoDATE <- data %>%
    filter(is.na(DATE))
    

  #create year and congress columns
  data %<>% mutate(year = as.numeric(substring(DATE,1,4) ))
  data %<>% mutate(congress = as.numeric(round((year - 2001.1)/2)) + 107) # the 107th congress began in 2001
  
  
  data %>%
    filter(Sort == 91) %>%
    select(Summary, DATE, congress)
  
  data %>%
    filter(Sort == 111) %>%
    select(Summary, DATE, congress)
  
  #Group duplicate data
  data %<>%
    group_by(Summary, DATE, congress) %>%
    mutate(n = n(),
           `Task No's` = str_c(`Task No.`, collapse = "; "),
           Sorts = str_c(Sort, collapse = "; ")) %>%
    arrange(-n) %>%
    select(-`Task No.`, -Sort) %>%
    ungroup() %>%
    distinct()
  
  #Create LetterID
  data %<>%
    mutate(LetterID = row_number())
  
  
  #Filter out headings
  data %<>%
    filter( ! Summary == "Summary")
  
  #Remove "Congressional Corespondence" to correctly match chamber
  data %<>%
    mutate(Summary = str_remove(Summary, "CONGRESSIONAL CORRESPONDENCE:"))
  
  data %<>%
    mutate(str_replace(Summary, "CONGRESSIONAL: HOMELAND SECURITY & GOVERNMENTAL AFFAIRS COMMITTEE-Feb. 16 Letter from Lieberman / Collins to Secretary Geithner re FinCEN's SV Rule AWAITI", "CONGRESSIONAL: HOMELAND SECURITY & GOVERNMENTAL AFFAIRS COMMITTEE-Feb. 16 Letter from Lieberman , Collins to Secretary Geithner re FinCEN's SV Rule AWAITI"))
  

  #String split for multiple members
  data %<>%
    mutate(Summary = str_split(Summary, ",| and ")) %>%
    unnest(Summary)
  
  #chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(Summary, "Congressman|Rep.|Con. |con. |cong |congs |cong. |rep| congressman  | Congresswoman |House |house |CONGRESSMAN |Congressmen |Representative|Representative "), "House", NA)) %>%
    mutate(chamber = ifelse(str_detect(Summary, "Sen |Sen.|Senators"), "Senate", chamber))
  
  #ID
  data %<>%
    mutate(ID = row_number())
  
  #Recode to match chamber_last
  data %<>%
    mutate(Summary = str_replace_all(Summary, "Congressman|Rep.|rep |cong | congressman |Cong. |Congresswoman|CONGRESSMAN |Congressmen |Representative", "Representative ")) %>%
    mutate(Summary = str_replace_all(Summary, "Senators|senators", "Senator")) %>%
    mutate(Summary = str_replace(Summary, "Chairmen Shelby", "Senator Shelby")) %>%
    mutate(Summary = str_replace(Summary, "Chairman Frank", "Representative Frank")) %>%
    mutate(Summary = str_replace(Summary, "Chairman Baucus", "Senator Baucus")) %>%
    mutate(Summary = str_replace(Summary, "Chairman Issa", "Representative Issa")) %>%
    mutate(Summary = str_replace(Summary, "Chairman Hensarling", "Representative Hensarling")) %>%
    mutate(Summary = ifelse(str_detect(chamber, "Senate") & str_detect(Summary, "Levin"), str_replace(Summary, "Levin", "Senator Levin"), Summary)) %>%
    mutate(Summary = str_replace(Summary, "Grassley", "Senator Grassley")) %>%
    mutate(Summary = str_replace(Summary, "Whitfield", "Representative Whitfield")) %>%
    mutate(Summary = str_replace(Summary, " Mack", "MACK, Connie")) %>%
    mutate(Summary = str_replace(Summary, "Sherman", "Representative Sherman")) %>%
    mutate(Summary = str_replace(Summary, "Feinstein", "Senator Feinstein")) %>%
    mutate(Summary = str_replace(Summary, " Kyi", " Kyl")) %>%
    mutate(Summary = str_replace(Summary, "Renee L. Ellmers", "Renee Ellmers")) %>%
    mutate(Summary = str_replace(Summary, "Senator Waxman", "Henry WAXMAN")) %>%
    mutate(Summary = str_replace(Summary, "McCaskill ", "Claire McCASKILL")) %>%
    mutate(Summary = str_replace(Summary, "Whitehouse", "Sheldon WHITEHOUSE")) %>%
    mutate(Summary = str_replace(Summary, "Lieberman", "Joseph LIEBERMAN")) %>%
    mutate(Summary = ifelse(str_detect(Summary, "Collins") & congress == 111, str_replace(Summary, "Collins", "Susan COLLINS"), Summary)) %>%
    mutate(Summary = str_replace(Summary, "Waters", "WATERS, Maxine")) %>%
    mutate(Summary = str_replace(Summary, "Cohen", "Stephen COHEN")) %>%
    mutate(Summary = str_replace(Summary, "Coburn", "Thomas COBURN"))
  
  #Fix chamber
  data %<>%
    mutate(chamber = ifelse(str_detect(Summary, "Henry WAXMAN"), str_replace(chamber, "Senate", "House"), chamber))
  
  #Trim White Space
  data %<>%
    mutate(Summary = str_trim(Summary))
  
  #Paste Chamber into FROM
  data %<>%
    mutate(Summary =ifelse(str_detect(chamber, "House") & ! str_detect(Summary, " "), paste("Representative", Summary, sep = " "), Summary)) %>%
    mutate(Summary = ifelse(str_detect(chamber, "Senate") & ! str_detect(Summary, " "), paste("Senator", Summary, sep = " "), Summary))


  #Extract Member names
  data %<>%
    extractMemberName2(members = members, col_name = "Summary")
  
  data %<>% select(ID, DATE, LetterID, chamber, congress, Summary, first_name, last_name, everything())
  
 
  #Check for duplicates
  sample2data<- data
  
  sample2data %<>%
    group_by(ID, Summary, DATE) %>%
    mutate(n = n(),
           last_names = str_c(last_name, collapse = "; ")) %>%
    arrange(-n) %>%
    select(-last_name) %>%
    ungroup() %>%
    distinct()
  
  
  #FOIA
  data %<>%
    mutate(NOTES = ifelse(str_detect(Summary, "Letter from Representative  Kennedy") & congress == 109, "FOIA Multiple Representative Kennedys", NOTES)) 
  
  data %<>%
    mutate(ERROR = ifelse(str_detect(Summary, "Representative Dingell") & is.na(last_name), "Wrong Dingell, Duplicate", ERROR))
  
  #Add first name 
  data %<>%
    mutate(first_name = ifelse(is.na(first_name) & ! is.na(last_name) & is.na(chamber), addFirst(first_name, last_name), first_name))
  
 
  Unfoundnames <- data %>%
    filter(is.na(last_name))
  
  Unfoundnames %<>%
    filter( ! str_detect(pattern, "404error"))
 
  data %>%
    filter(ID == 424) %>%
    select(Summary)
  

  ##testing code
  
  
   # sample <- data %>%
   # filter(is.na(last_name))  
   # View(sample) 
  
  
  
  return(data)
  
}