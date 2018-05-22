

# function will extract names found in members dataset from data$Summary column 

extractMemberName <- function(data, members){
  
  #create full name variables with different combinations of first, common, middle, middle initial, and last name
  members$first_last <- paste(members$first_name, members$last_name, sep = " ")
  members$common_last <- paste(members$common_name, members$last_name, sep = " ")
  members$first_middle_last <- paste(members$first_name, members$middle_name, members$last_name, sep = " ")
  members$first_initial_last <- paste(members$first_name, members$middle_initial, members$last_name, sep = " ")
  members$common_middle_last <- paste(members$common_name, members$middle_name, members$last_name, sep = " ")
  members$common_initial_last <- paste(members$common_name, members$middle_initial, members$last_name, sep = " ")
  
  # create FROM varible extracting name from data$Summary
  data$FROM <- gsub(pattern = paste(c('.*(', paste(members$common_last[1:850], collapse = '|'), ').*'), collapse = ""),
                    replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # extracts  first_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
   
     # first_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_middle_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
     
    # first_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$first_initial_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_middle_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_middle_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    
    # common_initial_last name formats
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1:850], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[850:1700], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[1700:2550], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) %>% 
    gsub(pattern = paste(c('.*(', paste(members$common_initial_last[2550:nrow(members)], collapse = '|'), ').*'), collapse = ""),
         replacement = "\\1", data$Summary, ignore.case = TRUE) 
  
  
  
  data$first_name <- gsub("^(\\w+) .*", replacement = "\\1", data$FROM)
  data$last_name <- gsub(".* (\\w+)$", replacement = '\\1', data$FROM)
  
  data %<>%
    formatFirstName() %>% 
    formatLastName()
  
  
  data %<>%
    mutate(first_name = ifelse(   grepl(paste(members$first_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
                                    grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE),
                                  
                                  first_name, NA) ) %>% 
    mutate(last_name = ifelse(
      grepl(paste(members$first_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$first_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_middle_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1:1500], collapse = '|'), data$Summary, ignore.case = TRUE)|
        grepl(paste(members$common_initial_last[1501:nrow(members)],collapse = "|"), data$Summary, ignore.case = TRUE), 
      last_name, NA)) %>% 
    mutate(FROM = ifelse( is.na(first_name) & is.na(last_name), NA, FROM))
  
  
  
  return(data)
}
