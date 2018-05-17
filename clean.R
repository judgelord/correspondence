

intercoder.agreement <- function(data) {
  # overall intercoder
  intercoder <-
    data %>% filter(!is.na(TYPE)) %>% group_by(ID) %>% summarize(n = length(unique(TYPE))) %>% ungroup() %>% count(n)
  
  # intercoder when certian
  intercoder1 <-
    data %>% filter(!is.na(TYPE) &
                      CERTAINTY == 1) %>% group_by(ID) %>% summarize(n = length(unique(TYPE))) %>% ungroup() %>% count(n)
  
  # upload file of cases to recode
  recode <- "recode.csv"
  filter(data,!is.na(TYPE)) %>% group_by(ID) %>% filter(length(unique(TYPE)) == 2) %>% arrange(ID) %>% select(agency, ID, everything()) %>%
    write.csv(recode) # saving file locally is faster
  drive_rm(paste0("Correspondence/", agency, " to Recode")) # remove old recode file
  drive_upload(recode, path = paste0("Correspondence/", agency, " to Recode"), type = "spreadsheet")
  file.remove(recode) # remove local file
  
  return(
    paste(
      "Intercoder agreement:",
      intercoder[2, 2],
      "of",
      intercoder[1, 2],
      "=",
      1 - round(intercoder[2, 2] / intercoder[1, 2], 2),
      "and intercoder agreement for CERTIAN==1:",
      intercoder1[2, 2],
      "of",
      intercoder1[1, 2],
      "=",
      1 - round(intercoder1[2, 2] / intercoder1[1, 2], 2)
    )
  )
}

# calling agency-specific clean() function and joining data depending on status of hand-coding
clean.agency <- function() {
  source(paste0(agency, ".R"))
  
  if (status == "not coded") {
    data <- clean(agency)
  }
  
  if (status == "coded") {
    if (length(coders) == 1) {
      data <- clean(paste(agency, coders[1]))
    }
    if(length(coders) == 2) {
      data <- full_join(clean(paste(agency, coders[1])),
                        clean(paste(agency, coders[2])))
      print(intercoder.agreement(data))
    }
  }
  
  if (status == "recoded") {
    data <- full_join(clean(paste(agency, coders[1])),
                      clean(paste(agency, coders[2])))
    
    data <- full_join(gs_title(paste(agency, "Recoded")) %>% gs_read(), # if intercoder disagreement has been recoded
                      data)
    print(intercoder.agreement(data))
  }
  
  data %<>% group_by(ID, last_name) %<>% top_n(1, agency) %>% ungroup() # select on observation
  data$agency <- agency # name agency
  
 # things to match on
  for(i in 1:length(members$id)) {
    if(sum(c("last_name", "first_name", "chamber", "congress") %in% names(data)) == 4){
    data %<>% 
      mutate(first_name = ifelse(last_name == members$last_name[i] &
                                   first_name == members$common_name[i] & 
                                   congress == members$congress[i] & 
                                   chamber == members$chamber[i],
                                 members$first_name[i], first_name))
    }
  }
  
  # problem vars
  data$ALT_TYPE <-NA
  return(data)
}

