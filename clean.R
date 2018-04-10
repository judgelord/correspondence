
intercoder.agreement <- function(data){
  # overall intercoder 
  intercoder <- data %>% filter(!is.na(TYPE)) %>% group_by(ID) %>% summarize(n = length(unique(TYPE))) %>% ungroup() %>% count(n)
  
  # intercoder when certian
  intercoder1 <- data %>% filter(!is.na(TYPE) & CERTAINTY == 1) %>% group_by(ID) %>% summarize(n = length(unique(TYPE))) %>% ungroup() %>% count(n)
  
  # upload file of cases to recode
  recode <- "recode.csv"
  filter(data, !is.na(TYPE)) %>% group_by(ID) %>% filter(length(unique(TYPE)) == 2) %>% arrange(ID) %>% select(agency, ID, everything()) %>%
  write.csv(recode) # saving file locally is faster
  drive_upload(recode,
               path = paste0("Correspondence/", agency," to Recode"), 
               type = "spreadsheet"
  )
  file.remove(recode) # remove local file
  
  return(paste("Intercoder agreement:", intercoder[2,2], "of", intercoder[1,2], "=", 1-round(intercoder[2,2]/intercoder[1,2], 2), 
               "and intercoder agreement for CERTIAN==1:", intercoder1[2,2], "of", intercoder1[1,2], "=", 1-round(intercoder1[2,2]/intercoder1[1,2], 2)
  ))
}

# calling agency-specific clean() function and joining data depending on status of hand-coding
clean.agency <- function(){
  source(paste0(agency, ".R"))
  
  if(status == "NA"){
    data <- clean(agency)
  }
  
  if(status == "coded"){
    data <- full_join(
      clean(paste0(coders[1], agency)),
      clean(paste0(coders[2], agency))
    ) 
    print(intercoder.agreement(data))
  }
  
  if(status == "recoded"){
    data <- full_join(
      clean(paste0(coders[1], agency)),
      clean(paste0(coders[2], agency))
    ) 
    
    data <- full_join(
      gs_title(paste(agency, "Recoded")) %>% gs_read(), # if intercoder disagreement has been recoded
      data
    ) 
    print(intercoder.agreement(data))
  }
  
  data %<>% group_by(ID) %<>% top_n(1, ID) %>% ungroup() # select on observation 
  data$agency <- agency # name agency
  return(data)
}
