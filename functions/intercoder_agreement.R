
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
  filter(data, !is.na(TYPE)) %>% group_by(ID) %>% filter(length(unique(TYPE)) == 2) %>% arrange(ID) %>% select(agency, ID, everything()) %>%
    write.csv(recode) # saving file locally is faster
  
  drive_rm(paste0("Correspondence/agencies/", agency, " to Recode")) # remove old recode file
  drive_upload(recode,
               path = paste0("Correspondence/agencies/", agency, " to Recode"),
               type = "spreadsheet")
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