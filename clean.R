


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











# calling agency-specific clean() function and joining data depending on status of hand-coding
clean.agency <- function(agency, status, coders) {
  source(paste0(agency, ".R"))
  
  if (status == "not coded") {
    data <- clean(agency)
  }
  
  if (status == "coded") {
    if (length(coders) == 1) {
      data <- clean(paste(agency, coders[1]))
    }
    if (length(coders) == 2) {
      data <- full_join(clean(paste(agency, coders[1])),
                        clean(paste(agency, coders[2])))
      
      # ######################FIXME ########################
      # TEMP REMOVED INTERCODER AGREEMENT METHOD FOR SPEED  #
      # print(intercoder.agreement(data))
    }
    #### extend if data get tripple coded ###
  }
  
  if (status == "recoded") {
    data <- full_join(clean(paste(agency, coders[1])),
                      clean(paste(agency, coders[2])))
    
    data <-
      full_join(gs_title(paste(agency, "Recoded")) %>% gs_read(), # if intercoder disagreement has been recoded
                data)
    print(intercoder.agreement(data))
  }
  
  # select one observation where coders disagree (disagreements are uploaded to drive in recode file)
  data %<>% group_by(ID, last_name) %<>% top_n(1, agency) %>% ungroup()
  
  # make consitant classes
  data %<>% mutate_at(names(data)[which(names(data) != "DATE")], as.character)

  
  data$agency <- agency # name agency
  data$department <- gsub("_.*", "", data$agency) # name dept
  
  if ("state" %in% names(data)) {
    data$state %<>% as.character()
  }
  
  
  # completing incomplete vars which will be used in merge
  for (i in 1:length(members$id)) {
    if (sum(c("first_name", "last_name", "state", "chamber", "congress") %in% names(data)) == 5) {
      data %<>%
        # incomplete first names
        mutate(
          first_name = ifelse(
            is.na(first_name) &
              !is.na(last_name) & !is.na(state) & !is.na(congress) & !is.na(chamber) &
              last_name == members$last_name[i] &
              state == members$state[i] &
              congress == members$congress[i] &
              chamber == members$chamber[i],
            members$first_name[i],
            first_name
          )
        ) %>%
        # incomplete chamber
        mutate(
          chamber = ifelse(
            is.na(chamber) &
              !is.na(last_name) & !is.na(state) & !is.na(congress) & !is.na(first_name) &
              last_name == members$last_name[i] &
              first_name == members$first_name[i] &
              congress == members$congress[i] &
              state == members$state[i],
            members$first_name[i],
            chamber
          )
        ) %>%
        # incomplete state
        mutate(
          state = ifelse(
            is.na(state) &
              !is.na(last_name) & !is.na(first_name) & !is.na(congress) & !is.na(chamber) &
              last_name == members$last_name[i] &
              first_name == members$first_name[i] &
              congress == members$congress[i] &
              chamber == members$chamber[i],
            members$state[i],
            state
          )
        )
    }
    
    if (sum(c("last_name", "first_name", "chamber", "congress") %in% names(data)) == 4) {
      data %<>%
        # incomplete first name 
        mutate(
          first_name = ifelse(
            is.na(first_name) &
              !is.na(last_name) & !is.na(congress) & !is.na(chamber) &
              last_name == members$last_name[i] &
              congress == members$congress[i] &
              chamber == members$chamber[i],
            members$first_name[i],
            first_name
          )
        ) %>%
      # if first name is common name
      mutate(
        first_name = ifelse(
          !is.na(last_name) & !is.na(congress) & !is.na(chamber) & !is.na(members$common_name[i]) & 
            last_name == members$last_name[i] &
            first_name == members$common_name[i] &
            congress == members$congress[i] &
            chamber == members$chamber[i],
          members$first_name[i],
          first_name
        )
      ) %>% 
      # if chamber is missing
        mutate(
          chamber = ifelse(
            is.na(chamber) &
              !is.na(first_name) & !is.na(last_name) & !is.na(congress) &
              last_name == members$last_name[i] &
              first_name == members$first_name[i] &
              congress == members$congress[i],
            members$chamber[i],
            chamber
          )
        )
    }
    
      if (sum(c("last_name", "first_name", "congress") %in% names(data)) == 3) {
        data %<>%
          # if first name is common name
          mutate(
            first_name = ifelse(
              !is.na(first_name) & !is.na(last_name) & !is.na(congress) & !is.na(members$common_name[i]) & 
                last_name == members$last_name[i] &
                first_name == members$common_name[i] &
                congress == members$congress[i],
              members$first_name[i],
              first_name
            )
          )
      }
  }
  
  
  return(data)
}
