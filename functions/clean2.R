# These functions call the agency-specific R script and prep them to be merged

# agency <- ""
# status <- "Not coded"
# coders <- NA

# calling agency-specific clean() function and joining data depending on status of hand-coding
clean.agency <- function(agency, status, coders) {
  source(paste0("agencies/",agency, ".R"))
  
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
  
  
  # select one observation where coders disagree (disagreements are uploaded to drive in recode file)
  data %<>% group_by(ID, last_name) %<>% top_n(1, agency) %>% ungroup()
  }
  
  # make consitant classes
  data %<>% mutate_at(names(data)[which(!names(data) %in% c("DATE", "congress"))], as.character)  
  data %<>% mutate_at(names(data)[which(names(data) %in% c("year", "congress"))], as.numeric)


  
  data$agency <- agency # name agency
  data$department <- gsub("_.*", "", data$agency) # name dept


  # completing incomplete vars which will be used in merge
  for (i in 1:length(members$id)) {
    if (sum(c("first_name", "last_name", "state", "chamber", "congress") %in% names(data)) == 5) {
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
      } #end if 
    } # end state loop
  return(data)
}#END MAIN LOOP 


