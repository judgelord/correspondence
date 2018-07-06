# prep sheets for hand coding
source("setup.R")

sheets <- gs_ls()
sheets %<>% filter(author %in% c("correspondenceresearch")) 
sheets <- sheets$sheet_key

for (i in sheets) {
  data <- gs_key(i) %>% gs_read()
  
  variables <-
    c(
      "FROM",
      "DATE",
      "SUBJECT",
      "TYPE",
      "CERTAINTY",
      "ALT_TYPE",
      "POLICY_EVENT",
      "EVENT_NAME",
      "EVENT_DATE",
      "NOTES"
    )
  
  data[, variables[which(!(variables %in% names(data)))]] <- "" # create new empty variables
  
  gs_key(i) %>% gs_edit_cells(input = names(data), trim = F, byrow = T) # save 

  
} # end function





