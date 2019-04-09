# prep sheets for hand coding by adding columns 
source("setup.R")

sheets <- gs_ls()
unique(sheets$author) ## Remind Justin to transfer ownership of U Chi coders' sheets
sheets %<>% filter(author %in% c("correspondenceresearch", "justin.grimmer")) 
sheets <- sheets$sheet_key

for (i in sheets) {
  data <- gs_key(i) %>% gs_read()
  
  ## List columns we want in each sheet
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
      "NOTES",
      "ERROR"
    )
  
  data[, variables[which(!(variables %in% names(data)))]] <- "" # create new empty variables
  
  gs_key(i) %>% gs_edit_cells(input = names(data), trim = F, byrow = T) # save 

  
} # end function





