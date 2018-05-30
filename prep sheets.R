# prep sheets for hand coding
source("setup.R")

sheets <- gs_ls()
sheets %<>% filter(author %in% c("justin.grimmer", "correspondenceresearch")) 
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





# EXTRA 


if (!("ID" %in% names(data))) {
  data$ID <- seq(1:dim(data)[1])
}

# gs_key(i) %>% gs_edit_cells(input = data$ID, trim = F, byrow = F) # save ID col (NEEDS TO BE FIXED TO APPEND AFTER LAST COL)

data %<>% select(
  ID,
  DATE,
  FROM,
  SUBJECT,
  TYPE,
  CERTAINTY,
  ALT_TYPE,
  POLICY_EVENT,
  EVENT_NAME,
  EVENT_DATE,
  NOTES,
  everything()
)

