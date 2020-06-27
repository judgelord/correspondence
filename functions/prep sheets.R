
# prep sheets for hand coding by adding columns 
source("setup.R")


sheets <- googledrive::drive_ls("datasheets")
#sheets <- gs_ls()

sheets %<>% .$id

# i = sheets[1] # for testing 

for (i in sheets) {
  data <- googlesheets4::gs4_get(i) %>% gs_read()
  
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
      "CONSTITUENT_TYPE",
      "CONSTITUENT_CLASS",
      "NOTES",
      "ERROR"
    )
  
  data[, variables[which(!(variables %in% names(data)))]] <- "" # create new empty variables
  
  # locate sheet i
  googlesheets4::gs4_get(i) %>% 
    # overwite just the column names, 0 rows
    googlesheets4::range_write(data[0,]) # save 

  
} # end function

