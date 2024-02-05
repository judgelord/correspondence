
# prep sheets for hand coding by adding columns 
source("setup.R")


sheets <- googledrive::drive_ls("datasheets")

# # Subsetting 
sheets %<>% map_df(rev)
# sheets$name == "DHHS_CMS Rochelle"
# sheets <- sheets[31:dim(sheets), ]
#sheets <- gs_ls()

sheets %<>% .$id

# i = sheets[1] # for testing 
updated <- NA

sheets <- sheets[!sheets %in% updated]

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
  
  if(sum(!names(data) %in% variables)>0){
  
  data[, variables[which(!(variables %in% names(data)))]] <- "" # create new empty variables
  
  # locate sheet i
  googlesheets4::gs4_get(i) %>% 
    # overwite just the column names, 0 rows
    googlesheets4::range_write(data[0,]) # save 
  } # end if
  
  updated <- c(updated, i)
  
} # end function

