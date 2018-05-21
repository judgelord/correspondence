# prep sheets for hand coding

sheets <- gs_ls()
sheets %<>% filter(author == "correspondenceresearch") %>% select("sheet_title")
sheets <- sheets$sheet_title

for (i in sheets) {
  i <- "DOE_FERC"
  data <- gs_title(i) %>% gs_read()
  
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
  
  data[, variables[which(!(variables %in% names(data)))]] <- ""
  
  if (!("ID" %in% names(data))) {
    data$ID <- seq(1:dim(data)[1])
    
  }
  
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
  
  gs_title(i) %>% gs_edit_cells(input = data, trim = TRUE)
  
}
