

library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X ))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

  
l <- read_excel_allsheets("DOJ_OALJraw.xlsx")

i = 1
d <- cbind(l[[i]], names(l[i]))
d %<>% mutate(NOTES = ifelse("NOTES" %in% names(dt), NOTES, NA))
d$NOTES %<>% as.character()

for(i in 2:52) {
  dt <- cbind(l[[i]], names(l[i]))
  dt %<>% mutate(NOTES = ifelse("NOTES" %in% names(dt), NOTES, NA))
  dt$NOTES %<>% as.character()
  dt$`CASE NUMBER` %<>% as.character()
  d <- full_join(d, dt)
}
i
dq <- cbind(l[[i]], names(l[i]))
dq %<>% mutate(NOTES = ifelse("NOTES" %in% names(dt), NOTES, NA))
dq$NOTES %<>% as.character()

write.csv(d, "DOL_OALJ.csv")
