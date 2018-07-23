

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


l <- read_excel_allsheets("STB2015.xlsx")

i = 1
d <- cbind(l[[i]], names(l[i]))
date <- d[3,3]
d <- d[6:nrow(d),]
#d$date <- date

for(i in 2:33) {
  dt <- cbind(l[[i]], names(l[i]))
  date <- dt[3,3]
  dt <- dt[6:nrow(dt),]
  #dt$date <- date
  d <- full_join(d, dt)
}
d15 <- d 
names(d15)

l <- read_excel_allsheets("STB2016.xlsx")

i = 1
d <- cbind(l[[i]], names(l[i]))

for(i in 2:42) {
  dt <- cbind(l[[i]], names(l[i]))
  d <- full_join(d, dt)
}
i
dq <- cbind(l[[i]], names(l[i]))

d16 <- d 

l <- read_excel_allsheets("STB2017.xlsx")

i = 1
d <- cbind(l[[i]], names(l[i]))

for(i in 2:49) {
  dt <- cbind(l[[i]], names(l[i]))
  d <- full_join(d, dt)
}
dq <- cbind(l[[i]], names(l[i]))
dq$`names(l[i])`



d17 <- d 

names(d15) <- names(d16)
d15$Date %<>% as.numeric()
d15$Date %<>% as.Date(origin="1900-01-01")
d <- rbind(d17, d16, d15)







write.csv(d, "DOL_OALJ.csv")
