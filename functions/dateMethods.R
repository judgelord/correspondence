

# Formats DATE column when multiple date formats exist
# example call:   data$DATE <- multidate(data$DATE, c("%d-%b-%y", "%b %d,%Y"))
multidate <- function(col, formats){
  a<-list()
  for(i in 1:length(formats)){
    a[[i]]<- as.Date(col,format=formats[i])
    a[[1]][!is.na(a[[i]])]<-a[[i]][!is.na(a[[i]])]
  }
  a[[1]]
}