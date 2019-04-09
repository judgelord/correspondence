cdc1 <- read.csv("DHHS_CDC 07 16.csv")
cdc2 <- read.csv("DHHS_CDC 08-09.csv")

for(i in 2:length(cdc2$ID)){
  if(is.na(cdc2$ID[i])){
    cdc2[i, 1:15] <- cdc2[i-1, 1:15]
  }
}

for(i in 2:length(cdc1$ID)){
  if(is.na(cdc1$ID[i]) && cdc1$FROM[i] == ""){
    cdc1[i, 1:3] <- cdc1[i-1, 1:3]
  }
  if(is.na(cdc1$ID[i])){
    cdc1$ID[i] <- cdc1$ID[i-1]
  }
}

cdc <- full_join(cdc1, cdc2)
write.csv(cdc, "DHHS_CDC.csv")


#####################################
# clean up workspace before commit #
#####################################
rm(list = ls(all = TRUE))


