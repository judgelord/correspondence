# cdc1 <- read.csv("DHHS_CDC 07 16.csv")
# cdc2 <- read.csv("DHHS_CDC 08-09.csv")
# 
# for(i in 2:length(cdc2$ID)){
#   if(is.na(cdc2$ID[i])){
#     cdc2[i, 1:15] <- cdc2[i-1, 1:15]
#   }
# }
# 
# for(i in 2:length(cdc1$ID)){
#   if(is.na(cdc1$ID[i]) && cdc1$FROM[i] == ""){
#     cdc1[i, 1:3] <- cdc1[i-1, 1:3]
#   }
#   if(is.na(cdc1$ID[i])){
#     cdc1$ID[i] <- cdc1$ID[i-1]
#   }
# }
# 
# cdc <- full_join(cdc1, cdc2)
# write.csv(cdc, "DHHS_CDC.csv")


#####################################
# clean up workspace before commit #
#####################################

# load 2010-11 data
cdc <- read_csv("https://www.dropbox.com/s/zugc4hgjrdj2t72/DHHS_CDC%202010-2011.csv?raw=1")
as.character(cdc[1,])
as.character(cdc[2,])

# fix duplicate col name to match other data 
cdc[1,11] <- "Date Of Communication"

# col names 
names(cdc) <- cdc[1,]

cdc %<>% rename(Correspondent = CORRESPONDENT)

# load 2012-13 data 
cdc1 <- read_csv("https://www.dropbox.com/s/kf64o9opvt84ajd/DHHS_CDC%202012-2013.csv?raw=1") 

# col names 
names(cdc1) <- cdc1[1,]

# merge two years 
cdc %<>% full_join(cdc1)

# check
names(cdc)
look <- cdc %>% 
  group_by(Correspondent) %>% 
  count(`Folder Id`) %>% arrange(-n)

cdc %<>% 
  rename(DATE = `Date Of Communication`,
         FROM = Correspondent,
         SUBJECT = Synopsis,
         `FolderID` = `Folder Id`)

# non-observations, good
cdc %>% filter(is.na(DATE))
cdc %>% filter(is.na(SUBJECT))

# looks like non-members, good 
cdc %>% filter(is.na(FROM))

########################################
# GOOGLE SHEET DATA 
data <- gs_title("DHHS_CDC") %>% gs_read() # get data
names(data)

data %<>% 
  mutate(FolderID = as.character(ID))

data %<>% full_join(cdc)

nrow(data)


write_csv(data, path = "data/DHHS_CDC.csv")


