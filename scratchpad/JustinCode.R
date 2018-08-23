##located in scratchpad

load("gh-pages/correspondence.RData") # load df (df is d + covariates + dropping obs not matching an ICPSR)
source("setup.R") # load packages and functions


##alright, putting together some simple summaries

df %<>% mutate(bioname_congress = paste(bioname, congress))

df %<>% mutate(bioname_congress_type = paste(bioname_congress, Type))

zeros <- data_frame(
  bioname_congress = rep(unique(df$bioname_congress), n_distinct(df$Type)),
  Type =  rep(unique(df$Type), n_distinct(df$bioname_congress)),
  permemberyear = 0) %>%
  mutate(bioname_congress_type = paste(bioname_congress, Type)) %>%
  filter(!bioname_congress_type %in% df$bioname_congress_type)

df %<>% full_join(zeros)



groups<- group_by(d, icpsr, congress)

counts<- summarise(groups, n= n())
##excluding the last row to avoid grouping with NA
counts2<- as.matrix(counts[,])
counts3<- counts2[which(is.na(counts2[,1])==F),]
##let's calculate the


reg1<- lm(counts3[,3]~as.factor(counts3[,2]) + as.factor(counts3[,1]))
###we can also look at the per-individual effects


reg1$coef[7:length(reg1$coef)]
##can just calcuate that in each Congress.
##so quickly, looking at the per Congress average

reg_cong<- lm(counts3[,3] ~as.factor(counts3[,2]))

g_cong<- group_by(d, congress)
count_cong<- as.matrix(summarize(g_cong, n = n()))

barplot(count_cong[1:5,2], xlab = 'Congress', ylab = 'Total Letters')
dev.copy(device =pdf, file='/Users/jgrimmer/correspondence/paper/CorrespondenceCount.pdf', height = 6, width = 6)
dev.off()

##doing the same thing with individual members, per Congress

p_cong<- group_by(d, congress, icpsr)
count_p<- as.matrix(summarize(p_cong, n = n()))

count_p2<- count_p[-which(is.na(count_p[,2])==T),]
hist(count_p2[order(count_p2[,3], decreasing=T), 3], breaks = 50, xlab = 'Number of Letters per Congress')
dev.copy(device =pdf, file='/Users/jgrimmer/correspondence/paper/PerCongressCount.pdf', height = 6, width = 6)
dev.off()

##how much persistance is there in the individual legislator's sending of letters?
##we can examine the

##grouping together the information of interest

ert<- group_by(df, congress, icpsr, party_name,  chair, prestige, prestige_chair, chamber)

total= summarise(ert, n = n())


##just a simple cross sectional

cross_pres<- lm(total$n~total$prestige + as.factor(total$congress), subset=which(total$congress!= 115))
cross_chair<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115))
cross_pres_chair<- lm(total$n~total$prestige_chair + as.factor(total$congress), subset=which(total$congress!= 115))

##obvious cross sectional difference.  The more powerful people are the ones sending more of these letters
##let's adjust for


cross_chair<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115 & total$chamber == 'House'))



reg1<- lm(total$n~total$prestige + as.factor(total$congress)+   as.factor(total$icpsr) , subset=which(total$congress!= 115))
reg2<- lm(total$n~total$prestige_chair + as.factor(total$congress)+   as.factor(total$icpsr) , subset=which(total$congress!= 115))

##checking chairs in general
reg3<- lm(total$n~total$chair + as.factor(total$congress)+   as.factor(total$icpsr) , subset=which(total$congress!= 115))
##chairs send a shit ton more.  that is *within* legislator
##they

reg3_sen<- lm(total$n~total$chair + as.factor(total$congress)+   as.factor(total$icpsr) ,
    subset=which(total$congress!= 115 &  total$chamber == 'Senate'))

reg3_house<- lm(total$n~total$chair + as.factor(total$congress)+   as.factor(total$icpsr) ,
    subset=which(total$congress!= 115 &  total$chamber == 'House'))


##examining who becomes a chair

chair_icpsr<- total$icpsr[which(total$chair==1)]
future_chair<- rep(0, nrow(total))

future_chair[which(total$icpsr %in% chair_icpsr & total$chair==0)]<- 1
##

##let's count the types.





