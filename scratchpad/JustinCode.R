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

barplot(count_cong[1:5,2], xlab = 'Congress', ylab = 'Total Letters', names.arg = as.character(seq(110, 114, by = 1)))
dev.copy(device =pdf, file='/Users/justingrimmer/correspondence/CorrespondenceCount.pdf', height = 6, width = 6)
dev.off()

##doing the same thing with individual members, per Congress

p_cong<- group_by(d, congress, icpsr)
count_p<- as.matrix(summarize(p_cong, n = n()))

count_p2<- count_p[-which(is.na(count_p[,2])==T),]
hist(count_p2[order(count_p2[,3],  decreasing=T), 3],main= '', breaks = 50, xlab = 'Number of Letters per Congress')
dev.copy(device =pdf, file='/Users/justingrimmer/correspondence/PerCongressCount.pdf', height = 6, width = 6)
dev.off()

##how much persistance is there in the individual legislator's sending of letters?
##we can examine the

##grouping together the information of interest

ert<- group_by(df, congress, icpsr, party_name,  chair, prestige, prestige_chair, chamber, majority)

total= summarise(ert, n = n())


##just a simple cross sectional

cross_pres<- lm(total$n~total$prestige + as.factor(total$congress), subset=which(total$congress!= 115))
cross_chair<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115))
cross_pres_chair<- lm(total$n~total$prestige_chair + as.factor(total$congress), subset=which(total$congress!= 115))

##obvious cross sectional difference.  The more powerful people are the ones sending more of these letters
##let's adjust for


cross_chair<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115 & total$chamber == 'House'))
cross_chair_senate<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115 & total$chamber == 'Senate'))



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

fut_cross<- lm(total$n~future_chair + as.factor(total$congress), subset = which(total$congress!=115 & total$chair==0))
##future chairs issue 30 more letters per year than other individuals
##and

##let's do this with party and year, so that we look at this within party and year



party_year<- lm(total$n ~total$chair +   as.factor(total$congress)*total$party_name + as.factor(total$icpsr), subset=which(total$congress!=115))

##that is 15.8 

##there is incredible within party heterogeneity.  Republicans have almost no 
##bump from becoming chair. 

##ok, let's do this by type

##[1] To be coded         Indiv. Constituent  501c3 or Local Gov.
##[4] Policy              Corp. Constituent   Corp. Policy

to_be_code<- ind_type<- policy<- char_loc<- corp<-corp_con<-  rep(0, nrow(total))
for(z in 1:nrow(total)){
	rows<- df[which(df$icpsr == total$icpsr[z] & df$congress==total$congress[z]),]$Type

	ind_type[z]<- len(which(rows=='Indiv. Constituent'))
	policy[z]<- len(which(rows=='Policy'))
	char_loc[z]<- len(which(rows=='501c3 or Local Gov.'))
	corp[z]<- len(which(rows=='Corp. Policy'))
	corp_con[z]<- len(which(rows=='Corp. Constituent'))
	to_be_code[z]<- len(which(rows=='To be coded'))
}

pol_letter<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115))
##do they send more before

pol_future<- lm(policy~future_chair + as.factor(total$congress), subset=which(total$congress!=115 & total$chair==0))


ind_letter<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115))
##do they send more before

ind_future<- lm(ind_type~future_chair + as.factor(total$congress), subset=which(total$congress!=115 & total$chair==0))


##next working on a corporate interest
corp_letter<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115))
##do they send more before

corp_future<- lm(corp~future_chair + as.factor(total$congress), subset=which(total$congress!=115 & total$chair==0))

##we need to do this by party to look at effects.  
pol_letter_dem<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Democratic'))
ind_letter_dem<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Democratic'))

ind_letter_rep<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Republican'))


##now looking at the shares


types<- cbind(ind_type, policy, char_loc, corp, corp_con)
sums<- apply(types, 1, sum)

for(z in 1:nrow(total)){
	types[z,]<- types[z,]/sums[z]


}


cross_ind_share<- lm(types[,1]~total$chair + as.factor(total$congress) , subset=which(total$congress!=115))
cross_ind_fut_share<- lm(types[,1]~future_chair + as.factor(total$congress) , subset=which(total$congress!=115 & total$chair==0))

did_ind_share<- lm(types[,1]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115))

##

cross_pol_share<- lm(types[,2]~total$chair + as.factor(total$congress) , subset=which(total$congress!=115))
cross_pol_fut_share<- lm(types[,2]~future_chair + as.factor(total$congress) , subset=which(total$congress!=115 & total$chair==0))

did_pol_share<- lm(types[,2]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115))


##now looking at corporations

cross_corp_share<- lm(types[,4]~total$chair + as.factor(total$congress) , subset=which(total$congress!=115))
cross_corp_fut_share<- lm(types[,4]~future_chair + as.factor(total$congress) , subset=which(total$congress!=115 & total$chair==0))

did_corp_share<- lm(types[,4]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115))

##no effect on corporations
cross_corpp_share<- lm(types[,5]~total$chair + as.factor(total$congress) , subset=which(total$congress!=115))
cross_corpp_fut_share<- lm(types[,5]~future_chair + as.factor(total$congress) , subset=which(total$congress!=115 & total$chair==0))

did_corpp_share<- lm(types[,5]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115))

##finally doing local work 

cross_local_share<- lm(types[,5]~total$chair + as.factor(total$congress) , subset=which(total$congress!=115))
cross_local_fut_share<- lm(types[,5]~future_chair + as.factor(total$congress) , subset=which(total$congress!=115 & total$chair==0))

did_local_share<- lm(types[,5]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115))

##ok, so the big differences are in the individual and policy 
##where they decrease

##there are cross sectional differences


it_d<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
##substantial difference between how Democrats 
it_r<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))

it_ds<- lm(types[,1]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
it_rs<- lm(types[,1]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))

##there is no change in the use of resources for constituents for Democrats
##but a big decrease for Republicans.  




p_d<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
p_r<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))

##now share
p_ds<- lm(types[,2]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
p_rs<- lm(types[,2]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))




c_d<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
c_r<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))

##now share
c_ds<- lm(types[,4]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
c_rs<- lm(types[,4]~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))


cp_d<- lm(corp_con~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))
cp_r<- lm(corp_con~total$chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Republican'))


##we need to setup this to do this at the agency level
##and see if represnetatives.  The clear lesson is that Republicans respond differently
##should include if they are in the majority

total_lets<- lm(total$n~ total$majority+ as.factor(total$icpsr), subset=which(total$congress!=115))

total_r<- lm(total$n~ total$majority+ as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=="Republican"))
total_d<- lm(total$n~ total$majority+ as.factor(total$icpsr), subset=which(total$congress!=115 & total$party_name=='Democratic'))

##different reaction to being in the minority.  we need to group this by year



group_year<- group_by(df, year, majority, presidents_party, icpsr, party_name, chair, prestige, prestige_chair)

sy<- summarise(group_year, n = n())


rep1<- lm(sy$n~as.factor(sy$year)-1, subset=which(sy$party_name=='Republican'))
dem1<- lm(sy$n~as.factor(sy$year)-1, subset=which(sy$party_name=='Democratic'))

tbc<- it<- py<- cloc<- corp<-corp_con<-  rep(0, nrow(sy))
for(z in 1:nrow(sy)){
	rows<- df[which(df$icpsr == sy$icpsr[z] & df$year==sy$year[z]),]$Type

	it[z]<- len(which(rows=='Indiv. Constituent'))
	py[z]<- len(which(rows=='Policy'))
	cloc[z]<- len(which(rows=='501c3 or Local Gov.'))
	corp[z]<- len(which(rows=='Corp. Policy'))
	corp_con[z]<- len(which(rows=='Corp. Constituent'))
	tbc[z]<- len(which(rows=='To be coded'))
}

checks<- cbind(it,py, cloc, corp, corp_con, tbc )

##so we can look at how individuals change their behavior in response to being in the minority
##and then deliver this 


it_cross<- lm(it~sy$majority + as.factor(sy$year) , subset=which(sy!= 2018))
it_did<- lm(it~sy$majority + as.factor(sy$year) + as.factor(sy$icpsr) , subset=which(sy!= 2018))

it_cross<- lm(it~sy$majority*sy$presidents_party + as.factor(sy$year), subset=which(sy!=2018))
it_did<- lm(it~sy$majority*sy$presidents_party + as.factor(sy$year) + as.factor(sy$icpsr) , subset=which(sy!= 2018))


py_cross<- lm(py~sy$majority + as.factor(sy$year) , subset=which(sy!= 2018))
py_did<- lm(py~sy$majority + as.factor(sy$year) + as.factor(sy$icpsr) , subset=which(sy!= 2018))

##alright, let's look at the majority with an outparty president
py_cross<- lm(py~sy$majority*sy$presidents_party + as.factor(sy$year), subset=which(sy!=2018))
py_did<- lm(py~sy$majority*sy$presidents_party + as.factor(sy$year) + as.factor(sy$icpsr) , subset=which(sy!= 2018))

out<- lm(sy$n~sy$majority*sy$presidents_party +  as.factor(sy$icpsr) +  as.factor(sy$year))
##pretty clear increase there
out_d<- lm(sy$n~sy$majority*sy$presidents_party +  as.factor(sy$icpsr) +  as.factor(sy$year))






