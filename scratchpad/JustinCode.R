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

ert<- group_by(df, congress, icpsr, party_name,  chair, prestige, prestige_chair, chamber, majority, agency, presidents_party, oversight_committee)

total= summarise(ert, n = n())


##just a simple cross sectional

cross_pres<- lm(total$n~total$prestige + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))
cross_chair<- lm(total$n~total$chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))
cross_pres_chair<- lm(total$n~total$prestige_chair + as.factor(total$congress)  + as.factor(total$agency), subset=which(total$congress!= 115))

##obvious cross sectional difference.  The more powerful people are the ones sending more of these letters
##let's adjust for


cross_chair<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115 & total$chamber == 'House'))
cross_chair_senate<- lm(total$n~total$chair + as.factor(total$congress), subset=which(total$congress!= 115 & total$chamber == 'Senate'))



reg1<- lm(total$n~total$prestige + as.factor(total$congress)+   as.factor(total$icpsr)  + as.factor(total$agency), subset=which(total$congress!= 115))
reg2<- lm(total$n~total$prestige_chair + as.factor(total$congress)+   as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!= 115))

##checking chairs in general
reg3<- lm(total$n~total$chair + as.factor(total$congress)+   as.factor(total$icpsr) + as.factor(total$congress) , subset=which(total$congress!= 115))
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

to_be_code<- ind_type<- policy<- char_loc<- corp<-corp_con<-  rep(NA, nrow(total))
for(z in 1:nrow(total)){
	rows<- df[which(df$agency ==total$agency[z] & df$icpsr == total$icpsr[z] & df$congress==total$congress[z]),]$Type

	ind_type[z]<- len(which(rows=='Indiv. Constituent' | rows =='Corp. Constituent' | rows=='501c3 or Local Gov.'))
	policy[z]<- len(which(rows=='Policy' | rows=='Corp. Policy'))
	#char_loc[z]<- len(which(rows=='501c3 or Local Gov.'))
	#corp[z]<- len(which(rows=='Corp. Policy'))
	#corp_con[z]<- len(which(rows=='Corp. Constituent'))
	#to_be_code[z]<- len(which(rows=='To be coded'))
}

pol_letter<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency) , subset=which(total$congress!=115))
##do they send more before
pol_letter_dem<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency) , subset=which(total$congress!=115 & total$party_name=='Democratic'))
pol_letter_rep<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency) , subset=which(total$congress!=115 & total$party_name=='Republican'))

pol_future<- lm(policy~future_chair + as.factor(total$congress)  + as.factor(total$agency), subset=which(total$congress!=115 & total$chair==0))


ind_letter<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))
##do they send more before

ind_letter_rep<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name=='Republican'))
ind_letter_dem<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name=='Democratic'))

ind_future<- lm(ind_type~future_chair + as.factor(total$congress), subset=which(total$congress!=115 & total$chair==0))


ind_chair<- lm(total$ind~total$chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))
ind_prestige_comm<- lm(total$ind~total$prestige + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))
ind_prestige_chair<- lm(total$ind~total$prestige_chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))

pol_chair<- lm(total$pol~total$chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))
pol_prestige_comm<- lm(total$pol~total$prestige + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))
pol_prestige_chair<- lm(total$pol~total$prestige_chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))






##so this shows that Democrats expand on both, while Republicans
##expand solely on polcy, but decrease otherwise.  

total_out<- lm(total$n~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_ind<- lm(total$ind~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))
total_pol<- lm(total$pol~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

tt<- summary(total_ind)
gg<- summary(total_pol)


total_out_pres<- lm(total$n~total$prestige + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_ind_pres<- lm(total$ind~total$prestige + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))
total_pol_pres<- lm(total$pol~total$prestige + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_did<- summary(total_out_pres)$coef
ind_did<- summary(total_ind_pres)$coef
pol_did<- summary(total_pol_pres)$coef

total_out_pres_chair<- lm(total$n~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_ind_pres_chair<- lm(total$ind~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))
total_pol_pres_chair<- lm(total$pol~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_did_chair<- summary(total_out_pres_chair)$coef
ind_did_chair<- summary(total_ind_pres_chair)$coef
pol_did_chair<- summary(total_pol_pres_chair)$coef


total_out_pres_chair<- lm(total$n~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_ind_pres_chair<- lm(total$ind~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))
total_pol_pres_chair<- lm(total$pol~total$prestige_chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115))

total_did_chair<- summary(total_out_pres_chair)$coef
ind_did_chair<- summary(total_ind_pres_chair)$coef
pol_did_chair<- summary(total_pol_pres_chair)$coef


total_out_dem<-  lm(total$n~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name== 'Democratic'))
total_out_rep<-  lm(total$n~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name=='Republican'))
##most of the increase comes from Democrats, Republicans have no overall change in their output.

##largely becuase they tend to substitue

##then we can do shares, but then switch to majority

##so doing the share thing

outs<- cbind(ind_type, policy)
row_sum<- apply(outs, 1, sum)

for(z in 1:nrow(outs)){
	outs[z,]<- outs[z,]/row_sum[z]

}

start<- lm(outs[,1]~total$chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!= 115))
start_rep<- lm(outs[,1]~total$chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Republican'))
start_dem<- lm(outs[,1]~total$chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Democratic'))

ps_rep<- lm(outs[,2]~total$chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Republican'))
ps_dem<- lm(outs[,2]~total$chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Democratic'))


##ok, now putting together some information about majority

maj_cross<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))
maj_did<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115))

m_did_r<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Republican'))
m_did_d<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Democratic'))


##same thing with the president's party maj_cross<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))

press_cross<- lm(total$n~total$presidents_party + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))

press_did<- lm(total$n~total$presidents_party + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115))

p_did_r<- lm(total$n~total$presidents_party + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Republican'))
p_did_d<- lm(total$n~total$presidents_party + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Democratic'))

##now with the interaction of the two

p_m_cross<- lm(total$n~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))

p_m_did<- lm(total$n~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115))

p_m_did_r<- lm(total$n~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Republican'))
p_m_did_d<- lm(total$n~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115 & total$party_name=='Democratic'))


p_m_ind<- lm(total$ind~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))

p_m_pol<- lm(total$pol~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!= 115))


p_m_did_ind<- lm(total$ind~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115))

p_m_did_pol<- lm(total$pol~total$presidents_party*total$majority + as.factor(total$congress) + as.factor(total$agency)  + as.factor(total$icpsr), subset=which(total$congress!= 115))


ert<- summary(p_m_did)
ert2<- summary(p_m_did_ind)$coef
ert3<- summary(p_m_did_pol)$coef


##we can do the oversight thing next.  




##so what do we learn from this ?

years<- lm(total$n~as.factor(total$congress), subset=which(total$congress!=115))
year_agency<- lm(total$n~as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115))
year_agency_ind<- lm(total$n~as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115))
##now let's look at superivising count, for this we need to aggregate up again
##but we can include yearx individual fixed effects. 

ind_type_over<- ifelse(df$Type %in% c('Indiv. Constituent', 'Corp. Constituent' , '501c3 or Local Gov.'), 1, 0)
pol_type_over<- ifelse(df$Type %in% c('Policy', 'Corp. Policy'), 1, 0)

df<- cbind(df, ind_type_over, pol_type_over)


##let's now aggregate this by oversight committee or not.  

tenure<- df$year - df$yearelected
ert<- group_by(df, congress, icpsr, party_name,  chair, prestige, prestige_chair, chamber, majority, agency, presidents_party, oversight_committee, oversight_committee_chair, ind_type_over, pol_type_over, female, pop2010_millions)

##so, looking at oversight by time





total= summarise(ert, n = n(), overs = sum(oversight_committee), ind = sum(ind_type_over), pol = sum(pol_type_over), over_ind = sum(oversight_committee*ind_type_over), over_pol = sum(oversight_committee*pol_type_over))

##now looking at the total sent to
##seniority correlation







overs<- lm(total$overs~total$oversight_committee_chair + as.factor(total$congress) + as.factor(total$agency), subset=which(total$congress!=115 & total$oversight_committee==1))
overs_did<- lm(total$n~total$oversight_committee_chair + as.factor(total$congress) + as.factor(total$icpsr), subset=which(total$congress!=115& total$oversight_committee==1))

ind_overs<- lm(total$ind~total$oversight_committee_chair + as.factor(total$congress) +as.factor(total$agency), subset=which(total$congress!=115 & total$oversight_committee==1))
pol_overs<- lm(total$pol~total$oversight_committee_chair + as.factor(total$congress) +as.factor(total$agency), subset=which(total$congress!=115 & total$oversight_committee==1))

ind_did<- lm(total$ind~total$oversight_committee_chair + as.factor(total$congress) +as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$oversight_committee==1))
pol_did<- lm(total$pol~total$oversight_committee_chair + as.factor(total$congress) +as.factor(total$agency)+ as.factor(total$icpsr), subset=which(total$congress!=115 & total$oversight_committee==1))

##but they leverage that to increase the number of individual requrests
##which is interesting.  in the next round we'd like to know how many more requests
##you make once being placed on on an oversight committee.  

ind_over_did<- lm(total$over_ind~total$oversight_committee_chair + as.factor(total$congress)*total$party_name + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$oversight_committee==1))
pol_over_did<- lm(total$over_pol~total$oversight_committee_chair + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$oversight_committee==1))

##so that is interesting. let's now check for chamber specific effects
##and then make the relevant tables

sen_maj<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='Senate'))
house_maj<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='House'))

##alright, now doing this by party

sen_maj_rep<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='Senate' & total$party_name =='Republican'))
house_maj_rep<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='House'& total$party_name =='Republican'))

sen_maj_dem<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='Senate'& total$party_name =='Democratic'))
house_maj_dem<- lm(total$n~total$majority + as.factor(total$congress) + as.factor(total$agency) + as.factor(total$icpsr), subset=which(total$congress!=115 & total$chamber =='House'& total$party_name =='Democratic'))

##ok, so no difference there.  let's make these tables
##first, 






##next working on a corporate interest
corp_letter<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115))
##do they send more before
corp_letter_rep<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name=='Republican'))
corp_letter_dem<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) + as.factor(total$agency), subset=which(total$congress!=115 & total$party_name=='Democratic'))



corp_future<- lm(corp~future_chair + as.factor(total$congress), subset=which(total$congress!=115 & total$chair==0))

##we need to do this by party to look at effects.  
pol_letter_dem<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Democratic'))
ind_letter_dem<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Democratic'))

ind_letter_rep<- lm(ind_type~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Republican'))
pol_letter_rep<- lm(policy~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Republican'))


##now looking at the shares
corp_letter_rep<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Republican'))
corp_letter_rep<- lm(corp~total$chair + as.factor(total$congress) + as.factor(total$icpsr) , subset=which(total$congress!=115 & total$party_name=='Democratic'))


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



temp<- df[which(df$complete==T), ]

group_year<- group_by(df, year, majority, presidents_party, icpsr, party_name, chair, prestige, prestige_chair, agency)

sy<- summarise(group_year, n = n())


rep1<- lm(sy$n~as.factor(sy$year)-1 + as.factor(sy$agency), subset=which(sy$party_name=='Republican' & sy$year<2017))
dem1<- lm(sy$n~as.factor(sy$year)-1 + as.factor(sy$agency), subset=which(sy$party_name=='Democratic'& sy$year<2017))

tbc<- it<- py<- cloc<- corp<-corp_con<-  rep(0, nrow(sy))
for(z in 1:nrow(sy)){
	rows<- df[which(df$icpsr == sy$icpsr[z] & df$year==sy$year[z] & df$agency == sy$agency[z]),]$Type

	it[z]<- len(which(rows=='Indiv. Constituent' | rows=='Corp. Constituent' | rows=='501c3 or Local Gov.'))
	py[z]<- len(which(rows=='Policy'| rows=='Corp. Policy'))
	#cloc[z]<- len(which(rows=='501c3 or Local Gov.'))
	#corp[z]<- len(which(rows=='Corp. Policy'))
	#corp_con[z]<- len(which(rows=='Corp. Constituent'))
	#tbc[z]<- len(which(rows=='To be coded'))
}

checks<- cbind(it,py, cloc, corp, corp_con, tbc )

##so we can look at how individuals change their behavior in response to being in the minority
##and then deliver this 


out<- lm(it~sy$majority*sy$presidents_party+ as.factor(sy$year) + as.factor(sy$agency) , subset=which(sy$year<2017))
out_pol<- lm(py~sy$majority*sy$presidents_party+ as.factor(sy$year) + as.factor(sy$agency) , subset=which(sy$year<2017))


##you write a lot more per-party and per year 
##can create the appropriate standard errors



it_cross
total_year<- lm(sy$n~sy$majority + as.factor(sy$year)  + as.factor(sy$agency), subset=which(sy$year< 2017))
total_did_year<- lm(sy$n~sy$majority + as.factor(sy$year)  + as.factor(sy$agency) + as.factor(sy$icpsr), subset=which(sy$year< 2017))

total_party_year<- lm(sy$n~sy$presidents_party + as.factor(sy$year)  + as.factor(sy$agency), subset=which(sy$year< 2017))
total_did_party_year<- lm(sy$n~sy$presidents_party + as.factor(sy$year)  + as.factor(sy$agency) + as.factor(sy$icpsr), subset=which(sy$year< 2017))

total_did_party_maj_year<- lm(sy$n~sy$presidents_party*sy$majority + as.factor(sy$year)  + as.factor(sy$agency) + as.factor(sy$icpsr), subset=which(sy$year< 2017))




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






