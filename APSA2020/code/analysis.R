##############
##############
##############
####
####
####  Analysis of  Correspondence Representation Data
####
####
##############
##############
##############


load('/Users/justingrimmer/Dropbox/Correspondence/data/dcounts.RData')
load('/Users/justingrimmer/Dropbox/Correspondence/data/members.RData')
library(dplyr)


df<- dcounts
##first congress data
nom<- read.delim('/Users/justingrimmer/Correspondence/data/nominate_data.csv', sep=',')


nom2<- nom %>% group_by(icpsr) %>% summarise(first_cong = min(congress))

nom2$first_year <- 1787 + 2*nom2$first_cong
df<- left_join(df, nom2, by = 'icpsr')

df$tenure <- df$year - df$first_year

##adding the max year 
final_tenure<- df %>% group_by(icpsr) %>%  summarise(max_year = max(tenure))

df<- left_join(df, final_tenure , by = 'icpsr')



##merge in census data
##2) merge in year data
##3) perform the simple analyses about district characteristics
##4) perform the analyses above to see if there are differences in the partisan representative in 
##the district. 



##let's put together the basic facts for this analysis. We want to know more about how Republicans and Democrats
##and then 


df1<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2009_Data.csv', sep=',')
df2<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2017_Data.csv', sep=',')





use1<- df1 %>% select(GEO.display.label, HC01_EST_VC01, HC02_EST_VC01)

use2<- df2 %>% select(GEO.display.label, HC01_EST_VC01, HC03_EST_VC01, HC04_EST_VC01, HC01_EST_VC17, HC02_EST_VC17, HC01_EST_VC18,HC02_EST_VC18)

df3<- read.delim('/Users/justingrimmer/Dropbox/correspondence/data/ACS_Download/2009_Data_Age.csv', sep=',')


use3<- df3 %>% select(GEO.display.label, HC01_EST_VC01, HC01_EST_VC16, HC01_EST_VC17, HC01_EST_VC18, HC01_EST_VC19, HC01_EST_VC20)

##writing a function  to parse the label
library(stringr)
parse_label<- function(x){
    out<- grep('Congressional District', x)
    if(len(out)==1){
    match1<- regexpr('Congressional District [0-9]+|Congressional District \\(at Large\\)', x)
    content<- regmatches(as.character(x), match1)
    dist_num<- regmatches(as.character(x), regexpr('[0-9]+|\\(at Large\\)', x))
    dist_num<- ifelse(dist_num=='(at Large)', '1', dist_num)
    state<- gsub("^ ", '', strsplit(as.character(x), split = ',')[[1]][2])
    output<- paste(tolower(state), dist_num, sep='_')
    return(output)
  }
    if(len(out)==0){
      output<- NA
      return(output)
    }
}


use1_ids<- c()
for(z in 2:nrow(use1)){

  use1_ids[z]<- parse_label(use1[z,1])

}

use2_ids<- c()
for(z in 2:nrow(use2)){

  use2_ids[z]<- parse_label(use2[z,1])

}

use3_ids<- c()
for(z in 2:nrow(use3)){

  use3_ids[z]<- parse_label(use3[z,1])

}

##
##now arranging the data to join back to the original data.  

##this is the 2006-2009 data, veterans
use1_num<- use1[-1,]
use1_num[,2]<- as.numeric(as.character(use1_num[,2]))
use1_num[,3]<- as.numeric(as.character(use1_num[,3]))


##this is the 2006-2009 data, population

use3_num<- use3[-1,]
use3_num[,2]<- as.numeric(as.character(use3_num[,2]))
use3_num[,3]<- round((as.numeric(as.character(use3_num[,3]))/100)*use3_num[,2])
use3_num[,4]<- round((as.numeric(as.character(use3_num[,4]))/100)*use3_num[,2])
use3_num[,5]<- round((as.numeric(as.character(use3_num[,5]))/100)*use3_num[,2])
use3_num[,6]<- round((as.numeric(as.character(use3_num[,6]))/100)*use3_num[,2])
use3_num[,7]<- round((as.numeric(as.character(use3_num[,7]))/100)*use3_num[,2])

##this is the 2010-2019 data, population + veterans

use2_num<- use2[-1,]

use2_num[,2]<- as.numeric(as.character(use2_num[,2]))
use2_num[,3]<- as.numeric(as.character(use2_num[,3]))
use2_num[,4]<- as.numeric(as.character(use2_num[,4]))
use2_num[,5]<- as.numeric(as.character(use2_num[,5]))
use2_num[,6]<- as.numeric(as.character(use2_num[,6]))
use2_num[,7]<- as.numeric(as.character(use2_num[,7]))


colnames(use1_num)<- c('state_dist', 'total', 'veterans')
colnames(use2_num)<- c('state_dist', 'total', 'veterans', 'percent_veterans', 'total_65_74', 'percent_65_74', 'total_75', 'percent_75')
colnames(use3_num)<- c('state_dist', 'total', 'total_65_69', 'total_70_74', 'total_75_79', 'total_80_84', 'total_85')


use1_num[,1]<- use1_ids[-1]
use2_num[,1]<- use2_ids[-1]
use3_num[,1]<- use3_ids[-1]


use1_num$percent_veterans<- use1_num[,3]/use1_num[,2]
use2_num$total_65_plus<- use2_num$total_65_74 + use2_num$total_75
use2_num$percent_veterans<- use2_num$veterans/use2_num$total
use3_num$total_65_plus<- use3_num$total_65_69+ use3_num$total_70_74 + use3_num$total_75_79 + use3_num$total_80_84 + use3_num$total_85


##be careful with totals, because the total is not commensurable

##alright, now we're going to load the appropriate data.  


for(z in 1:nrow(use1_num)){
  use1_num$state[z]<- strsplit(use1_num[z,1], split='_')[[1]][1]
  use2_num$state[z]<- strsplit(use2_num[z,1], split='_')[[1]][1]
  use3_num$state[z]<- strsplit(use3_num[z,1], split='_')[[1]][1]

}


state1<- use1_num %>% group_by(state) %>% summarise(total = sum(total), veterans = sum(veterans))
state2<- use2_num %>% group_by(state) %>% summarise(total = sum(total), veterans = sum(veterans), total_65_plus = sum(total_65_plus))
state3<- use3_num %>% group_by(state) %>% summarise(total = sum(total), total_65_plus = sum(total_65_plus))

state1$state_dist<- rep(NA, nrow(state1))
state2$state_dist<- rep(NA, nrow(state2))
state3$state_dist<- rep(NA, nrow(state3))


for(z in 1:nrow(state1)){
  state1$state_dist[z]<- paste(state1[z,1], '0', sep='_')
  state2$state_dist[z]<- paste(state2[z,1], '0', sep='_')
  state3$state_dist[z]<- paste(state3[z,1], '0', sep='_')
}


use1_sub<- use1_num %>% select(state_dist, total, veterans)
state1_sub<-  state1 %>% select(state_dist, total, veterans)

use3_sub<- use3_num %>% select(total_65_plus)
state3_sub<- state3 %>% select(total_65_plus)


total_65_plus<- c(unlist(use3_sub[,1]), unlist(state3_sub[,1]))

use1_sub<- cbind(rbind(use1_sub, state1_sub), total_65_plus)

use2_sub<- use2_num %>% select(state_dist, total, veterans, total_65_plus)

state2_sub<- state2 %>% select(state_dist, total, veterans, total_65_plus)

use2_sub<- rbind(use2_sub, state2_sub)





df$state_dist<- paste(paste(df$state, df$district_code, sep='_'))
df$state_dist[which(df$year<2011)]<- paste(df$state_dist[which(df$year<2011)], 'early', sep='_')
df$state_dist[which(df$year>2010)]<- paste(df$state_dist[which(df$year>2010)], 'late', sep='_')

use1_sub$state_dist<- paste(use1_sub$state_dist, 'early', sep='_')
use2_sub$state_dist<- paste(use2_sub$state_dist, 'late', sep='_')


use<- rbind(use1_sub, use2_sub)

df_merge<- left_join(df, use, by = 'state_dist')

##alright, now we can do the merge on the veterans and the population 
##


f<- read.delim('/Users/justingrimmer/Dropbox/Correspondence/data/ACS_Download/poverty/ACSST1Y2010.S1701_data_with_overlays_2020-08-25T153002.csv', sep=',')


labels<- rep(NA, nrow(f))
for(z in 2:len(labels)){
	labels[z]<- parse_label(as.character(f[z,2]))
}

f$labels<- labels
state<- rep(NA, 438)

f$number<- as.numeric(as.character(f[,95]))
f$total_number<- as.numeric(as.character(f[,3]))

for(z in 2:438){
	state[z]<- strsplit(labels, split='_')[[z]][[1]]
}

f$state<- state

state_num<- f %>% group_by(state) %>% summarise(num_pov = sum(number), total_number = sum(total_number))
state_num$state<- paste(state_num$state, '0', sep='_')


use_sub<- f %>%select(labels, number, total_number)
colnames(use_sub)<- c('state_dist', 'num_pov', 'total_number')
colnames(state_num)<- c('state_dist', 'num_pov', 'total_number')
use_sub<- rbind(use_sub, state_num)
use_sub$state_dist<- paste(use_sub[,1], 'late', sep='_')

use_sub<- use_sub[-1,]

df_merge<- left_join(df_merge, use_sub, by = 'state_dist')


##loading the median income information from 2010

med_inc<- read.delim('/Users/justingrimmer/Dropbox/Correspondence/data/ACS_Download/poverty/ACSDT1Y2010.B19013_data_with_overlays_2020-08-25T174053.csv', sep=',')

labels<- rep(NA, nrow(med_inc))

for(z in 2:len(labels)){
	labels[z]<- parse_label(as.character(med_inc[z,2]))
}


med_inc$labels<- labels
state<- rep(NA, 438)

med_inc$med_inc<- as.numeric(as.character(med_inc[,3]))

for(z in 2:438){
	state[z]<- strsplit(labels, split='_')[[z]][[1]]
}

med_inc$state<- state

state_num<- med_inc %>% group_by(state) %>% summarise(med_inc = mean(med_inc))
state_num$state<- paste(state_num$state, '0', sep='_')


use_sub<- med_inc %>%select(labels, med_inc)
colnames(use_sub)<- c('state_dist', 'med_inc')
colnames(state_num)<- c('state_dist', 'med_inc')
use_sub<- rbind(use_sub, state_num)
use_sub$state_dist<- paste(use_sub[,1], 'late', sep='_')

use_sub<- use_sub[-1,]
df_merge<- left_join(df_merge, use_sub, by = 'state_dist')



d<- df_merge %>% subset(chamber != 'President') %>% group_by(icpsr, chamber, year,party_code,party_name,  tenure, max_year, district_code, 
			state_abbrev, total,veterans, total_65_plus, num_pov, total_number, med_inc, state_dist) %>% summarise(
						tot_mil = sum(per_icpsr_chamber_year_agency_military), 
						tot_vet = sum(per_icpsr_chamber_year_agency_vet), 
						tot_sen = sum(per_icpsr_chamber_year_agency_senior), 
						tot_low_inc = sum(per_icpsr_chamber_year_agency_lowincome), 
						tot_hardship = sum(per_icpsr_chamber_year_agency_hardship), 
						overall= sum(per_icpsr_chamber_year_agency_type),
						prop_mil = sum(per_icpsr_chamber_year_agency_military)/sum(per_icpsr_chamber_year_agency_type),
						prop_vet = sum(per_icpsr_chamber_year_agency_vet)/sum(per_icpsr_chamber_year_agency_type),
						prop_sen = sum(per_icpsr_chamber_year_agency_senior)/sum(per_icpsr_chamber_year_agency_type),
						prop_low_inc = sum(per_icpsr_chamber_year_agency_lowincome)/sum(per_icpsr_chamber_year_agency_type),
						prop_hardship = sum(per_icpsr_chamber_year_agency_hardship)/sum(per_icpsr_chamber_year_agency_type),
						)




d$year_chunk<- ifelse(d$year < 2011, 0, 1)
df_merge$year_chunk<- ifelse(df$year < 2011, 0, 1)

d$state_district<- paste(paste(d$state_abbrev, d$district_code, sep='_'),  d$year_chunk, sep='_')
d$state_district[d$chamber=='Senate']<- paste(d$state_abbrev[d$chamber=='Senate'], d$district_code[d$chamber=='Senate'], sep='_')


df_merge$state_district<- paste(paste(df_merge$state_abbrev, df_merge$district_code, sep='_'),df_merge$year_chunk, sep='_')
df_merge$state_district[df_merge$chamber =='Senate'] <- paste(df_merge$state_abbrev[df$chamber=='Senate'], df_merge$district_code[df_merge$chamber=='Senate'], sep='_')


d$perc_vet<- d$veterans/d$total
d$perc_old<- d$total_65_plus/d$total

d$perc_poor<- d$num_pov/d$total_number


d$senate<- ifelse(d$chamber=='Senate', 1, 0)

d$republican<- ifelse(d$party_name=='Republican Party', 1, 0) 
d$democrat<- ifelse(d$party_name=='Democratic Party', 1, 0)

d$state_dist<- ifelse(d$chamber=='Senate', gsub('_late|_early', '', d$state_dist), d$state_dist)


library(mgcv)
library(ggplot2)
g1<- d %>% ggplot(aes(x = perc_vet, y = overall)) + geom_smooth() + xlab('Percent Veterans') + ylab('Total Letters')
g2<- d %>% ggplot(aes(x = perc_old, y = overall)) + geom_smooth() + xlab('Percent Old') + ylab('Total Letters')
g3<- d %>% ggplot(aes(x = perc_poor, y = overall)) + geom_smooth() + xlab('Percent Poor') + ylab('Total Letters')
g4<- d %>% ggplot(aes(x = med_inc, y = overall)) + geom_smooth() + xlab('Median Income') + ylab('Total Letters')

g5<- d  %>% subset(senate ==1) %>% ggplot(aes(x = total, y = overall)) + geom_smooth() + xlab('Population') + ylab('Total Letters')


wtd.quantile(d$med_inc, w = d$overall, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(d$med_inc, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=T)
 
wtd.quantile(d$perc_vet, w = d$overall, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(d$perc_vet, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=T)

wtd.quantile(d$perc_old, w = d$overall, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(d$perc_old, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=T)

wtd.quantile(d$perc_poor, w = d$overall, c(0.1, 0.25, 0.5, 0.75, 0.9))
quantile(d$perc_poor, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm=T)



library(foreign)
write.dta(d, file='~/Dropbox/CorRep/data/RepData.dta')


##

temp<- df_merge %>% group_by(agency, icpsr, year, chamber, party_code,party_name,  tenure, max_year, district_code, 
			state_abbrev, total,veterans, total_65_plus, num_pov, total_number, med_inc, state_district, state_dist)  %>% 
			summarise('per_icpsr_year_agency' = sum(per_icpsr_chamber_year_agency_type))

temp$state_dist<- ifelse(temp$chamber=='Senate', gsub('_late|_early', '', temp$state_dist), temp$state_dist)

temp$republican<- ifelse(temp$party_name=='Republican Party', 1, 0)


write.dta(temp, file='~/Dropbox/CorRep/data/PerAgencyCount.dta')
##joining in the jacobson data.  


mil<- lm(tot_mil~perc_vet + as.factor(chamber), data = d)
vet<- lm(tot_vet~perc_vet + as.factor(chamber), data = d)
poor<- lm(tot_low_inc~perc_poor + as.factor(chamber), data = d)
hard<- lm(tot_hardship~I(med_inc/10000) + as.factor(chamber) , data = d)

mil_prop<- lm(prop_mil~perc_vet + as.factor(chamber), data = d)
vet_prop<- lm(prop_vet~perc_vet + as.factor(chamber), data = d)



##with year fixed effects 


mil_year<- lm(tot_mil~perc_vet*as.factor(party_name) + as.factor(chamber) + as.factor(year) , data = d)
vet_year<- lm(tot_vet~perc_vet*as.factor(party_name) + as.factor(chamber) + as.factor(year) , data = d)
poor_year<- lm(tot_low_inc~perc_poor*as.factor(party_name) + as.factor(chamber) + as.factor(year) , data = d)
hard_year<- lm(tot_hardship~I(med_inc/10000)*as.factor(party_name) + as.factor(chamber) + as.factor(year), data = d)




old<- lm(tot_sen~perc_old + as.factor(chamber),  data = d)

##so what do we conclude from this?


un_agency<- unique(temp$agency)



coef<- rep(NA, len(un_agency))
log_coef<- rep(NA, len(un_agency))

for(z in 35:nrow(temp)){
	coef[z]<- lm(per_icpsr_year_agency~ republican + as.factor(year) + as.factor(state_district) , data = temp, subset=which(agency==un_agency[z]))$coef[2]
	log_coef[z]<- lm(I(log(per_icpsr_year_agency + 1))~ republican + as.factor(year) + as.factor(state_district) , data = temp, subset=which(agency==un_agency[z]))$coef[2]

}

cbind(log_coef[order(log_coef, decreasing=T)], un_agency[order(log_coef, decreasing=T)])
cbind(coef[order(coef, decreasing=T)], un_agency[order(coef, decreasing=T)])

total_nums<- temp %>% group_by(agency) %>%summarise(total_agency = sum(per_icpsr_year_agency))

part1<- cbind(log_coef[order(log_coef, decreasing=T)], un_agency[order(log_coef, decreasing=T)])
colnames(part1)<- c('coef', 'agency')
part1<- as.data.frame(part1)
part2<- cbind(part1, total_nums$total_agency[order(log_coef, decreasing=T)])


inc<- lm(tot_low_inc~ as.factor(party_name) + as.factor(chamber) + as.factor(year)+ as.factor(state_district), data = d)
mil<- lm(tot_mil~ as.factor(party_name) + as.factor(year) + as.factor(state_district), data = d)
vet<- lm(tot_vet~ as.factor(party_name) + as.factor(chamber) + as.factor(year), data = d)
hard<- lm(tot_hardship~ as.factor(party_name) + as.factor(chamber) + as.factor(year), data = d)
sen<- lm(tot_sen~as.factor(party_name) + as.factor(chamber) + as.factor(year), data = d)

##we want to be able to assess individual effects now, so we can create a data set wwith state
##and district indices.  
inc_fix<- lm(tot_low_inc~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
vet_fix<- lm(tot_vet~  as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))



over_fix<- lm(overall~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
over_fix_senate<- lm(overall~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='Senate'))

vet_fix<- lm(tot_vet~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
mil_fix<- lm(tot_mil~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
sen_fix<- lm(tot_sen~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
hard_fix<- lm(tot_hardship~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
inc_fix<- lm(tot_low_inc~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))

prop_vet_fix<- lm(prop_vet~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
prop_mil_fix<- lm(prop_mil~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
prop_sen_fix<- lm(prop_sen~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
prop_hard_fix<- lm(prop_hardship~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))
prop_inc_fix<- lm(prop_low_inc~ as.factor(party) + as.factor(year) + as.factor(state_district), data = d, subset=which(chamber=='House'))


per_ag<- lm(per_icpsr_chamber_year_agency_type~as.factor(party) + as.factor(year) + as.factor(state_district) , data = dcounts, subset=which(chamber =='House'))

##1) 