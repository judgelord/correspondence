#########################
#########################
####
####
####
#### Stata code for Snyder, Grimmer, Judge-Lord, and Powell
####
##########################
##########################

clear 

use /users/justingrimmer/Dropbox/CorRep/data/Business.dta

destring year, replace

egen ia_fixed = group(state_district)
egen ya_fixed = group(year)

reg tot_mil perc_vet senate , cluster(ia_fixed)
reg tot_vet perc_vet senate , cluster(ia_fixed)
reg tot_sen perc_old senate , cluster(ia_fixed)
reg tot_hardship  med_inc senate, cluster(ia_fixed)
reg tot_low_inc  perc_poor senate, cluster(ia_fixed)


gen log_mil = log(tot_mil + 1)
gen log_vet = log(tot_vet + 1)
gen log_sen = log(tot_sen + 1)
gen log_hard = log(tot_hardship + 1)
gen log_low = log(tot_low_inc + 1)
gen log_overall = log(overall + 1)

reg log_mil perc_vet senate , cluster(ia_fixed)
reg log_vet perc_vet senate , cluster(ia_fixed)
reg log_sen perc_old senate , cluster(ia_fixed)
reg log_hard  med_inc senate, cluster(ia_fixed)
reg log_low med_inc senate, cluster(ia_fixed)
reg log_low  perc_poor senate, cluster(ia_fixed)
reg log_hard  perc_poor senate, cluster(ia_fixed)

gen vet_rep = perc_vet*republican
gen old_rep = perc_old*republican
gen med_rep = med_inc*republican
gen poor_rep = perc_poor *republican 

gen pop_total = total/1000000

gen letter_person = overall/total

##let's now interac this with Republicans to see differential responsiveness

eststo clear 
eststo: reg log_mil perc_vet republican senate i.year , cluster(ia_fixed)
estadd local DV "Mil", replace 

eststo: reg log_vet perc_vet republican senate i.year , cluster(ia_fixed)
estadd local DV "Vet", replace 

eststo: reg log_sen perc_old republican senate i.year , cluster(ia_fixed)
estadd local DV "Senior", replace 

eststo: reg log_low med_inc republican senate i.year, cluster(ia_fixed)
estadd local  DV "Poor", replace 

eststo: reg log_low perc_poor republican senate i.year, cluster(ia_fixed)
estadd local  DV "Poor", replace 

##

esttab using /Users/justingrimmer/Dropbox/CorRep/table/Descriptive.tex,  nostar nogap nomtitles booktabs replace drop(_cons republican senate) s(DV N, labels("Dependent Variable" "Observations")) label varlabels(perc_vet "Percent Veteran" perc_old "Percent 65+" med_inc "Median Income" perc_poor "Percent Poor") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at District Level.  All dependent variables are log-transformed.")

##examining determinants of overall number of letters

eststo clear

eststo: reg overall perc_vet perc_old perc_poor med_inc republican i.year if senate ==0, cluster(ia_fixed)
estadd local chamber "House" , replace
estadd local DV "Total", replace
estadd local year "\checkmark", replace


eststo: reg log_overall perc_vet perc_old perc_poor med_inc republican i.year if senate ==0, cluster(ia_fixed)
estadd local chamber "House" , replace
estadd local DV "Log(Total + 1)", replace
estadd local year "\checkmark", replace


eststo: reg overall perc_vet perc_old perc_poor med_inc republican pop_total i.year if senate ==1, cluster(ia_fixed)
estadd local chamber "Senate" , replace
estadd local DV "Total", replace
estadd local year "\checkmark", replace


eststo: reg log_overall perc_vet perc_old perc_poor med_inc republican pop_total i.year if senate ==1, cluster(ia_fixed)
estadd local chamber "Senate" , replace
estadd local DV "Log(Total + 1)", replace
estadd local year "\checkmark", replace


esttab using /Users/justingrimmer/Dropbox/CorRep/table/OverallDistrict.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(DV chamber year N, labels("Dependent Variable" "Chamber" "Year Fixed Effects" "Observations")) label varlabels(perc_vet "Percent Veteran" perc_old "Percent 65+" med_inc "Median Income" perc_poor "Percent Poor" republican "Republican" total "Population, Millions") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at District Level.")



##within the same district and within year 

xtset ia_fixed 

eststo clear 

eststo: reg log_overall republican i.year, cluster(ia_fixed)
estadd local DV "Total", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

eststo: xtreg log_overall republican i.year, fe cluster(ia_fixed)
estadd local DV "Total", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace


eststo:reg log_low republican i.year, cluster(ia_fixed)
estadd local DV "Poor", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

eststo: xtreg log_low republican i.year, fe cluster(ia_fixed)
estadd local DV "Poor", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace

gen log_business_overall = log(business_overall + 1)


eststo: reg log_business_overall republican, cluster(ia_fixed)
estadd local DV "Business", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

xtset ia_fixed
eststo: xtreg log_business_overall republican i.year , fe cluster(ia_fixed)
estadd local DV "Business", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace

esttab using /Users/justingrimmer/Dropbox/CorRep/table/RepublicanBusinessLevel.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(DV N year_fixed dist_fixed, labels("Dependent Variable" "Observations" "Year Fixed Effects" "District Fixed Effects")) label varlabels(republican "Republican") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at District Level. All dependent variables are log-transformed")


xtset ia_fixed 

eststo clear


eststo:reg log_mil republican i.year, cluster(ia_fixed)
estadd local DV "Mil", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

eststo: xtreg log_mil republican i.year, fe cluster(ia_fixed)
estadd local DV "Mil", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace

eststo:reg log_vet republican i.year, cluster(ia_fixed)
estadd local DV "Vet", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

eststo: xtreg log_vet republican i.year, fe cluster(ia_fixed)
estadd local DV "Vet", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace

eststo:reg log_sen republican i.year, cluster(ia_fixed)
estadd local DV "Senior", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "", replace

eststo: xtreg log_sen republican i.year, fe cluster(ia_fixed)
estadd local DV "Senior", replace 
estadd local year_fixed "\checkmark", replace 
estadd local dist_fixed "\checkmark", replace

esttab using /Users/justingrimmer/Dropbox/CorRep/table/RepublicanLevel.tex,  nostar nogap nomtitles booktabs replace drop(_cons) s(DV N year_fixed dist_fixed, labels("Dependent Variable" "Observations" "Year Fixed Effects" "District Fixed Effects")) label varlabels(republican "Republican") se sfmt(%9.0fc %5.2c) nonotes addnote("Robust standard errors in parentheses, clustered at District Level. All dependent variables are log-transformed")








##need to do this by agency and for Republicans


use /users/justingrimmer/Dropbox/CorRep/data/PerAgencyCount.dta

egen ia_fixed = group(state_dist)
xtset ia_fixed

gen log_piay = log(per_icpsr_year_agency + 1)

reg log_piay republican i.year, cluster(ia_fixed)
reg per_icpsr_year_agency republican i.year, cluster(ia_fixed)

##so we can now do the diff in diff


xtreg log_piay republican i.year, fe cluster(ia_fixed)
xtreg log_piay republican i.year if tenure> 2, fe cluster(ia_fixed)

xtreg per_icpsr_year_agency republican i.year, fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if tenure>2, fe cluster(ia_fixed)

egen agency_ind = group(agency)

xtreg per_icpsr_year_agency republican##agency_ind i.year, fe cluster(ia_fixed)
xtreg log_piay republican##agency_ind i.year, fe cluster(ia_fixed)


##so we see about a 5% of decline in the number of posts when republicans take over 
xtreg log_piay republican i.year if agency== "EPA", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="EPA", fe cluster(ia_fixed)

##so let's get these into some output 

xtreg log_piay republican i.year if agency== "DHHS_CMS", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="DHHS_CMS", fe cluster(ia_fixed)

xtreg log_piay republican i.year if agency== "VA", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="VA", fe cluster(ia_fixed)

xtreg log_piay republican i.year if agency== "SSA", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="SSA", fe cluster(ia_fixed)

xtreg log_piay republican i.year if agency== "DOL_VETS", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="DOL_VETS", fe cluster(ia_fixed)

xtreg log_piay republican i.year if agency== "Treasury_Fiscal", fe cluster(ia_fixed)
xtreg per_icpsr_year_agency republican i.year if agency=="Treasury_Fiscal", fe cluster(ia_fixed)



clear 
destring year, replace

egen ia_fixed = group(state_district)
egen ya_fixed = group(year)







