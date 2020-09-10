##API Key 
##6d281e6a38988243820d745f43d765b7623bc280


library(tidycensus)
census_api_key('6d281e6a38988243820d745f43d765b7623bc280')

data <- get_acs(geography = "congressional district", variables = 'B09010', year = 2009)

v09<- load_variables(2009, 'acs5', cache=T)

##2009 variables

##B02001 Race
##B08303 Travel time to work
##B09010 Receipt of SSI, PUblic assistance income, Food Stamps
##B11001 Household Type
##B19001 Household income
##B19013 Median household income
##B19056 SSI for households
##B19058 Public Assistance Income or Food Stamps/SNAP in the Past 12 Months f
##B25081 Mortgage status
##B01001 Sex by Age
##


v16<- load_variables(2016, 'acs5', cache=T)

store<- unique(v16[,3])
for(z in 1:nrow(store)){
		print(v16[which(v16[,3]==as.character(store[z,1]))[1],])
		readline('wait')

}

##B01001_001 Sex by Age 
##B02001_001
