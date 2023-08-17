

# read in data 
d <- read_csv("fda.csv")


# identify leading rows that we need to combine for each observation 
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/lead-lag

# combine dates
d %<>% mutate(
  DATE = ifelse(
    # if the next row's id is blank, we need to combine
    id != "" & lead(id,1) == "", # check whether this is blank or NA
    # then combine DATE cell with one below 
    DATE %>% str_c(lead(DATE, 1)),
    # otherwise, leave as is
    DATE 
    ) 
  ) 

# combine SUBJECT 
d %<>% mutate(
  SUBJECT = ifelse(
    # if the next row's id is blank, we need to combine
    id != "" & lead(id,1) == "", # check whether this is blank or NA
    # then combine SUBJECT cell with one below 
    SUBJECT %>% str_c(lead(SUBJECT, 1)),
    # otherwise, leave as is
    SUBJECT 
  ) 
) 


# Copy and edit the code above to repeat the above for "ToOrgAbb", "statusdate", and any other columns


# Check to make sure it work
head(d)


# Then, delete the row you just pasted in 

d %<>% filter(!(id != "" & lag(id, 1) == "")) # or !is.na() if you imported blanks as NAs

# then repeat until you paste in all of the rows

# then save so you can copy and paste it into the "FDA Rochelle" google sheet 

write_csv("fda_clean.csv")



