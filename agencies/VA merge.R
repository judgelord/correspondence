
files <- str_c("VA/",list.files("VA") )

xls.files <- files[str_detect(files, ".xlsx")]
csv.files <- files[str_detect(files, ".csv")]


# FUNCTION INSPECT NAMES
readva <- function(file){
  names <- readxl::read_xlsx(file) %>% names()
  # make the same length
  length(names) <- 30
  return(names)} 

xls <- map_dfc(xls.files, readva) %>% 
  as.matrix() %>% 
  t() %>% 
  as.tibble()

# FUNCTION INSPECT NAMES
readva <- function(file){
  names <- read_csv(file) %>% names()
  # make the same length
  length(names) <- 30
  return(names)} 
csv <- map_dfc(csv.files, readva) %>% 
  as.matrix() %>% 
  t() %>% 
  as.tibble()



################################
## DATA #
#########

## SENATE 

## 2016
library(readxl)

s16 <- read_excel("VA/2016 Senate Mail Distribution Log.xlsx")%>%  #one sheet
    mutate_all(as.character) %>% 
  mutate(DATE = as.Date(DATE))
    
# 2017-2019 are broken out by month
read_excel_sheets <- function(sheet){
  sheet %<>% 
  read_excel(sheet = ., path = path) %>% 
  mutate_all(as.character) %>% 
  .[,1:17]
  
  names(sheet) <- names[1:17]
  return(sheet)
}

## 2017
path <- "VA/2017 Senate Mail Distribution Log.xlsx"

names <- read_excel(sheet = "Dec", path = path) %>% names()

s17  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
    mutate_all(as.character) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  distinct()

## 2018
path <- "VA/2018 Senate Mail Distribution Log.xlsx"# monthly sheets

names <- read_excel(sheet = "Dec", path = path) %>% names()

s18  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  mutate_all(as.character) %>% 
  mutate(DATE = as.Date(DATE))%>% 
  distinct()
        
## 2019
path <- "VA/2019 Senate Mail Distribution Log.xlsx"# monthly sheets

names <- read_excel(sheet = "Mar", path = path) %>% names()

s19  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  mutate_all(as.character) %>% 
  mutate(DATE = as.Date(DATE))%>% 
  distinct()

# 2015
s15 <- read_excel("VA/Senate Mail Distribution Log 2015.xlsx") %>% 
  rename(DATE = `Date Letter Rec'd`,
         FROM = `Member    (Last Name, Initial)`,
         Constituent = `Constituent   (Last Name, First name)`,
         SUBJECT = Subject,
         `Date Inquiry Assigned` = `Date Letter Assign'd`) %>% 
  mutate_all(as.character) %>% 
  mutate(DATE = as.Date(DATE))%>% 
  .[,1:11] %>% 
  distinct()


# 2008-2014
read_excel_sheets <- function(sheet){
  d <- sheet %>% 
    read_excel(sheet = ., 
               path = path, 
               range = cell_cols("A:G"),
               col_names = names[1:7],
               col_types = c("date", "date", 
                             "text","text", "text","text", "text"))
  
  names(d) <- names[1:7]
  
  d %<>% 
    mutate(DATE = as.Date(DATE, "%m/%d/%Y")) %>% 
    rename(assigned = `Date Letter Assign'd`) %>% 
    fill(DATE)
    # FIXME
    # FILLING DATES DOWN, BUT THERE IS TEXT IN THE WAY THAT WILL CAUSE DATE ERRORS!
    # SEE 2010 sheet, "SecVA ltr" in the date column
    
  return(d)
}

path <- "VA/Senate Mail Distribution Log 2008-2014.xlsx" # monthly sheets

names <- read_excel(sheet = "2014", path = path) %>% names()

read_excel_sheets("2009")

s0814  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  distinct()




senate <- full_join(s19, s18)%>% 
  full_join(s17) %>% 
  full_join(s16) %>% 
  full_join(s15) %>% 
  full_join(s0814) %>% 
  mutate(chamber = "Senate") %>% 
  select(DATE, FROM, chamber, SUBJECT, Constituent, everything())

# FIXME 
# Potentially interesting info in these extra columns being dropped
senate %<>% select(-starts_with("..."), -Date, -date,-asof)



###############
## House # 
###############
# 2008-2019
read_csvs <- function(path){
  d <- path %>% 
    read_csv() %>%#col_types = c("date", "date",  "text","text", "text","text", "text")) 
    .[,1:7]
  
  d %<>% 
    mutate(DATE = as.Date(DATE, "%m/%d/%y")) %>% 
    fill(DATE) %>% 
    distinct()
  # FIXME
  # FILLING DATES DOWN

  return(d)
}

read_csvs(csv.files[12])

csv.files

house  <-  csv.files %>% #monthly sheets
  purrr::map_dfr(read_csvs) %>% 
  distinct() %>% 
  mutate(chamber = "House")

# interesting nots in X7
# MERGE WITH OTHER NOTE FIELDS
# house %<>% select(-X7)

VA <- full_join(senate, house) %>% 
  arrange(DATE)

write_csv(full_join(senate, house), path = "VA.csv")
