
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

csv <- map_dfc(csv.files, readva) %>% 
  as.matrix() %>% 
  t() %>% 
  as.tibble()



################################
## DATA #
#########

## SENATE 

## 2016
s16 <- read_excel("VA/2016 Senate Mail Distribution Log.xlsx")%>%  #one sheet
    mutate_all(as.character) 
    

read_excel_sheets <- function(sheet){
  sheet %<>% 
  read_excel(sheet = ., path = path) %>% 
  mutate_all(as.character) %>% 
  .[,1:20]
  
  names(sheet) <- names[1:20]
  return(sheet)
}

## 2017
path <- here("VA/2017 Senate Mail Distribution Log.xlsx")

names <- read_excel(sheet = "Dec", path = path) %>% names()

s17  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
    mutate_all(as.character) %>% 
  distinct()

## 2018
path <- "VA/2018 Senate Mail Distribution Log.xlsx"# monthly sheets

names <- read_excel(sheet = "Dec", path = path) %>% names()

s18  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  mutate_all(as.character) %>% 
  distinct()
        
## 2019
path <- "VA/2019 Senate Mail Distribution Log.xlsx"# monthly sheets

names <- read_excel(sheet = "Mar", path = path) %>% names()

s19  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  #purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  mutate_all(as.character) %>% 
  distinct()

# 2015
s15 <- read_excel("VA/Senate Mail Distribution Log 2015.xlsx")


# 2008-2014
read_excel_sheets <- function(sheet){
  d <- sheet %>% 
    read_excel(sheet = ., 
               path = path, 
               range = cell_cols("A:G"),
               col_names = names[1:7],
               col_types = c("date", "date", 
                             "text","text", "text","text", "text")) %>% 
    #mutate_all(as.character) %>% 
    .[,1:7] 
  
  names(d) <- names[1:7]
  
  d %<>% mutate(year = sheet)%>% 
    mutate(DATE = as.Date(DATE, "%m/%d/%Y")) %>% 
    fill(DATE)
    # FIXME
    # FILLING DATES DOWN, BUT THERE IS TEXT IN THE WAY THAT WILL CAUSE DATE ERRORS!
    # SEE 2010 sheet, "SecVA ltr" in the date column
    
  return(d)
}

path <- "VA/Senate Mail Distribution Log 2008-2014.xlsx" # monthly sheets

names <- read_excel(sheet = "2014", path = path) %>% names()

read_excel_sheets("2008")

s0814  <-  path %>% #monthly sheets
  excel_sheets() %>% 
  # purrr::set_names() %>% 
  purrr::map_dfr(read_excel_sheets) %>% 
  mutate_all(as.character) %>% 
  distinct()




senate <- full_join(s19, s18)%>% 
  full_join(s17) %>% 
  full_join(s16) %>% 
  full_join(s15) %>% 
  full_join(s0814) %>% 
  mutate(chamber = "Senate")

house <- full_join(
  read_csv("VA/VA House 2008.csv")%>% mutate_all(as.character),
  read_csv("VA/VA House 2009.csv") %>% mutate_all(as.character)
) %>% 
  full_join(
    read_csv("VA/VA House 2010.csv") %>% mutate_all(as.character)
) %>% 
  full_join(
    read_csv("VA/VA House 2010.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2012.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2013.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2014.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2015.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2016.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2017.csv") %>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2018.csv")%>% mutate_all(as.character)
  ) %>% 
  full_join(
    read_csv("VA/VA House 2019.csv") %>% mutate_all(as.character)
  ) %>% 
  mutate(chamber = "House")
