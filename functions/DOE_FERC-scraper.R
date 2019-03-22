source("setup.R")
library(rvest)

# https://elibrary.ferc.gov/idmws/search/fercgensearch.asp


# extract table 
web_pages <- list.files(here("FERC", "html"), pattern = ".htm")
web_pages <- web_pages[1]

scraper <- function(web_page){
  
  # Get raw html
  html <- read_html(here("FERC", "html", web_page))

table <- html %>% 
  html_nodes("table") %>%
  .[[3]] %>% # I happen to be interested in the third table on this page
  html_table(fill = T) %>% # turn html in to a data frame
  rename(id = X1,
         date = X2,
         docket = X3,
         summary = X4) %>% 
  filter(str_detect(id, "Submittal")) %>% # clean it up a bit
  gather(key = key, value = link_text, -id, -date, -docket, -summary) %>% 
  #drop_na(link_text) %>% 
  filter(link_text %in% c("Image", "FERC Generated PDF") ) %>% #FIXME
  # group_by(link_text, id) %>% top_n(1) %>% ungroup() %>%
  separate(date, c("doc_date", "filed_date"), sep = "\n\t") %>%
  mutate(doc_date = as.Date(doc_date, "%m/%d/%Y"),
         filed_date = as.Date(filed_date, "%m/%d/%Y") ) %>%
  # arrange(desc(link_text)) %>% # image before pdf
  arrange(id) %>% # id numbers count up
  arrange(desc(filed_date)) %>% # dates count down 
  mutate(id = str_remove(id, "Submittal")) %>% 
  mutate(id = str_replace(id, "Document Components", "(partial)")) %>% 
  mutate(index = row_number()) # add an index


urls <- html %>% 
  html_nodes("table") %>%
  html_nodes("a") # "a" nodes contain url linked text

urls <- tibble(
  link_text = html_text(urls), # the linked text 
  url = html_attr(urls, "href") ) %>% # the url (an html attribute)
  mutate(fileID = str_extract(url, "[0-9].*"), # the url contains the file name, but in this case, not the extension
         file_extention = str_replace(link_text, ".*PDF", ".pdf"), # but the linked text tells us the file type
         file_extention = str_replace(file_extention, "Image", ".tif") ) %>%
  filter(str_detect(url, "opennat")) %>%  # filter to get rows that have the files we want
  filter(link_text %in% c("Image","FERC Generated PDF") ) %>% # filter to pdf files
  mutate(index = row_number()) # add index 

  # merge with table by index
  d <- full_join(table, urls)  %>%
    # add the file name and file extension
    mutate(file_name = paste0(id, "-", fileID, file_extention) ) 

  # filter out files we already have
  download <- d %>% 
    filter(!file_name %in% list.files(here("FERC")) ) 
    
  # Now we can use the function download.file(url, destfile)
  # walk2() takes two vectors, .x and .y, and applies the function .f(.x, .y)
  # Here, .x is url, .y is destfile, and .f is download.file():
  walk2(download$url, here("FERC", download$file_name), download.file)


  return(d %>% 
           select(id, doc_date, filed_date, docket, summary, file_name, url) %>% 
           filter(!is.na(id)) # drop errors #FIXME
         )
}
## map_dfr() takes a vector, .x and applies the function .f(.x), 
## binding the results as rows in a data frame
tables <- map_dfr(web_pages, scraper)

log <- d %>% 
  select(-url) %>% 
  spread(d, link_text, file_name)
###################################################################








 ##########################################
# download all files 
scraper <- function(web_page){

  # Get raw html
  html <- read_html(here("FERC", web_page))
  
  # A data frame with rows breaks at the node "<tr>" 
  d <- tibble(table_rows = html_nodes(html,"tr"))
  
  d %<>% 
    mutate(links = html_node(table_rows,"a"), # "a" nodes contain urls and linked text
           link_text = html_text(links), # the text 
           url = html_attr(links, "href"), # the url
           fileID = str_extract(url, "[0-9].*"), # the url contains the file name, but in this case, not the extension
           file_extention = str_replace(link_text, ".*PDF", ".pdf"), # but the linked text tells us the file type
           file_extention = str_replace(file_extention, "Image", ".tif"), 
           file_name = paste0(fileID, file_extention), # add the file name and file extension
           summary = html_text(table_rows)) %>% # grab all the table text just because
    select(-links, -table_rows) %>%
    filter(str_detect(url, "opennat")) %>%  # filter out rows that don't have the files we want
    filter(file_extention == ".pdf")

  d %<>% filter(!file_name %in% list.files(here("FERC")) ) # filter out files we already have
  
  # Now we can use the function download.file(url, destfile)
  # walk2() takes two vectors, .x and .y, and applies the function .f(.x, .y)
  # Here, .x is url, .y is destfile, and .f is download.file():
  walk2(d$url, here("FERC", d$file_name), download.file)
}

# walk() takes one vector, .x and applies the function .f(.x)
walk(web_pages, scraper)





##################################
##### OLD CODE: 

# Get html
FERChtml <- read.csv("FERChtml.html", header = F)

# Select pdf files
FERCfiles <- str_match(FERChtml$V1, 'opennat.asp.fileID=[0-9]*. >FERC')
FERCfiles <- FERCfiles[which(!is.na(FERCfiles))]
FERCfiles <- gsub(". >FERC", "", FERCfiles)

# Complete urls
for(i in 1:length(FERCfiles)){
  FERCfiles[i] <- paste0("https://elibrary.ferc.gov/idmws/common/", FERCfiles[i])
}

# Download 
for (myurl in FERCfiles) {
  filename <- paste(str_match(myurl, "fileID=(.+)")[2], ".pdf", sep="")
  #download(myurl, filename) # COMMENT OUT TO AVOID ACCIDENTAL DOWNLOAD
  Sys.sleep(2)
}

# get both pdf document and log ID numbers from html 
FERCid <- str_match(FERChtml$V1, "opennat.asp.fileID=[0-9]*.....fldrslt .cf3 .ul .ulc3 .strokec3 FERC|[0-9]{8}-[0-9]{4}")
FERCid <- FERCid[which(!is.na(FERCid))]
FERCid <- gsub("opennat.asp.fileID=|....fldrslt.*", "", FERCid)

# match ID numbers 
FERCid <- as.data.frame(FERCid)
FERCid$id <- NA # new var for doc id numbers 
for(i in 1:dim(FERCid)[1]){
  if(grepl("-", FERCid$FERCid[i]) && !grepl("-",FERCid$FERCid[i+1])){
    FERCid$id[i] <- FERCid$FERCid[i+1]
  }
}
head(FERCid)
tail(FERCid)
FERCid <- FERCid[which(grepl("-", FERCid$FERCid)),]
FERCid <- unique(FERCid)


# read in and format log file 
FERClog <- read.csv("FERClog.csv")
names(FERClog) <- c("X", "FERCid", "Date", "Docket", "Summary", "Type", "Size", "X.1")
FERClog$FERCid <- str_match(FERClog$FERCid, "[0-9]{8}.[0-9]{4}")

# merge log file with IDs from html
FERClog <- merge(FERClog, FERCid, by = "FERCid")


# pdf letters to text (note: could also OCR images, see html for image doc ids)
FERClog$LetterText <-NA

totext <- function(id){
  text <- pdf_text(paste0(id,".pdf"))
  text <- gsub("  |\n", "", paste(text[1], text[2]))
  return(text)
}

for(i in 1:dim(FERClog)[1]){
  if(!grepl("-", FERClog$id[i])){
    tryCatch({
      FERClog$LetterText[i] <- totext(FERClog$id[i])
    },
    error=function(cond) {
      message(paste(i, "error:"))
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      message(paste(i, "warning:"))
      message(cond)
      return(NULL)
    },
    finally={
      message(paste("Processed", i))
    })
  }}

# examine
head(FERClog$LetterText)
tail(FERClog)

class(FERClog$LetterText)

# save log + letters
write.csv(FERClog, "FERClogandLetters.csv")