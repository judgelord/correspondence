here::here(source("setup.R"))

# install.packages('downloader','stringr', 'pdftools', 'tesseract')
library(tesseract)
library(downloader)
library(pdftools)
library(rvest)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

cast <- lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()
cast

poster <- lego_movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster


# https://elibrary.ferc.gov/idmws/search/fercgensearch.asp

# Get html
FERChtml <- read_html(here("FERC","1.html"))




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