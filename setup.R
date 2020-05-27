  options(stringsAsFactors = FALSE)
  
  requires <- c("gmailr", "dplyr", "ggplot2", "gdata", "magrittr","googlesheets4","googledrive","devtools","stringi","stringr", "tidyverse",
                "pdftools", "here", "rvest","maps", "ineq", "mapproj", "dotwhisker", "crayon")
  to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
  install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
  
  library(devtools)
  if(!"fiftystater" %in% rownames(installed.packages())) {
    devtools::install_github("wmurphyrd/fiftystater")
  }
  
  
  
  library(tidyverse)
  library(dplyr) # in case tydyverse fails (problem on linux)
  library(ggplot2); theme_set(theme_bw())
  options(
    ggplot2.continuous.color = "viridis",
    ggplot2.continuous.fill = "viridis"
  )
  scale_color_discrete <- function(...)
    scale_color_viridis_d(...)
  scale_fill_discrete <- function(...)
    scale_fill_viridis_d(...)
  library(magrittr)
  library(googlesheets)
  # modified gs_read function to assume char typ
  gs_read_chr <- function(title){
    data <- gs_read(title, 
                    FROM = col_character(), 
                    DATE = col_character(),
                    SUBJECT = col_character(), 
                    TYPE = col_character(),
                    POLICY_EVENT = col_character(),
                    NOTES = col_character(),
                    ERROR = col_character())
  }
  library(googledrive)
  library(gmailr)
  library(stringi)
  # library(pdftools)
  library(here)
  library(rvest)
  library(tidyr)
  library(ineq)
  # library(stargazer)
  library(maps)
  library(fiftystater)
  library(mapproj)
  library(knitr)
  library(broom)
  library(dotwhisker)
  library(crayon)
  library(tidyverse)
  
  source(here("functions/clean.R")) # data cleaning and intercoder agreement functions 
  source(here("functions/stateFromLower.R")) # format state names
  source(here("functions/dateMethods.R"))
  source(here("functions/nameMethods.R")) # functions for cleaning member names to match the augmented member file
  
  #source(here("members/nameCongress.R")) # augments voteview member names
  ## Load augmented member names without having to load voteview package
  load(here("members/members.Rdata"))
  
  ## Load committee data
  load(here("data/committees.Rdata"))
  
  ## Load typos and date corrections
  source(here("members/MemberNameTypos.R"))
  source(here("members/MemberNameDateCorrections.R"))

  
  knitr::opts_chunk$set(echo = TRUE, # echo = TRUE means that your code will show
                        warning = FALSE,
                        message = FALSE,
                        fig.align = "center", 
                        fig.path= 'Figs/', ## where to save figures
                        fig.height = 3,
                        fig.width = 3)
  
  data_complete <- function(){
    if( length(data_list$agency) != length(unique(df$agency)) ){
      base::message("missing from d:", paste(data_list$agency[!data_list$agency %in% unique(d$agency)], collapse = ", "))
      
      base::message("missing from df:", paste(data_list$agency[!data_list$agency %in% unique(df$agency)], collapse = ", "))
      
      base::message(paste("merge.R stopped at", stopped))
    }
    
    if(length(data_list$agency) == length(unique(df$agency))){
      base::message(green("Merge complete"))
    }
  }

  
  
## Since clean scripts were written for the googlesheets packaged, which has been replaced by googlesheets4
  gs_read <-   function(sheet){sheet %>% googlesheets4::read_sheet(col_types = "c")}
  
  gs_title <- function(file.name){googledrive::drive_get(file.name)}