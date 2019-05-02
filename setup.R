  options(stringsAsFactors = FALSE)
  
  requires <- c("gmailr", "dplyr", "ggplot2", "gdata", "magrittr","googlesheets","googledrive","devtools","stringi","stringr", "tidyverse",
                "pdftools", "here", "Rvoteview", "rvest")
  to_install <- c(requires %in% rownames(installed.packages()) == FALSE)
  install.packages(c(requires[to_install], "NA"), repos = "https://cloud.r-project.org/" )
  
  library(devtools)
  if(!"Rvoteview" %in% rownames(installed.packages())) {
    devtools::install_github("voteview/Rvoteview")
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
  library(googledrive)
  library(Rvoteview)
  library(stringi)
  library(pdftools)
  library(here)
  library(rvest)
  
  source(here("functions/clean.R")) # data cleaning and intercoder agreement functions 
  source(here("functions/stateFromLower.R")) # format state names
  source(here("functions/dateMethods.R"))
  source(here("functions/nameMethods.R")) # functions for cleaning member names to match the augmented member file
  
  source(here("members/nameCongress.R")) # augments voteview member names
  source(here("members/MemberNameDateCorrections.R"))
  
  # FIXME
  # source(here("committees/committees.R"))
  
  knitr::opts_chunk$set(echo = TRUE, # echo = TRUE means that your code will show
                        warning = FALSE,
                        message = FALSE,
                        fig.align = "center", 
                        fig.path= 'Figs/', ## where to save figures
                        fig.height = 3,
                        fig.width = 3)
  
# gs_ls() # log in to google

