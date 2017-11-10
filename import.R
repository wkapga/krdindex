#' ---
#' title: "Visualisierung JPM EMU"
#' author: "wkapga"
#' date: " 2017"
#' ---

library(purrr)
library(readr)
library(stringr)


#' ## Import latest GBI
#' 


import_current_index <- function(path_indexfiles) {
  #' get file list 
  list_indexfiles <- list.files(path = path_indexfiles,pattern="GBROAD") 

  #' extract dates a la 20140505 and find position of highest (newest) date
  i <-  list_indexfiles %>% str_extract_all("\\d{8}") %>%  unlist() %>% which.max 

  #' read this file
  indexdata <- 
    file.path(path_indexfiles,list_indexfiles[i]) %>%  read_csv(skip=1)

  indexdata <- indexdata %>% filter( !is.na(Price) ) # get rid of disclaimer
  
  #' get date of index from 1st row of file
  date_of_index <- file.path(path_indexfiles,list_indexfiles[i]) %>% 
      read.csv(nrows=1,header=FALSE) %>%   paste(.) %>% tail(1) %>% as.Date("%Y%m%d")

  return(list(date_of_index, indexdata) )
}

import_current_index_iboxx <- function(path_indexfiles) {
  #' get file list 
  list_indexfiles <- list.files(path = path_indexfiles,pattern="iboxx_eur_eod_underlyings") 
  
  #' extract dates a la 20140505 and find position of highest (newest) date
  i <-  list_indexfiles %>% str_extract_all("\\d{8}") %>%  unlist() %>% which.max 
  
  #' read this file
  indexdata <- 
    file.path(path_indexfiles,list_indexfiles[i]) %>%  read_csv
  
  date_of_index <- indexdata$Date %>% max 
  
  return(list(date_of_index, indexdata) )
}


