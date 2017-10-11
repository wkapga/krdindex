#' ---
#' title: "Visualisierung JPM EMU"
#' author: "wkapga"
#' date: " 2017"
#' ---

library(tidyverse)
library(stringr)


#' ## Import latest GBI
#' 


import_current_index <- function(path_indexfiles) {
  list_indexfiles <- list.files(path = path_indexfiles,pattern="GBROAD") 

  #' extract dates a la 20140505 and find position of highest (newest) date
  i <-  list_indexfiles %>% str_extract_all("\\d{8}") %>%  unlist() %>% which.max -> i

  #' read this file
  indexdata <- 
    file.path(path_indexfiles,list_indexfiles[i]) %>%  read_csv(skip=1)

  #' get date of index from 1st row of file
  date_of_index <- file.path(path_indexfiles,list_indexfiles[i]) %>% 
      read.csv(nrows=1,header=FALSE) %>%   paste(.) %>% tail(1) %>% as.Date("%Y%m%d")

  return(date_of_index, indexdata )
}
