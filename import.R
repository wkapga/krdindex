#' ---
#' title: "Visualisierung JPM EMU"
#' author: "wkapga"
#' date: " 2017"
#' ---

library(tidyverse)
library(stringr)


#' ## Import GBI
#' 
#' path
path_indexfiles <- "/home/wkapga/thinclient_drives/Z:/jpm/Kursversorgung/Jpm/Done"
#' get file list 
list_indexfiles <- list.files(path = path_indexfiles,pattern="GBROAD") 

#' extract dates a la 20140505 and find position of highest (newest) date
  list_indexfiles %>% str_extract_all("\\d{8}") %>%  unlist() %>% which.max -> i
#' read this file
indexdata <- file.path(path_indexfiles,list_indexfiles[i]) %>%  read_csv(skip=1)
  