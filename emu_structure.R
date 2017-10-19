
source("import.R")

library(tidyr)
library(purrr)
library(dplyr)
library(lubridate)
library(knitr)
library(janitor)
#library(xlsx)
library(keyrateduration)
library(XLConnect)


#' path
if (R.Version()$os == "linux-gnu") {
  path_indexfiles <- "/home/wkapga/thinclient_drives/Z:/jpm/Kursversorgung/Jpm/Done"
} else {
  path_indexfiles <- "//webmethodsprod/mlw/jpm/Kursversorgung/Jpm/Done"
  path_export <- "K:/AM/GFI/Allgemein/BM/jpm/structure/"
}

indexaddcalcs <- function(indeximp){
  emu_indexdata <- indeximp[[2]]
  date_of_index <- indeximp[[1]]
  
  
  #' calc maturity dates
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  emu_indexdata <- emu_indexdata  %>% mutate( mat=as.Date(Maturity,"%d %B %Y")) 
  Sys.setlocale("LC_TIME", lct)
  
  #' calc time till maturity in years
  emu_indexdata <- emu_indexdata %>% mutate(ttm=interval(date_of_index, mat) / duration(num = 1, units = "years"))
  
  #' add wgt
  emu_indexdata <- emu_indexdata %>% mutate(wgt= (`MVal-EUR MM`/sum(`MVal-EUR MM`)) )
  
  #' add krd
  emu_indexdata %>% select(ttm,Coupon,Yield,Freq,`Mac Dur`,ISIN) %>% 
    pmap(~ keydur(keyrates,..1,..2,..3,..4,..5)) -> emu_indexdata$krd
  
  return(list(date_of_index, emu_indexdata))
}

xls_push_tibble_to_new_ws <- function(dd,wb,sheet){
  createSheet(wb, sheet)
  writeWorksheet(wb, dd,sheet)
}

index2xls <- function(indeximp,xlsfilename,keyrates, countries,maturities) {
  
  indexdata <- indeximp[[2]]
  date_of_index <- indeximp[[1]]
  
  # --- Index Calculations per bond
  #' EMU Countries only
  emu_indexdata <- indexdata %>% filter(Country %in% emu_countries)# %>% group_by(ISIN)
  emu_indexdata <- emu_indexdata %>% mutate(wgt = wgt/sum(wgt) )
  
  # --- export to xlsx
  #' open worksheet TODO: check for missing file
  wb = loadWorkbook(xlsfilename) 
  
#  createSheet(wb, "date")
#  writeWorksheet(wb, date_of_index,"date")
  xls_push_tibble_to_new_ws(date_of_index,wb,"date")
  
  #' weights by country
  createSheet(wb, "date")
    emu_indexdata %>% group_by(Country) %>% summarize(wgt = (100*sum(wgt))) %>% 
      adorn_totals() %>%
      xls_push_tibble_to_new_ws(wb,"weights")
  
  #' duration
  emu_indexdata %>% summarize( dur = sum( wgt*`Mac Dur`) )
  
  #' duration by country
  emu_indexdata %>% group_by(Country) %>% 
    summarize( dur = sum(wgt*`Mac Dur`) ) %>% adorn_totals() %>% 
    xls_push_tibble_to_new_ws(wb,"Dur_by_Country")

  #' duration by country and mat bucket
  emu_indexdata %>% group_by(Country,`Mat Sect`) %>%
    summarize( durcontrib = sum(wgt*`Mac Dur`), dur = durcontrib/sum(wgt) ) %>% 
    arrange(Country,dur) %>% 
    xls_push_tibble_to_new_ws(wb,"Dur_by_Country_and_Bucket")
  
  #' key rate duration by country
  # emu_indexdata %>% select(Country,ISIN,krd,wgt) %>% unnest() %>% 
  #    group_by(Country,kr) %>% summarize(dur = sum(val*wgt)) %>% kable(digits=2)
  
  emu_indexdata %>% select(Country,ISIN,krd,wgt) %>% unnest() %>% 
    group_by(Country,kr) %>% summarize(dur = sum(val*wgt)) %>% 
    spread(kr,dur) %>% adorn_totals()  %>% 
    xls_push_tibble_to_new_ws(wb,"KeyDur_by_Country")
  
    
  saveWorkbook(wb)
  
}


#' keyrates
keyrates <- c(2,5,7,10,15,30,100)
#' EMU Countries
emu_countries <- c("AT","BE","FR","FI","DE","NL","IT","ES","IE","PT","GR")

#' read index from network drive if not in environment (helps w/ debug)
if ( ("indeximp" %in% ls() ) == FALSE) {
  indeximp <- import_current_index(path_indexfiles) %>% indexaddcalcs()
}


#' generate report to xlsx
index2xls(indeximp,paste0(path_export,"EMU.xlsx"),keyrates,emu_countries)


