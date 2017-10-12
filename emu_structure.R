
source("import.R")

library(tidyr)
library(purrr)
library(dplyr)
library(lubridate)
library(knitr)
library(janitor)
library(xlsx)
library(keyrateduration)


#' keyrates
keyrates <- c(2,5,7,10,15,30,100)
#' path
if (R.Version()$os == "linux-gnu") {
  path_indexfiles <- "/home/wkapga/thinclient_drives/Z:/jpm/Kursversorgung/Jpm/Done"
} else {
  path_indexfiles <- "//webmethodsprod/mlw/jpm/Kursversorgung/Jpm/Done"
}


indeximp <- import_current_index(path_indexfiles)

indexdata <- indeximp[[2]]
date_of_index <- indeximp[[1]]

#' EMU Countries
emu_countries <- c("AT","BE","FR","FI","DE","NL","IT","ES","IE","PT","GR")
#' EMU Countries only
emu_indexdata <- indexdata %>% filter(Country %in% emu_countries)# %>% group_by(ISIN)

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

#' add krd contrib
# emu_indexdata %>% mutate(wkrd = map(krd, ~ .x *wgt )) -> emu_indexdata

#' weights by country
emu_indexdata %>% group_by(Country) %>% summarize((100*sum(wgt)))

#' duration
emu_indexdata %>% summarize( dur = sum( wgt*`Mac Dur`) )

#' duration by country
emu_indexdata %>% group_by(Country) %>% summarize( dur = sum(wgt*`Mac Dur`) )

#' key rate duration by country
# emu_indexdata %>% select(Country,ISIN,krd,wgt) %>% unnest() %>% 
#    group_by(Country,kr) %>% summarize(dur = sum(val*wgt)) %>% kable(digits=2)

emu_indexdata %>% select(Country,ISIN,krd,wgt) %>% unnest() %>% 
  group_by(Country,kr) %>% summarize(dur = sum(val*wgt)) %>% 
  spread(kr,dur) %>% adorn_totals() -> aa

aa %>% kable(digits=2)


write_excel_csv(aa,"/home/wkapga/thinclient_drives/G:/RSI/wkapga/tmp_krdindex1.csv")
