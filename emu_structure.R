
source("import.R")
source("keydur.R")

library(lubridate)

#' keyrates
keyrates <- c(2,5,7,10,15,30,100)

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
  pmap(~ keydur2(..1,..2,..3,..4,keyrates,..5)) -> emu_indexdata$krd

#' add krd contrib
emu_indexdata %>% mutate(wkrd = map(krd, ~ .x *wgt )) -> emu_indexdata

#' weights by country
emu_indexdata %>% group_by(Country) %>% summarize((100*sum(wgt)))

#' duration
emu_indexdata %>% summarize( sum(wgt*`Mac Dur`) )

#' duration by country
emu_indexdata %>% group_by(Country) %>% summarize( sum(wgt*`Mac Dur`) )

#' key rate duration
emu_indexdata$wkrd %>% map(sum)




# emu_indexdata %>% select(krd) # %>%  pmap(~ ..1 * ..2[1]) 