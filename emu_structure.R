
source("import.R")
source("keydur.R")

library(lubridate)

#' keyrates
keyrates <- c(2,5,7,10,15,30,100)

#' EMU Countries
emu_countries <- c("AT","BE","FR","FI","DE","NL","IT","ES","IE","PT","GR")
#' EMU Countries only
emu_indexdata <- indexdata %>% filter(Country %in% emu_countries)

#' calc maturity dates
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
emu_indexdata <- emu_indexdata  %>% mutate( mat=as.Date(Maturity,"%d %B %Y")) 
Sys.setlocale("LC_TIME", lct)

#' calc time till maturity in years
emu_indexdata <- emu_indexdata %>% mutate(ttm=interval(date_of_index, mat) / duration(num = 1, units = "years"))

#' add wgt
emu_indexdata <- emu_indexdata %>% mutate(wgt=`MVal-EUR MM`/sum(`MVal-EUR MM`))

#ttm,coupon,yield,freq,keyrates,targetdur
emu_indexdata %>% select(ttm,Coupon,Yield,Freq,`Mac Dur`) %>% 
  pmap(~ keydur(..1,..2,..3,..4,keyrates,..5)) %>% tibble -> emu_indexdata$krd

 
# pmap_dbl(emu_indexdata$wgt,emu_indexdata)
