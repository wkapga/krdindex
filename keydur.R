# KRD Functions

wg <- function (keyrates,tt) {
  # helper function for key rate duration
  lo_in = max(which(keyrates <= tt)) # find keyrate lower than t
  w = rep(0,length(keyrates))
  w[lo_in] = ( keyrates[lo_in+1] -tt ) /(keyrates[lo_in+1] - keyrates[lo_in]) # assign wgt to lower kr
  w[lo_in+1] = 1 - w[lo_in]  # rest to higher kr
  return(w)
}


wg2 <- function (keyrates,tt) {
  # helper function for key rate duration
  # find index of first keyrate > tt
  i <- keyrates %>% detect_index(~ .x > tt)
  #spr <- keyrates[i] - keyrates[i-1]
  # list of zeros length of keyrates
  w <- map_dbl(keyrates, ~ 0)
  
  w <- modify_at(w,i, ~.x + (tt-keyrates[i-1] )/ (keyrates[i] - keyrates[i-1]) )
  w <- modify_at(w,i-1, ~.x + (keyrates[i]-tt )/ (keyrates[i] - keyrates[i-1]) )
  return(unlist(w))
  }


keydur <- function(ttm,coupon,yield,freq,keyrates,targetdur) {
  # function to calculate vector of key rate duarations for given keyrates
  # coupon, yield in percent, target duration is optional
  keyrates <- c(0, keyrates , 10e3) # add keyrate at 0 and far in future
  tt <- seq((ttm*freq - floor(ttm*freq))/freq, ttm, by = 1/freq ) # vector of occurence of cashflow in time
  cf <- rep(coupon/freq/100, length(tt)) + c( rep(0, length(tt)-1), 1) # cash flows
  
  dcf <- cf * (1+yield/100)^(-tt) # discounted cash flows
  vec <- dcf * tt / sum(dcf) 
  
  # duration would be sum(vev)
  kw <- sapply(tt, function(x) wg(keyrates,x)) # get matrix of weights
  k <- kw %*% vec # matrix multiplication
  # add keyrate at zero to first keyrate and add keyrate far in future to last keyrate
  k <- c(sum(head(k,2)), k[3:(length(k)-2)], sum(tail(k,2)) )
  
  # use stated duration if given
  if ( ! missing(targetdur) ) { k <- k * targetdur/sum(k)  }
  return(k)
} 


keydur2 <- function(ttm,coupon,yield,freq,keyrates,targetdur) {
  # function to calculate vector of key rate duarations for given keyrates
  # coupon, yield in percent, target duration is optional
  keyrates_expanded <- c(0, keyrates , 10e3) # add keyrate at 0 and far in future
  tt <- seq((ttm*freq - floor(ttm*freq))/freq, ttm, by = 1/freq ) # vector of occurence of cashflow in time
  cf <- rep(coupon/freq/100, length(tt)) + c( rep(0, length(tt)-1), 1) # cash flows
  
  dcf <- cf * (1+yield/100)^(-tt) # discounted cash flows
  vec <- dcf * tt / sum(dcf) # duration per cash flow, total duration would be sum(vec)
  
  kw <- map_dfc(tt, ~ as.tibble(wg2(keyrates_expanded,.x))) # get matrix of weights
  k <- as.matrix(kw) %*% as.matrix(vec) %>% as.tibble %>%  flatten_dbl() # matrix multiplication
  
  # add keyrate at zero to first keyrate and add keyrate far in future to last keyrate
  k <- c(sum(head(k,2)), k[3:(length(k)-2)], sum(tail(k,2)) )
  
  # use stated duration if given
  if ( ! missing(targetdur) ) { k <- k * targetdur/sum(k)  }

  return(tibble( kr = keyrates, val=k))
} 



