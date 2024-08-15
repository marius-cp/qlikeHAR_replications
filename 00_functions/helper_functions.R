paneldmw <- function(pdat, ml = NULL) {
  # Convert the data frame to a panel data frame
  #pdat <- plm::pdata.frame(dif, index = sym)
  # Fit the pooled model
  pmod <- plm::plm(dif ~ 1, data = pdat, model = "pooling")
  # Calculate Newey-West adjusted standard errors
  vcov_nw <- plm::vcovNW(pmod, maxlag = ml)
  # Calculate the result
  result <- coef(pmod) / sqrt(vcov_nw)
  
  # Return the result as a numeric vector
  return(as.numeric(result))
}

compute_statistics <- function(symbol_data) {
  # Calculate qlike_loss_diff
  qlike_loss_diff <- symbol_data$lossdifqlike
  dmqlike <- lm(qlike_loss_diff ~ 1)
  qlike_stats <- lmtest::coeftest(dmqlike, vcov = vcovHAC(dmqlike))
  
  # Calculate mse_loss_diff
  mse_loss_diff <- symbol_data$lossdiffmse
  dmmse <- lm(mse_loss_diff ~ 1)
  mse_stats <- lmtest::coeftest(dmmse, vcov = sandwich::vcovHAC(dmmse))
  
  # Return results
  list(
    symbol = symbol_data$sym[1],
    win = symbol_data$win[1],
    horizon = symbol_data$horizon[1],
    model = symbol_data$model[1],
    qlike_statistic = qlike_stats[1, "t value"],
    qlike_p_value = qlike_stats[1, "Pr(>|t|)"],
    mse_statistic = mse_stats[1, "t value"],
    mse_p_value = mse_stats[1, "Pr(>|t|)"]
  )
}




do_predictions <- function(rv, rq, good, bad, win = 1000) {
  N <- length(rv)
  horizon <- (win+1):N
  out <- foreach(
    t=(win+1):N, 
    .errorhandling = 'pass', 
    .packages = c("dplyr", "tibble", "RcppRoll", "xts")
  ) %dopar% {
    
    #print(paste0(t, ' / ', N))
    
    source("../00_functions/qlikeHAR.R")
    
    
    S =  function(x, y) log(x)+y/x
    
    # Store data of the current step
    rv_ <- as.matrix(rv[(t-win):(t-1),]) %>% sqrt() #!!! reg in volas!!
    rq_ <- as.matrix(rq[(t-win):(t-1),])
    good_ <- as.matrix(good[(t-win):(t-1),])
    bad_ <- as.matrix(bad[(t-win):(t-1),])
    d_ <- index(rv)[t]
    xf <- as.numeric(rv[t]) %>% sqrt()
    
    rvw <-  RcppRoll::roll_meanr(rv_, n=5, fill=NA) 
    xf_w <- RcppRoll::roll_meanr(rv %>% sqrt(), n=5, fill=NA)[t] 
    rvm <-  RcppRoll::roll_meanr(rv_, n=22, fill=NA)
    xf_m <- RcppRoll::roll_meanr(rv %>% sqrt(), n=22, fill=NA)[t] 
    
    
    # HAR ======================================================================
    data_har <- data.frame(
      #date= ymd(aapl$date),
      rv = rv_, 
      rvw = rvw, 
      rvm = rvm
    ) #%>% tidyr::drop_na()
    data_har
    
    ## day ============================
    mod_har <- qlikeHAR(X=data_har[1:(win-1),], y=data_har[2:win,1], S=S)
    har_1da <- # one day ahead HAR
      tibble(
        date=d_, 
        realization = xf, 
        qlikeHAR = mod_har$coef %*% (c(1, tail(data_har, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_har$ols_model %*% (c(1, tail(data_har, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HAR", 
        horizon = "1day"
      )
    
    ## week ============================
    mod_har_w <- qlikeHAR(X=data_har[1:(win-1),], y=data_har[2:win,2], S=S)
    
    har_1wa <- # one week ahead HAR
      tibble(
        date=d_, 
        realization = xf_w, 
        qlikeHAR = mod_har_w$coef %*% (c(1, tail(data_har, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_har_w$ols_model %*% (c(1, tail(data_har, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HAR", 
        horizon = "1week"
      )
    
    ## month ===========================
    mod_har_m <- qlikeHAR(X=data_har[1:(win-1),], y=data_har[2:win,3], S=S)
    mod_har_m$coef 
    mod_har_m$ols_model
    mod_har_m$coef  %*% (c(1, tail(data_har, 1)) %>% as.numeric())
    mod_har_m$ols_model %*% (c(1, tail(data_har, 1)) %>% as.numeric())
    
    har_1ma <- # one month ahead HAR
      tibble(
        date=d_, 
        realization = xf_m, 
        qlikeHAR = mod_har_m$coef %*% (c(1, tail(data_har, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_har_m$ols_model %*% (c(1, tail(data_har, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HAR", 
        horizon = "1month"
      )
    
    # HARQ ======================================================================
    data_harq <- data.frame(
      #date= ymd(aapl$date),
      rv = rv_, 
      rvw = rvw, 
      rvm = rvm, 
      rvrq = sqrt(rq_)*(rv_)
    ) #%>% tidyr::drop_na()
    data_harq
    
    ## day ============================
    mod_harq <- qlikeHAR(X=data_harq[1:(win-1),], y=data_harq[2:win,1], S=S)
    harq_1da <- # one day ahead HARQ
      tibble(
        date=d_, 
        realization = xf, 
        qlikeHAR = mod_harq$coef %*% (c(1, tail(data_harq, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_harq$ols_model %*% (c(1, tail(data_harq, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HARQ", 
        horizon = "1day"
      )
    
    ## week ============================
    mod_harq_w <- qlikeHAR(X=data_harq[1:(win-1),], y=data_harq[2:win,2], S=S)
    harq_1wa <- # one week ahead HARQ
      tibble(
        date=d_, 
        realization = xf_w, 
        qlikeHAR = mod_harq_w$coef %*% (c(1, tail(data_harq, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_harq_w$ols_model %*% (c(1, tail(data_harq, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HARQ", 
        horizon = "1week"
      )
    
    ## month ===========================
    mod_harq_m <- qlikeHAR(X=data_harq[1:(win-1),], y=data_harq[2:win,3], S=S)
    harq_1ma <- # one month ahead HARQ
      tibble(
        date=d_, 
        realization = xf_m, 
        qlikeHAR = mod_harq_m$coef %*% (c(1, tail(data_harq, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_harq_m$ols_model %*% (c(1, tail(data_harq, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "HARQ", 
        horizon = "1month"
      )
    
    
    # SHAR ======================================================================
    data_shar<- data.frame(
      #date= ymd(aapl$date),
      rv = rv_, 
      rvw = rvw, 
      rvm = rvm, 
      good = good_, 
      bad = bad_
    ) #%>% tidyr::drop_na()
    data_shar
    
    ## day ============================
    mod_shar <- qlikeHAR(X=data_shar[1:(win-1),], y=data_shar[2:win,1], S=S)
    shar_1da <- # one day ahead SHAR
      tibble(
        date=d_, 
        realization = xf, 
        qlikeHAR = mod_shar$coef %*% (c(1, tail(data_shar, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_shar$ols_model %*% (c(1, tail(data_shar, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "SHAR", 
        horizon = "1day"
      )
    
    ## week ============================
    mod_harq_w <- qlikeHAR(X=data_shar[1:(win-1),], y=data_shar[2:win,2], S=S)
    shar_1wa <- # one week ahead SHAR
      tibble(
        date=d_, 
        realization = xf_w, 
        qlikeHAR = mod_harq_w$coef %*% (c(1, tail(data_shar, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_harq_w$ols_model %*% (c(1, tail(data_shar, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "SHAR", 
        horizon = "1week"
      )
    
    ## month ===========================
    mod_harq_m <- qlikeHAR(X=data_shar[1:(win-1),], y=data_shar[2:win,3], S=S)
    shar_1ma <- # one month ahead SHAR
      tibble(
        date=d_, 
        realization = xf_m, 
        qlikeHAR = mod_harq_m$coef %*% (c(1, tail(data_shar, 1)) %>% as.numeric()) %>% as.numeric(), 
        mseHAR = mod_harq_m$ols_model %*% (c(1, tail(data_shar, 1))%>% as.numeric())  %>% as.numeric(), 
        model = "SHAR", 
        horizon = "1month"
      )
    
    onedayahead   <-  bind_rows(har_1da, harq_1da, shar_1da)
    oneweekahead  <-  bind_rows(har_1wa, harq_1wa, shar_1wa)
    onemonthahead <-  bind_rows(har_1ma, harq_1ma, shar_1ma)
    
    out <- bind_rows(onedayahead,oneweekahead,onemonthahead)
    
  }
  
  out <- out[!sapply(out, function(x) inherits(x, "simpleError"))]
  out <- do.call('rbind', out)
  #saveRDS(out, file)
  out
}

