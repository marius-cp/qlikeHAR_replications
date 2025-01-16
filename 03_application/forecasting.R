rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(xts)
library(lubridate)
library(ggh4x)
library(sandwich)
source("../00_functions/qlikeHAR.R")
source("../00_functions/helper_functions.R")
library(tidyverse)
library(doParallel)
datall <- readRDS("../02_data/capire_clean.rds") #%>% filter(date >= "2020-01-01")


datall <-
readRDS("../02_data/capire_clean.rds") %>% 
  mutate(
    rv1_w = RcppRoll::roll_meanr(rv1, n=5, fill=NA),
    rv1_m = RcppRoll::roll_meanr(rv1, n=22, fill=NA),
    rv5_w = RcppRoll::roll_meanr(rv5, n=5, fill=NA),
    rv5_m = RcppRoll::roll_meanr(rv5, n=22, fill=NA)
    ) %>% 
  select(
    date, Symbol, 
    rv1, rv1_w, rv1_m,  rq1,  bpv1, good1,  bad1, 
    rv5, rv5_w, rv5_m,  rq5,  bpv5, good5,  bad5
    ) %>% 
  tidyr::drop_na()

datall

# sym <- "NKE"
# dat <- datall %>% dplyr::filter(Symbol == sym) 
# rv =xts(dat$rv5, order.by = dat$date)
# rvw=xts(dat$rv5_w, order.by = dat$date)
# rvm=xts(dat$rv5_m, order.by = dat$date)
# rq = xts(dat$rq5, order.by = dat$date)
# good =  xts(dat$good5, order.by = dat$date)
# bad =  xts(dat$bad5, order.by = dat$date)
# win = 1000


# define qlike loss 
S =  function(x, y) log(x)+y/x
syms <- datall$Symbol %>% unique()
wins <- c(250, 500, 750, 1000, 1500, 2000)


# 5min ----
fcasts <- NULL
for (w in wins) {
  print(w)
  for (s in 1:(length(syms))) {
    sym <- syms[s]
    dat <- datall %>% dplyr::filter(Symbol == sym) 
    
    core.max <- 8
    cl <- makeCluster(min(parallel::detectCores() - 1, core.max))
    registerDoParallel(cl)
    fcasts_temp <- new_do_predictions(
      rv =xts(dat$rv5, order.by = dat$date), 
      rvw=xts(dat$rv5_w, order.by = dat$date), 
      rvm=xts(dat$rv5_m, order.by = dat$date), 
      rq = xts(dat$rq5, order.by = dat$date), 
      good =  xts(dat$good5, order.by = dat$date), 
      bad =  xts(dat$bad5, order.by = dat$date),
      win = w
    ) %>% mutate(sym = sym, win=w)
    stopCluster(cl)
    
    fcasts <- bind_rows(fcasts, fcasts_temp)
  }
}

saveRDS(fcasts, "fcasts_capire_5min_rvola.rds")
fcasts

# 1min ----
fcasts <- NULL
for (w in wins) {
  print(w)
  for (s in 1:(length(syms))) {
    sym <- syms[s]
    dat <- datall %>% dplyr::filter(Symbol == sym) 
    
    core.max <- 8
    cl <- makeCluster(min(parallel::detectCores() - 1, core.max))
    registerDoParallel(cl)
    fcasts_temp <- new_do_predictions(
      rv =xts(dat$rv1, order.by = dat$date), 
      rvw=xts(dat$rv1_w, order.by = dat$date), 
      rvm=xts(dat$rv1_m, order.by = dat$date), 
      rq = xts(dat$rq1, order.by = dat$date), 
      good =  xts(dat$good1, order.by = dat$date), 
      bad =  xts(dat$bad1, order.by = dat$date),
      win = w
    ) %>% mutate(sym = sym, win=w)
    stopCluster(cl)
    
    fcasts <- bind_rows(fcasts, fcasts_temp)
  }
}

saveRDS(fcasts, "fcasts_capire_1min_rvola.rds")
fcasts

