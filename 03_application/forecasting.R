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

# define qlike loss 
S =  function(x, y) log(x)+y/x
syms <- datall$Symbol %>% unique()
wins <- c(250, 500, 750, 1000, 1500, 2000)

# forecasting ----
## 5min ----
fcasts <- NULL
for (w in wins) {
  print(w)
  for (s in 1:length(syms)) {
    sym <- syms[s]
    dat <- datall %>% dplyr::filter(Symbol == sym) 
  
    core.max <- 8
    cl <- makeCluster(min(parallel::detectCores() - 1, core.max))
    registerDoParallel(cl)
    fcasts_temp <- do_predictions(
      rv = xts(dat$rv5, order.by = dat$date),
      rq = xts(dat$rq5, order.by = dat$date), 
      good =  xts(dat$good5, order.by = dat$date),
      bad =  xts(dat$bad5, order.by = dat$date), 
      win = w
    ) %>% mutate(sym = sym, win=w)
    stopCluster(cl)
    
    fcasts <- bind_rows(fcasts, fcasts_temp)
  }
}
saveRDS(fcasts, "./fcasts_capire_5min.rds")

check <- fcasts %>% group_by(sym, horizon, win) %>% 
  summarize(n())

## 1min ----
fcasts <- NULL
for (w in wins) {
  print(w)
  for (s in 1:length(syms)) {
    sym <- syms[s]
    dat <- datall %>% dplyr::filter(Symbol == sym) 
    
    core.max <- 8
    cl <- makeCluster(min(parallel::detectCores() - 1, core.max))
    registerDoParallel(cl)
    fcasts_temp <- do_predictions(
      rv = xts(dat$rv1, order.by = dat$date),
      rq = xts(dat$rq1, order.by = dat$date), 
      good =  xts(dat$good1, order.by = dat$date),
      bad =  xts(dat$bad1, order.by = dat$date), 
      win = w
    ) %>% mutate(sym = sym, win=w)
    stopCluster(cl)
    
    fcasts <- bind_rows(fcasts, fcasts_temp)
  }
}
saveRDS(fcasts, "./fcasts_capire_1min.rds")


# plots  ----
## 5min ----
fcasts <- readRDS("./fcasts_capire_5min.rds")

fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01") %>% 
  filter(sym =="AAPL") %>% group_by(win, horizon) %>% summarise(n=n())

# HARQ results often in negative vola
# other neg vola dates 
negvola <-
c(
  # negative vola sHAR QLIKE
  fcasts %>% 
  filter(model != "HARQ", qlikeHAR<0) %>% 
  pull(date) %>% 
  unique(),
# negative vola sHAR MSE
fcasts %>% 
  filter(model != "HARQ", mseHAR<0) %>% 
  pull(date) %>% 
  unique()
) %>% sort()

losses <- 
  fcasts %>% 
  filter(
    model != "HARQ",
    date>="2012-01-01", date<"2024-01-01", 
    !date %in% negvola
         ) %>% 
  mutate(
    qlikeloss_qlikeHAR =  log(qlikeHAR)+realization/qlikeHAR,
    qlikeloss_mseHAR =  log(mseHAR)+realization/mseHAR, 
    seloss_qlikeHAR =  (realization-qlikeHAR)^2, 
    seloss_mseHAR =  (realization-mseHAR)^2,
    lossdifqlike = qlikeloss_qlikeHAR - qlikeloss_mseHAR, 
    lossdiffmse = seloss_qlikeHAR - seloss_mseHAR
  )

avglosses <-
  losses %>% 
  select(sym,win,horizon, qlikeloss_qlikeHAR:seloss_mseHAR) %>% 
  pivot_longer(
    cols = qlikeloss_qlikeHAR:seloss_mseHAR
  ) %>% 
  group_by(name, sym, win, horizon) %>% 
  summarise(mean(value, na.rm=T)) %>% arrange(sym)
avglosses

# Perform the operations
results <- losses %>% 
  group_by(sym, win, horizon, model) %>% 
  group_map(~ compute_statistics(.x), .keep = TRUE) %>% 
  bind_rows()

results
saveRDS(results, "DMW_results_individual_5min.rds")


results %>% 
  mutate(
    rej_qlike = qlike_p_value < .1, 
    rej_mse = mse_p_value < .1, 
  ) %>% 
  group_by(symbol, win, horizon) %>% 
  summarise(mean_rej_qlike=mean(rej_qlike), mean_rej_mse=mean(rej_mse)) %>% 
  group_by(win, horizon) %>% 
  summarise(mean(mean_rej_qlike), mean(mean_rej_mse))

paneldmw_ <- 
  losses %>%
  mutate(
    lossdifqlike = qlikeloss_qlikeHAR - qlikeloss_mseHAR, 
    lossdiffmse = seloss_qlikeHAR - seloss_mseHAR
  ) %>%
  group_by(win, horizon, model) %>%
  summarise(
    qlike_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdifqlike), 
        index = "sym"
      ), 
      ml=5
    ), 
    mse_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdiffmse), 
        index = "sym"
      ), 
      ml=5
    )
  ) 
paneldmw_
saveRDS(paneldmw_, "DMW_results_panel_5min.rds")


breakpoints <- c(-Inf, -1.96, -1.64, 0, 1.64, 1.96, Inf)
labels <- c("<-1.96", "t<-1.64", "inconclusive", "inconclusive", "t>1.64", "t>1.96")
color_scale <- scale_color_manual(
  values = c(
    "<-1.96" = "#440154FF", 
    "t<-1.64" = "#31688EFF", 
    "inconclusive" = "grey", 
    "t>1.64" = "#35B779FF", 
    "t>1.96" = "#FDE725FF"
  ), 
  drop = FALSE, 
)

infoinconc <- 
  bind_rows(
    results %>% 
      filter(qlike_p_value >.1) %>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="QLIKE",
        qHARbetter=ifelse(qlike_statistic<0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      ),
    results %>% 
      filter(mse_p_value >.1)%>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="MSE", 
        qHARbetter=ifelse(mse_statistic<=0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      )
  ) %>% 
  mutate(
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  )

paneldmw_ %>% 
  pivot_longer(
    cols = qlike_statistic:mse_statistic, 
    values_to = "teststatistic", 
    names_to = "lossfunction"
  ) %>% 
  mutate(symbol="panel DMW")

plotdata <- 
  results %>% 
  pivot_longer(
    cols =c(qlike_statistic,mse_statistic), 
    values_to = "teststatistic", 
    names_to = "lossfunction"
  ) %>% 
  bind_rows(
    paneldmw_ %>% 
      pivot_longer(
        cols = qlike_statistic:mse_statistic, 
        values_to = "teststatistic", 
        names_to = "lossfunction"
      ) %>% 
      mutate(symbol="panel DMW")
  ) %>% 
  mutate(
    lossfunction = ifelse(lossfunction=="mse_statistic", "MSE", "QLIKE"),
    tstatgroups = cut(
      teststatistic, 
      breaks=breakpoints, 
      labels=labels, 
      include.lowest=TRUE
    )
  ) %>% 
  mutate(
    symbol = factor(
      symbol, 
      levels = c(sort(setdiff(unique(symbol), "panel DMW")), "panel DMW")
      ), 
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
      )
    ) %>%
  arrange(symbol) 



plotdata %>% 
  ggplot(aes(y=fct_rev(symbol), x=factor(win), color=tstatgroups))+
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =1.5), 
    fill = "lightyellow", show.legend = FALSE
  )+ 
  geom_point()+
  #geom_tile()+
  color_scale+
  facet_nested(model~horizon+lossfunction )+
  xlab("length of estimation window")+
  ylab("symbol")+
  theme_bw()+
  theme(
    legend.position = "bottom", 
    legend.title  = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
    )+
  geom_point(
    infoinconc %>% filter(qHARbetter==0),
    mapping = aes(y=(symbol), x=factor(win)),
    shape="+", color="black"
  )+
  geom_hline(yintercept = 1.5, linetype = "solid", color = "black")+
  ylab(element_blank())

ggsave("fig_capire_dm_5min.pdf", width = 8, height = 8)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_capire_dm_5min.pdf", width = 8, height = 8)

## 1min ----
fcasts <- readRDS("./fcasts_capire_1min.rds")

fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01") %>% 
  filter(sym =="AAPL") %>% group_by(win, horizon) %>% summarise(n=n())

# HARQ results often in negative vola
# other neg vola dates 
negvola <-
  c(
    # negative vola sHAR QLIKE
    fcasts %>% 
      filter(model != "HARQ", qlikeHAR<0) %>% 
      pull(date) %>% 
      unique(),
    # negative vola sHAR MSE
    fcasts %>% 
      filter(model != "HARQ", mseHAR<0) %>% 
      pull(date) %>% 
      unique()
  ) %>% sort()

losses <- 
  fcasts %>% 
  filter(
    model != "HARQ",
    date>="2012-01-01", date<"2024-01-01", 
    !date %in% negvola
  ) %>% 
  mutate(
    qlikeloss_qlikeHAR =  log(qlikeHAR)+realization/qlikeHAR,
    qlikeloss_mseHAR =  log(mseHAR)+realization/mseHAR, 
    seloss_qlikeHAR =  (realization-qlikeHAR)^2, 
    seloss_mseHAR =  (realization-mseHAR)^2,
    lossdifqlike = qlikeloss_qlikeHAR - qlikeloss_mseHAR, 
    lossdiffmse = seloss_qlikeHAR - seloss_mseHAR
  )

avglosses <-
  losses %>% 
  select(sym,win,horizon, qlikeloss_qlikeHAR:seloss_mseHAR) %>% 
  pivot_longer(
    cols = qlikeloss_qlikeHAR:seloss_mseHAR
  ) %>% 
  group_by(name, sym, win, horizon) %>% 
  summarise(mean(value, na.rm=T)) %>% arrange(sym)
avglosses

# Perform the operations
results <- losses %>% 
  group_by(sym, win, horizon, model) %>% 
  group_map(~ compute_statistics(.x), .keep = TRUE) %>% 
  bind_rows()

results
saveRDS(results, "DMW_results_individual_1min.rds")


results %>% 
  mutate(
    rej_qlike = qlike_p_value < .1, 
    rej_mse = mse_p_value < .1, 
  ) %>% 
  group_by(symbol, win, horizon) %>% 
  summarise(mean_rej_qlike=mean(rej_qlike), mean_rej_mse=mean(rej_mse)) %>% 
  group_by(win, horizon) %>% 
  summarise(mean(mean_rej_qlike), mean(mean_rej_mse))

paneldmw_ <- 
  losses %>%
  mutate(
    lossdifqlike = qlikeloss_qlikeHAR - qlikeloss_mseHAR, 
    lossdiffmse = seloss_qlikeHAR - seloss_mseHAR
  ) %>%
  group_by(win, horizon, model) %>%
  summarise(
    qlike_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdifqlike), 
        index = "sym"
      ), 
      ml=5
    ), 
    mse_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdiffmse), 
        index = "sym"
      ), 
      ml=5
    )
  ) 
paneldmw_
saveRDS(paneldmw_, "DMW_results_panel_1min.rds")


breakpoints <- c(-Inf, -1.96, -1.64, 0, 1.64, 1.96, Inf)
labels <- c("<-1.96", "t<-1.64", "inconclusive", "inconclusive", "t>1.64", "t>1.96")
color_scale <- scale_color_manual(
  values = c(
    "<-1.96" = "#440154FF", 
    "t<-1.64" = "#31688EFF", 
    "inconclusive" = "grey", 
    "t>1.64" = "#35B779FF", 
    "t>1.96" = "#FDE725FF"
  ), 
  drop = FALSE, 
)

infoinconc <- 
  bind_rows(
    results %>% 
      filter(qlike_p_value >.1) %>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="QLIKE",
        qHARbetter=ifelse(qlike_statistic<0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      ),
    results %>% 
      filter(mse_p_value >.1)%>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="MSE", 
        qHARbetter=ifelse(mse_statistic<=0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      )
  ) %>% 
  mutate(
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  )

paneldmw_ %>% 
  pivot_longer(
    cols = qlike_statistic:mse_statistic, 
    values_to = "teststatistic", 
    names_to = "lossfunction"
  ) %>% 
  mutate(symbol="panel DMW")


plotdata <- 
  results %>% 
  pivot_longer(
    cols =c(qlike_statistic,mse_statistic), 
    values_to = "teststatistic", 
    names_to = "lossfunction"
  ) %>% 
  bind_rows(
    paneldmw_ %>% 
      pivot_longer(
        cols = qlike_statistic:mse_statistic, 
        values_to = "teststatistic", 
        names_to = "lossfunction"
      ) %>% 
      mutate(symbol="panel DMW")
  ) %>% 
  mutate(
    lossfunction = ifelse(lossfunction=="mse_statistic", "MSE", "QLIKE"),
    tstatgroups = cut(
      teststatistic, 
      breaks=breakpoints, 
      labels=labels, 
      include.lowest=TRUE
    )
  ) %>% 
  mutate(
    symbol = factor(
      symbol, 
      levels = c(sort(setdiff(unique(symbol), "panel DMW")), "panel DMW")
    ), 
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  ) %>%
  arrange(symbol) 



plotdata %>% 
  ggplot(aes(y=fct_rev(symbol), x=factor(win), color=tstatgroups))+
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax =1.5), 
    fill = "lightyellow", show.legend = FALSE
  )+ 
  geom_point()+
  #geom_tile()+
  color_scale+
  facet_nested(model~horizon+lossfunction )+
  xlab("length of estimation window")+
  ylab("symbol")+
  theme_bw()+
  theme(
    legend.position = "bottom", 
    legend.title  = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  geom_point(
    infoinconc %>% filter(qHARbetter==0),
    mapping = aes(y=(symbol), x=factor(win)),
    shape="+", color="black"
  )+
  geom_hline(yintercept = 1.5, linetype = "solid", color = "black")+
  ylab(element_blank())

ggsave("fig_capire_dm_1min.pdf", width = 8, height = 8)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_capire_dm_1min.pdf", width = 8, height = 8)


# appl test ----
# test qlike function
aapl <- dat %>% filter(Symbol=="AAPL")

rv <- xts(sqrt(aapl$rv5), order.by = aapl$date) # use matrix of xts as input
win=1000; t=win+1
rv_ <- as.matrix(rv[(t-win):(t-1),])
d_ <- index(rv)[t]
xf <- as.numeric(rv[t])


# HAR
## har ----
data_har <- data.frame(
  #date= ymd(aapl$date),
  "rv" = aapl$rv5 %>% sqrt(), 
  rvw = RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=5, fill=NA), 
  rvm = RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=22, fill=NA)
) %>% tidyr::drop_na()
data_har

### har daily ----
mod_har <- qlikeHAR(X=data_har[1:(1000-1),], y=data_har[2:1000,1], S=S)
mod_har$coef  %*% (c(1, tail(data_har, 1)) %>% as.numeric())
mod_har$ols_model %*% (c(1, tail(data_har, 1)) %>% as.numeric())

### har weekly ----
mod_har_w <- qlikeHAR(X=data_har[1:(1000-1),], y=data_har[2:1000,2], S=S)
mod_har_w$coef 
mod_har_w$ols_model
mod_har_w$coef  %*% (c(1, tail(data_har, 1)) %>% as.numeric())
mod_har_w$ols_model %*% (c(1, tail(data_har, 1)) %>% as.numeric())

### har monthly ----
mod_har_m <- qlikeHAR(X=data_har[1:(1000-1),], y=data_har[2:1000,3], S=S)
mod_har_m$coef 
mod_har_m$ols_model
mod_har_m$coef  %*% (c(1, tail(data_har, 1)) %>% as.numeric())
mod_har_m$ols_model %*% (c(1, tail(data_har, 1)) %>% as.numeric())


## harq ----
data_harq <- data.frame(
  #date= ymd(aapl$date),
  rv = aapl$rv5%>% sqrt(), 
  rvw =RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=5, fill=NA), 
  rvm =RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=22, fill=NA),
  rvrq = sqrt(aapl$rq5)*sqrt(aapl$rv5)
) %>% tidyr::drop_na()
data_harq

### harq daily ----
mod_harq <- qlikeHAR(X=data_harq[1:(1000-1),], y=data_harq[2:1000,1], S=S)
mod_harq$coef
mod_harq$ols_model
mod_harq$coef  %*% (c(1, tail(data_harq, 1)) %>% as.numeric())
mod_harq$ols_model %*% (c(1, tail(data_harq, 1)) %>% as.numeric())

### harq weekly ----
mod_harq_w <- qlikeHAR(X=data_harq[1:(1000-1),], y=data_harq[2:1000,2], S=S)
mod_harq_w$coef 
mod_harq_w$ols_model
mod_harq_w$coef  %*% (c(1, tail(data_harq, 1)) %>% as.numeric())
mod_harq_w$ols_model %*% (c(1, tail(data_harq, 1)) %>% as.numeric())

### harq monthly ----
mod_harq_m <- qlikeHAR(X=data_harq[1:(1000-1),], y=data_harq[2:1000,3], S=S)
mod_harq_m$coef 
mod_harq_m$ols_model
mod_harq_m$coef  %*% (c(1, tail(data_harq, 1)) %>% as.numeric())
mod_harq_m$ols_model %*% (c(1, tail(data_harq, 1)) %>% as.numeric())

## shar ----
data_shar<- data.frame(
  #date= ymd(aapl$date),
  rv = aapl$rv5%>% sqrt(), 
  rvw =RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=5, fill=NA), 
  rvm =RcppRoll::roll_meanr(aapl$rv5%>% sqrt(), n=22, fill=NA),
  good = aapl$good5, 
  bad = aapl$bad5
) %>% tidyr::drop_na()
data_shar

### shar daily ----
mod_shar <- qlikeHAR(X=data_shar[1:(1000-1),], y=data_shar[2:1000,1], S=S)
mod_shar$coef
mod_shar$ols_model
mod_shar$coef  %*% (c(1, tail(data_shar, 1)) %>% as.numeric())
mod_shar$ols_model %*% (c(1, tail(data_shar, 1)) %>% as.numeric())

### shar weekly ----
mod_harq_w <- qlikeHAR(X=data_shar[1:(1000-1),], y=data_shar[2:1000,2], S=S)
mod_harq_w$coef 
mod_harq_w$ols_model
mod_harq_w$coef  %*% (c(1, tail(data_shar, 1)) %>% as.numeric())
mod_harq_w$ols_model %*% (c(1, tail(data_shar, 1)) %>% as.numeric())

### shar monthly ----
mod_harq_m <- qlikeHAR(X=data_shar[1:(1000-1),], y=data_shar[2:1000,3], S=S)
mod_harq_m$coef 
mod_harq_m$ols_model
mod_harq_m$coef  %*% (c(1, tail(data_shar, 1)) %>% as.numeric())
mod_harq_m$ols_model %*% (c(1, tail(data_shar, 1)) %>% as.numeric())
