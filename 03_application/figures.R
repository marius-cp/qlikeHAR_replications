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
library(patchwork)

# plot 5min ----
fcasts <- readRDS("./fcasts_capire_5min_rvola.rds")

fcasts %>% 
  filter(date>="2012-01-01", date<"2024-01-01") %>% 
  filter(sym =="AAPL") %>% group_by(win, horizon, model) %>% summarise(n=n())

negvola <-
  c(
    # negative vola sHAR QLIKE
    fcasts %>% 
      filter(
        qlikeHAR<0
      ) %>% 
      pull(date) %>% 
      unique(),
    # negative vola sHAR MSE
    fcasts %>% 
      filter(mseHAR<0) %>% 
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
    #qlikeloss_qlikeHAR = (realization / qlikeHAR) - log(realization / qlikeHAR) - 1,
    qlikeloss_mseHAR =  log(mseHAR)+realization/mseHAR,
    #qlikeloss_mseHAR = (realization / mseHAR) - log(realization / mseHAR) - 1,
    seloss_qlikeHAR =  100*(realization-qlikeHAR)^2, 
    seloss_mseHAR =  100*(realization-mseHAR)^2,
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
  #filter(sym=="AAPL",win==2000) %>% 
  group_by(sym, win, horizon, model) %>% 
  group_map(~ compute_statistics(.x), .keep = TRUE) %>% 
  bind_rows()

results


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
      ml=10
    ), 
    mse_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdiffmse), 
        index = "sym"
      ), 
      ml=10
    )
  ) 
paneldmw_


# breakpoints <- c(-Inf, -1.96, -1.64, 0, 1.64, 1.96, Inf)
# labels <- c("t<-1.96", "t<-1.64", "inconclusive", "inconclusive", "t>1.64", "t>1.96")
# color_scale <- scale_color_manual(
#   values = c(
#     "t<-1.96" = "#440154FF", 
#     "t<-1.64" = "#31688EFF", 
#     "inconclusive" = "gray", 
#     "t>1.64" = "#35B779FF", 
#     "t>1.96" = "#FDE725FF"
#   ), 
#   drop = FALSE, 
# )


breakpoints <- c(-Inf, -1.96, 0, 1.96, Inf)
labels <- c("qlikeHAR sig. better", "inconclusive", "inconclusive", "mseHAR sig. better")
color_scale <- scale_color_manual(
  values = c(
    #"t<-2.33" = "#440154FF", 
    "qlikeHAR sig. better" = "blue", 
    "inconclusive" = "gray", 
    "mseHAR sig. better" = "coral"
    # "t>2.33" = "#FDE725FF"
  ), 
  drop = FALSE, 
)


infoinconc <- 
  bind_rows(
    results %>% 
      filter(qlike_p_value >.05) %>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="QLIKE",
        qHARbetter=ifelse(qlike_statistic<0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      ),
    results %>% 
      filter(mse_p_value >.05)%>% 
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

# plot 1min ----
fcasts <- readRDS("./fcasts_capire_1min_rvola.rds")

fcasts %>% 
  filter(date>="2011-01-01", date<"2024-03-01") %>% 
  filter(sym =="AAPL") %>% group_by(win, horizon, model) %>% summarise(n=n())

negvola <-
  c(
    # negative vola sHAR QLIKE
    fcasts %>% 
      filter(
        qlikeHAR<0
      ) %>% 
      pull(date) %>% 
      unique(),
    # negative vola sHAR MSE
    fcasts %>% 
      filter(mseHAR<0) %>% 
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
    #qlikeloss_qlikeHAR = (realization / qlikeHAR) - log(realization / qlikeHAR) - 1,
    qlikeloss_mseHAR =  log(mseHAR)+realization/mseHAR,
    #qlikeloss_mseHAR = (realization / mseHAR) - log(realization / mseHAR) - 1,
    seloss_qlikeHAR =  100*(realization-qlikeHAR)^2, 
    seloss_mseHAR =  100*(realization-mseHAR)^2,
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
  #filter(sym=="AAPL",win==2000) %>% 
  group_by(sym, win, horizon, model) %>% 
  group_map(~ compute_statistics(.x), .keep = TRUE) %>% 
  bind_rows()

results



results %>% 
  mutate(
    rej_qlike = qlike_p_value < .1, 
    rej_mse = mse_p_value < .1, 
  ) %>% 
  group_by(symbol, win, horizon) %>% 
  summarise(mean_rej_qlike=mean(rej_qlike), mean_rej_mse=mean(rej_mse)) %>% 
  group_by(win) %>% 
  summarise(mean(mean_rej_qlike), mean(mean_rej_mse))

results %>% 
  mutate(
    rej_qlike = qlike_p_value < .05, 
    rej_mse = mse_p_value < .05, 
  ) %>% 
  group_by(symbol, win, horizon) %>% 
  summarise(mean_rej_qlike=mean(rej_qlike), mean_rej_mse=mean(rej_mse)) %>% 
  group_by(horizon) %>% 
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
      ml=10
    ), 
    mse_statistic = paneldmw(
      plm::pdata.frame(
        data.frame(sym,dif=lossdiffmse), 
        index = "sym"
      ), 
      ml=10
    )
  ) 
paneldmw_


# breakpoints <- c(-Inf, -1.96, -1.64, 0, 1.64, 1.96, Inf)
# labels <- c("t<-1.96", "t<-1.64", "inconclusive", "inconclusive", "t>1.64", "t>1.96")
# color_scale <- scale_color_manual(
#   values = c(
#     "t<-1.96" = "#440154FF", 
#     "t<-1.64" = "#31688EFF", 
#     "inconclusive" = "gray", 
#     "t>1.64" = "#35B779FF", 
#     "t>1.96" = "#FDE725FF"
#   ), 
#   drop = FALSE, 
# )


breakpoints <- c(-Inf, -1.96, 0, 1.96, Inf)
labels <- c("qlikeHAR sig. better", "inconclusive", "inconclusive", "mseHAR sig. better")
color_scale <- scale_color_manual(
  values = c(
    #"t<-2.33" = "#440154FF", 
    "qlikeHAR sig. better" = "blue", 
    "inconclusive" = "gray", 
    "mseHAR sig. better" = "coral"
   # "t>2.33" = "#FDE725FF"
  ), 
  drop = FALSE, 
)



infoinconc <- 
  bind_rows(
    results %>% 
      filter(qlike_p_value >.05) %>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="QLIKE",
        qHARbetter=ifelse(qlike_statistic<0, 1, 0)
      ) %>% 
      select(
        symbol,win,horizon,model,lossfunction, qHARbetter, tstatgroups
      ),
    results %>% 
      filter(mse_p_value >.05)%>% 
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


# summarize table----

# time ----

# comp time ----

## 5 min ----
fcasts <- readRDS("./fcasts_capire_5min_rvola.rds")

fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01") %>% 
  group_by(win,model,horizon) %>% 
  summarise(mean(time),mean(timeOLS))

plottime5min <-
fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01", model!="HARQ") %>% 
  select(date, win, model, horizon, time, timeOLS) %>% 
  pivot_longer(cols = time:timeOLS) %>% 
  mutate(name=ifelse(name=="time","QLIKE Solution","OLS SE Solution"))%>% 
  mutate(
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  ) %>% 
  ggplot(aes(x=factor(win), y=(value), color=name))+
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0)) +
  facet_nested(model~horizon)+
  scale_y_log10()+
  coord_cartesian(ylim = c(1e-4,.0225))+
  xlab("length of estimation window")+
  ylab("time in seconds")+
  theme_bw()+
  theme(
    legend.position = "bottom", 
    legend.title  = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  scale_color_manual(values = c("coral", "purple"))

ggsave("fig_capire_estimationtime_5min.pdf", width = 8, height = 6)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_capire_estimationtime_5min.pdf", width = 8, height = 6)


## 1 min ----
fcasts <- readRDS("./fcasts_capire_1min_rvola.rds")

fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01") %>% 
  group_by(win,model,horizon) %>% 
  summarise(mean(time),mean(timeOLS))

plottime1min <-
fcasts %>% 
  filter(date>="2011-01-01", date<"2024-01-01", model!="HARQ") %>% 
  select(date, win, model, horizon, time, timeOLS) %>% 
  pivot_longer(cols = time:timeOLS) %>% 
  mutate(name=ifelse(name=="time","QLIKE Solution","OLS SE Solution"))%>% 
  mutate(
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  ) %>% 
  ggplot(aes(x=factor(win), y=(value), color=name))+
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0)) +
  facet_nested(model~horizon)+
  scale_y_log10()+
  coord_cartesian(ylim = c(1e-4,.0225))+
  xlab("length of estimation window")+
  ylab("time in seconds")+
  theme_bw()+
  theme(
    legend.position = "bottom", 
    legend.title  = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  scale_color_manual(values = c("coral", "purple"))

ggsave("fig_capire_estimationtime_1min.pdf", width = 8, height = 6)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_capire_estimationtime_1min.pdf", width = 8, height = 6)

(
 (plottime5min + ggtitle("a) 5 minute data")) / 
 (plottime1min + ggtitle("a) 1 minute data"))
  )+ 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("fig_capire_estimationtime_patch.pdf", width = 8, height = 2*6)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_capire_estimationtime_patch.pdf", width = 8, height = 2*6)
