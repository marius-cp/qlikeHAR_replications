library(doParallel)
library(tidyverse)
library(sandwich)

source("qlikeHAR.R")
source("helper_functions.R")

# define qlike loss 
S <-  function(x, y) log(x)+y/x
wins <- c(250, 500, 750, 1000, 1500, 2000)

fcasts <- NULL
for (w in wins) {
	print(w)
	fcasts_temp <- do_predictions(
      	rv = xts(dat$rv, order.by = dat$date),
      	rq = xts(dat$rq, order.by = dat$date), 
      	good =  xts(dat$good, order.by = dat$date),
      	bad =  xts(dat$bad, order.by = dat$date), 
      	win = w
    	) %>% mutate(sym = Symbol, win=w)
	fcasts <- bind_rows(fcasts, fcasts_temp)
	}

filename <- paste0("fcasts_", Symbol, "_M", M, "_alpha", alphav, ".rds")

#saveRDS(fcasts, filename)

fcasts <- readRDS(filename)

fcasts %>% group_by(win, horizon) %>% summarise(n=n())

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
    date>="2001-01-01", date<"2024-01-01", 
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
  dplyr::select(sym,win,horizon, qlikeloss_qlikeHAR:seloss_mseHAR) %>% 
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

filename <- paste0("DMW_results_individual_", Symbol, "_M", M, "_alpha", alphav, ".rds")

#saveRDS(results, filename)







