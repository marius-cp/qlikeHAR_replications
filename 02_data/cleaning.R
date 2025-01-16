rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)
library(readxl)

# read ----
file_path <- "Capire_March2024.xlsx" 
sheet_names <- excel_sheets(file_path)
print(sheet_names)

companies <- read_excel(file_path, sheet = "Companies", col_names = FALSE) 
dates <- read_excel(file_path, sheet = "Dates", col_names = FALSE) 

## rv
rv1min <- read_excel(file_path, sheet = "RV", col_names = FALSE) 
rv5min <- read_excel(file_path, sheet = "RV_5", col_names = FALSE) 
colnames(rv1min) <- companies %>% pull(`...1`)
colnames(rv5min) <- companies %>% pull(`...1`)

rv1min <- 
  rv1min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "rv1"
  )
rv1min

rv5min <- 
  rv5min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "rv5"
  )
rv5min

rv <- left_join(rv1min,rv5min)

## bpv
bpv1min <- read_excel(file_path, sheet = "BPV", col_names = FALSE) 
bpv5min <- read_excel(file_path, sheet = "BPV_5", col_names = FALSE) 
colnames(bpv1min) <- companies %>% pull(`...1`)
colnames(bpv5min) <- companies %>% pull(`...1`)

bpv1min <- 
  bpv1min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "bpv1"
  )
bpv1min

bpv5min <- 
  bpv5min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "bpv5"
  )
bpv5min

bpv <- left_join(bpv1min,bpv5min)


## rq
rq1min <- read_excel(file_path, sheet = "RQ", col_names = FALSE) 
rq5min <- read_excel(file_path, sheet = "RQ_5", col_names = FALSE) 
colnames(rq1min) <- companies %>% pull(`...1`)
colnames(rq5min) <- companies %>% pull(`...1`)

rq1min <- 
  rq1min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "rq1"
  )
rq1min

rq5min <- 
  rq5min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "rq5"
  )
rq5min

rq <- left_join(rq1min,rq5min)


## good
good1min <- read_excel(file_path, sheet = "Good", col_names = FALSE) 
good5min <- read_excel(file_path, sheet = "Good_5", col_names = FALSE) 
colnames(good1min) <- companies %>% pull(`...1`)
colnames(good5min) <- companies %>% pull(`...1`)

good1min <- 
  good1min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "good1"
  )
good1min

good5min <- 
  good5min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "good5"
  )
good5min

good <- left_join(good1min,good5min)

## bad
bad1min <- read_excel(file_path, sheet = "Bad", col_names = FALSE) 
bad5min <- read_excel(file_path, sheet = "Bad_5", col_names = FALSE) 
colnames(bad1min) <- companies %>% pull(`...1`)
colnames(bad5min) <- companies %>% pull(`...1`)



bad1min <- 
  bad1min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "bad1"
  )
bad1min

bad5min <- 
  bad5min %>% 
  mutate(
    date = dates$...1,
    date = as.Date(date, format="%d-%b-%Y")
  ) %>% 
  select(date,everything()) %>% 
  pivot_longer(
    cols = AAPL:WMT, 
    names_to = "Symbol", 
    values_to = "bad5"
  )
bad5min

bad <- left_join(bad1min,bad5min)

dat <- left_join(rv,left_join(rq,left_join(bpv,left_join(good,bad))))
# do not use DOW and CRM stock as it appears first in 2019
dat <- dat %>% filter(Symbol!="DOW")
dat <- dat %>% filter(Symbol!="CRM")
dat

saveRDS(dat,"capire_clean.rds")
