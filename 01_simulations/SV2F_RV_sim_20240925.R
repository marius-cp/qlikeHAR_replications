rm(list=ls())
needed.packages <- c("chron","lubridate","zoo","xts","data.table","highfrequency",
			"timeSeries","quantmod","ggplot2","dplyr","MASS")
#lapply(needed.packages, install.packages, character.only = TRUE)
lapply(needed.packages, require, character.only = TRUE)

# Set the seed for reproducibility
set.seed(123)

library(JumpTest)

# SV2F model simulation
# M number of time points within each interval
# m number of intervals to be simulated
# p.0 start price
# mu drift
# v.1 volatility parameter
# v.2 volatility parameter
# beta.0 underlying Brownian motion intercept paramter
# beta.1 underlying Brownian motion slope parameter
# beta.2 underlying Brownian motion slope parameter
# alpha.1 volatility parameter
# alpha.2 volatility parameter
# beta.v2 second factor Brownian motion slope parameter
# r1 correlation to first factor
# r2 correlation to second factor

M <- 78

SV2F <- function(M,m,p.0=3,mu=.03,v.1=.5,v.2=.5,beta.0=-1.2,beta.1=.04,beta.2=1.5,alpha.1=-.00137,alpha.2=-1.386,beta.v2=.25,r1=-.3,r2=-.3){
  n <- M*m
  dt <- 1/M
  z <- mvrnorm(n,rep(0,3),matrix(c(1,r1,r2,r1,1,0,r2,0,1),3,3))
  mt <- mu*dt
  st <- sqrt(dt)
  v1xs <- 1+alpha.1*dt
  v2xs <- 1+alpha.2*dt
  pp2 <- pv2(n,mt,beta.0,beta.1,beta.2,p.0,v.1,v.2,st,z,v1xs,v2xs,beta.v2)
  return(pp2[-1])
}

pv2 <- function(n, mt, b0, b1, b2, p0, v10, v20, st, z, v1xs, v2xs, bv2) {
    .Call('_JumpTest_pv2', PACKAGE = 'JumpTest', n, mt, b0, b1, b2, p0, v10, v20, st, z, v1xs, v2xs, bv2)
}

T <- 5000 + 1

p <- SV2F(M=M,m=T)

#ts.plot(y[,1])

p.day <- matrix(c(0,diff(p)), nrow=M)

rv <- rRVar(p.day, makeReturns = FALSE)
rv <- rv[-1]
ts.plot(rv, xlab = "Date", ylab = "Realized Variance", type = "l")

# Generate the sequence for one day
if (M == 390) {
one_day_sequence <- seq(
  from = as.POSIXct("2010-01-02 09:31:00"),
  to = as.POSIXct("2010-01-02 16:00:00"),
  by = "1 min"
)
} else {
one_day_sequence <- seq(
  from = as.POSIXct("2010-01-02 09:35:00"),
  to = as.POSIXct("2010-01-02 16:00:00"),
  by = "5 min"
)
}

# Function to adjust the date part of the sequence
adjust_date <- function(date_seq, day_offset) {
  as.POSIXct(format(date_seq + days(day_offset), "%Y-%m-%d %H:%M:%S"))
}

# Generate sequences for T days
date_sequences <- lapply(0:(T-1), function(day_offset) {
  adjust_date(one_day_sequence, day_offset)
})

# Combine all sequences into one vector
DT <- do.call("c", date_sequences)

p.mat <- data.table(DT,c(0,diff(p)))
# realized semi variances:
rSVar <- rSemiCov(p.mat, makeReturns = FALSE)
good <- sapply(rSVar, function(x) x[["positive"]])
good <- good[-1]
names(good) <- NULL
bad <- sapply(rSVar, function(x) x[["negative"]])
bad <- bad[-1]
names(bad) <- NULL

# realized quarticity:
rq <- rQuar(p.mat, makeReturns = FALSE)
rq <- rq$V2[-1]

date <- seq.Date(from = as.Date("2010-01-01"), length.out = T-1, by = "day")
Symbol <- "SV2F"

# rows: days, colums: measures
dat <- data.frame(date,Symbol,rv,rq,good,bad)

