rm(list=ls())
needed.packages <- c("chron","lubridate","zoo","xts","data.table","highfrequency",
			"timeSeries","quantmod","ggplot2","dplyr","MASS")
#lapply(needed.packages, install.packages, character.only = TRUE)
lapply(needed.packages, require, character.only = TRUE)

# Set the seed for reproducibility
set.seed(123)

library(JumpTest)

# SV1FJ model simulation
# M number of time points within each interval
# m number of intervals to be simulated
# p0 start price
# lam frequency of jump
# sig std of jumpsize
# mu drift
# v0 volatility parameter
# beta0 underlying Brownian motion intercept parameter
# beta1 underlying Brownian motion slope parameter
# alphav volatility parameter
# cov Brownian motion correlation (rho)

lam <- 0.058
sig <- 1
alphav <- -0.1
M <- 390

SV1FJ <- function(M,m,p0=3,lam=.2,sig=2,mu=.03,v0=.5,beta0=0,beta1=.125,alphav=-.1,cov=-.62){
  n <- M*m
  dt <- 1/M
  z <- mvrnorm(n,rep(0,2),matrix(c(1,cov,cov,1),2,2))
  n1 <- rpois(n,lam*dt)
  jl <- which(n1>0)
  M0 <- sapply(n1[jl],rnorm,n=1,sd = sig)
  M <- rep(0,n)
  M[jl] <- M0
  pp <- pvc(n, p0, mu*dt, beta0, beta1, v0, sqrt(dt), 1+alphav*dt, z, M)
  return(cbind(pp[-1], n1))
}

pvc <- function(n, p0, mt, beta0, beta1, v0, st, vxs, z, m) {
    .Call('_JumpTest_pvc', PACKAGE = 'JumpTest', n, p0, mt, beta0, beta1, v0, st, vxs, z, m)
}

T <- 5000 + 1

p <- SV1FJ(M=M,m=T, lam=lam, sig=sig, alphav=alphav)

#ts.plot(p[,1])

p.day <- matrix(c(0,diff(p[,1])), nrow=M)

rv <- rRVar(p.day, makeReturns = FALSE)
rv <- rv[-1]
ts.plot(rv, xlab = "Date", ylab = "Realized Variance", type = "l")


# Define the start and end times for a single day
start_time <- "09:31:00"
end_time <- "16:00:00"

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

p.mat <- data.table(DT,c(0,diff(p[,1])))
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
Symbol <- "SV1FJ"

# rows: days, colums: measures
dat <- data.frame(date,Symbol,rv,rq,good,bad)

