rm(list = ls())

library(dplyr)
library(splines)
library(ggplot2)
library(MASS)
library(mvtnorm)
library(truncnorm)
library(geosphere)

source('spline.reg.mcmc.R')

load('dfc.Rdata')

# Set up
from <- as.POSIXct(as.Date("1998-10-01"))
to <- as.POSIXct(as.Date("1998-12-31"))
id.ls <- c('PV98TUG01', 'PV98TUG02', 'PV98TUG04', 'PV98TUG10', 'PV98TUG11', 'PV98TUG13', 
           'PV98TUG14', 'PV98TUG18')
seal.ls <- lapply(id.ls, function(id) dfc %>% filter(DeployID == id) %>% filter(Date > from & Date < to))

# MCMC
n.iter <- 5000
lambda <- seq(0.1, 1, by = 0.2)
DIC <- matrix(NA, nrow = length(seal.ls), ncol = length(lambda))
psi.save <- matrix(NA, nrow = length(seal.ls), ncol = n.iter - round(n.iter/10))
for (i in 1:length(seal.ls)) {
  lon.pred <- c()
  lat.pred <- c()
  psi.ls <- matrix(NA, nrow = length(lambda), ncol = n.iter - round(n.iter/10))
  for (j in 1:length(lambda)) {
    cat('Running model with seal ', i, ' lambda ', j, '\n')
    mcmc.out <- spline.reg.mcmc(seal.ls[[i]], n.iter, lambda[j])
    DIC[i, j] <- mcmc.out$DIC
    S.pred.mn <- apply(mcmc.out$S.pred, 2, function(x) apply(x, 1, mean))
    lon.pred <- rbind(lon.pred, S.pred.mn[1, ])
    lat.pred <- rbind(lat.pred, S.pred.mn[2, ])
    psi.ls[j, ] <- mcmc.out$psi
  }
  seal.ls[[i]]$LonPred <- lon.pred[which.min(DIC[i, ]), ]
  seal.ls[[i]]$LatPred <- lat.pred[which.min(DIC[i, ]), ]
  psi.save[i, ] <- psi.ls[which.min(DIC[i, ]), ]
}

save.image('tune_spline_reg.Rdata')
