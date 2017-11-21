setwd("/Users/luxinyi/Desktop/Research/HarborSeal/Script/EDA")
rm(list = ls())
library(dplyr)
library(splines)
library(ggplot2)
library(MASS)
library(mvtnorm)
library(truncnorm)
library(geosphere)

load('dfc.Rdata')
id1 <- 'PV98TUG13'
id2 <- 'PV98TUG14'
id3 <- 'PV98TUG18'
from <- as.POSIXct(as.Date("1998-07-01"))
to <- as.POSIXct(as.Date("1998-10-01"))
seal1 <- dfc %>% filter(DeployID == id1) %>% filter(Date > from & Date < to)
seal2 <- dfc %>% filter(DeployID == id2) %>% filter(Date > from & Date < to)
seal3 <- dfc %>% filter(DeployID == id3) %>% filter(Date > from & Date < to)

# ## Plot Basis Expansion
# times <- seq(from, to, by = 60*60*24)
# df1 <- 3
# df2 <- 3*4
# tmp1.bs <- scale(bs(times, df = df1, intercept = FALSE))
# tmp2.bs <- scale(bs(times, df = df2, intercept = FALSE))
# X <- cbind(tmp1.bs, tmp2.bs)
# par(mar=c(4,4,2,1), mgp = c(2, 1, 0))
# pdf('basisExpansion.pdf')
# matplot(X, type = 'l', col = c(rep(1, df1), rep(2, df2)), lty = 1, xlab = 'Day', ylab = expression(X[m](t)), cex.lab = 1.2)
# dev.off()

# ## Simulate Prior
# par(mfrow = c(1, 1))
# ### psi, p, c
# x <- seq(0, 1, by = 0.001)
# a <- 5
# b <- 5
# y <- dbeta(x, a, b)
# plot(x, y, type = 'l')
# 
# x <- seq(-1, 1, by = 0.001)
# sigma.tune <- 0.05
# y <- dnorm(x, 0, sigma.tune)
# plot(x, y, type = 'l')
# 
# ### sigma2
# library(invgamma)
# x <- seq(0, 5, by = 0.001)
# a1 <- 5
# a2 <- 1
# y <- dinvgamma(x, shape = a1, rate = a2)
# plot(x, y, type = 'l')

## MCMC
source('spline.reg.mcmc.R')
n.iter <- 100
seal <- list(seal1, seal2, seal3)
lambda <- seq(0.1, 1, by = 0.1)
DIC <- matrix(NA, nrow = 3, ncol = length(lambda))
for (i in 1:3) {
  for (j in 1:length(lambda)) {
    cat('Running model with seal ', i, ' lambda ', j, '\n')
    mcmc.out <- spline.reg.mcmc(as.data.frame(seal[i]), n.iter, lambda[j])
    DIC[i, j] <- mcmc.out$DIC
  }
}
# 5 7 3

seal1.out <- spline.reg.mcmc(seal1, 10000, 0.5)
seal2.out <- spline.reg.mcmc(seal2, 10000, 0.7)
seal3.out <- spline.reg.mcmc(seal3, 10000, 0.3)

## Diagnostic
plotBeta0 <- function(seal.out){
  par(mar = c(3.5, 3.5, 1.5, 0.5), mgp = c(2, 0.5, 0))
  layout(matrix(c(1, 1, 2, 3, 3, 4), nrow = 2, ncol = 3, byrow = TRUE))
  plot(seal.out$beta0[1, ], type = 'l', xlab = 'Iteration', ylab = expression(beta[0]),
       cex.lab = 1.5, main = 'Longitude')
  plot(density(seal.out$beta0[1, ]), xlab = expression(beta[0]), ylab = 'Density', main = '', cex.lab = 1.5)
  abline(v = mean(seal.out$beta0[1, ]), col = 'red')
  abline(v = quantile(seal.out$beta0[1, ], c(0.025, 0.975)), col = 'blue', lty = 2)

  plot(seal.out$beta0[2, ], type = 'l', xlab = 'Iteration', ylab = expression(beta[0]),
       cex.lab = 1.5, main = 'Latitude')
  plot(density(seal.out$beta0[2, ]), xlab = expression(beta[0]), ylab = 'Density', main = '', cex.lab = 1.5)
  abline(v = mean(seal.out$beta0[2, ]), col = 'red')
  abline(v = quantile(seal.out$beta0[2, ], c(0.025, 0.975)), col = 'blue', lty = 2)

}

plotPsi <- function(seal.out){
  par(mar = c(3.5, 3.5, 1.5, 0.5), mgp = c(2, 0.5, 0))
  layout(matrix(c(1, 1, 2), nrow = 1, ncol = 3, byrow = TRUE))
  plot(seal.out$psi, type = 'l', xlab = 'Iteration', ylab = expression(psi),
       cex.lab = 1.5)
  plot(density(seal.out$psi), xlab = expression(psi), ylab = 'Density', main = '', cex.lab = 1.5)
  abline(v = mean(seal.out$psi), col = 'red')
  abline(v = quantile(seal.out$psi, c(0.025, 0.975)), col = 'blue', lty = 2)
}

plotPc <- function(seal.out){
  par(mar = c(3.5, 3.5, 1.5, 0.5), mgp = c(2, 0.5, 0))
  layout(matrix(c(1, 1, 2, 3, 3, 4), nrow = 2, ncol = 3, byrow = TRUE))
  plot(seal.out$p, type = 'l', xlab = 'Iteration', ylab = expression(rho),
       cex.lab = 1.5)
  plot(density(seal.out$p), xlab = expression(rho), ylab = 'Density', main = '', cex.lab = 1.5)
  abline(v = mean(seal.out$p), col = 'red')
  abline(v = quantile(seal.out$p, c(0.025, 0.975)), col = 'blue', lty = 2)

  plot(seal.out$c, type = 'l', xlab = 'Iteration', ylab = 'c',
       cex.lab = 1.5)
  plot(density(seal.out$c), xlab = 'c', ylab = 'Density', main = '', cex.lab = 1.5)
  abline(v = mean(seal.out$c), col = 'red')
  abline(v = quantile(seal.out$c, c(0.025, 0.975)), col = 'blue', lty = 2)
}

plotTraj <- function(seal.out, seal){
  par(mfrow = c(2, 1), mar = c(3.5, 3.5, 0.5, 0.5), mgp = c(2, 0.5, 0))
  S.pred.mn <- apply(seal.out$S.pred, 2, function(x) apply(x, 1, mean))
  plot(seal$Date, seal$Longitude, xlab = '', ylab = "Longitude", ylim = c(-157, -152),
       pch = seq(1, 7)[as.numeric(seal2$LocationQuality)])
  legend('topleft', c('0', '1', '2', '3', 'A', 'B'), pch = seq(2, 7), cex = 0.5)
  lines(seal$Date, S.pred.mn[1, ], col = 'red', type = 'p', pch = 16, cex = 0.8)

  plot(seal$Date, seal$Latitude, xlab = "Time", ylab = "Latitude", ylim = c(55, 60),
       pch = seq(1, 7)[as.numeric(seal$LocationQuality)])
  legend('topleft', c('0', '1', '2', '3', 'A', 'B'), pch = seq(2, 7), cex = 0.5)
  lines(seal$Date, S.pred.mn[2, ], col = 'red', type = 'p', pch = 16, cex = 0.8)
}

getTbl <- function(seal.out){
  seal.out.tbl <- rbind(seal.out$beta0, seal.out$psi, seal.out$p, seal.out$c, seal.out$sigma2)
  post.mn <- apply(seal.out.tbl, 1, mean)
  post.ci <- apply(seal.out.tbl, 1, function(x) quantile(x, c(0.025, 0.975)))
  seal.out.tbl <- data.frame(rbind(post.mn, post.ci), row.names = c('mean', '2.5%', '97.5%'))
  colnames(seal.out.tbl) <- c('beta0lon', 'beta0lat', 'psi', 'p', 'c', 's1', 's2', 's3', 's4', 's5', 's6')

  return (seal.out.tbl)
}

seal1.out.tbl <- getTbl(seal1.out)
seal2.out.tbl <- getTbl(seal2.out)
seal3.out.tbl <- getTbl(seal3.out)

jpeg('psi13.jpeg')
plotPsi(seal1.out)
dev.off()

jpeg('predTraj13.jpeg')
plotTraj(seal1.out, seal1)
dev.off()

jpeg('psi14.jpeg')
plotPsi(seal2.out)
dev.off()

jpeg('predTraj14.jpeg')
plotTraj(seal2.out, seal2)
dev.off()

jpeg('psi18.jpeg')
plotPsi(seal3.out)
dev.off()

jpeg('predTraj18.jpeg')
plotTraj(seal3.out, seal3)
dev.off()

pdf('expectedW.pdf')
par(mfrow = c(1, 1))
plot(seal1$Date, apply(seal1.out$W, 1, mean), ylim = c(0, 1), xlab = 'Time (t)', ylab = 'Expected w(t)', cex = 0.8)
lines(seal2$Date, apply(seal2.out$W, 1, mean), type = 'p', pch = 2, col = 'blue', cex = 0.8)
lines(seal3$Date, apply(seal3.out$W, 1, mean), type = 'p', pch = 3, col = 'brown', cex = 0.8)
abline(h = 0.5, col = 'red')
legend('bottomright', c('PV98TUG13', 'PV98TUG14', 'PV98TUG18'), pch = c(1, 2, 3), col = c('black', 'blue', 'brown'))
dev.off()

library(leaflet)
combined <- rbind(seal1, seal2, seal3)
pal <- colorFactor(palette = topo.colors(length(unique(combined$DeployID))) , levels = unique(combined$DeployID))
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = combined, lng = ~Longitude, lat = ~Latitude,
                   color = ~pal(DeployID), radius = 3, stroke = FALSE, fillOpacity = 0.8)
