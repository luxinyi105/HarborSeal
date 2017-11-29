# 11/27
# tuning lambda, violin plots of psi, animation of location difference
rm(list = ls())

library(dplyr)
library(splines)
library(ggplot2)
library(MASS)
library(mvtnorm)
library(truncnorm)
library(geosphere)

source('spline.reg.mcmc.R')
source('functions.R')
load('tune_spline_reg2.Rdata')

seal1.out <- spline.reg.mcmc(seal.ls[[1]], 10000, 0.3)
# seal2.out <- spline.reg.mcmc(seal.ls[[2]], 10000, 0.9)
# seal3.out <- spline.reg.mcmc(seal.ls[[3]], 10000, 0.1)
# seal4.out <- spline.reg.mcmc(seal.ls[[4]], 10000, 0.9)
# seal5.out <- spline.reg.mcmc(seal.ls[[5]], 10000, 0.5)
# seal6.out <- spline.reg.mcmc(seal.ls[[6]], 10000, 0.1)
# seal7.out <- spline.reg.mcmc(seal.ls[[7]], 10000, 0.7)
# seal8.out <- spline.reg.mcmc(seal.ls[[8]], 10000, 0.1)

save.image('seals_out.Rdata')

# id.ls <- c('PV98TUG01', 'PV98TUG02', 'PV98TUG04', 'PV98TUG10', 'PV98TUG11', 'PV98TUG13', 
#            'PV98TUG14', 'PV98TUG18')
# for (i in 1:length(id.ls)) {
#   jpeg(filename = paste('../../Report/rp1127/', id.ls[i], '.jpeg', sep = ''))
#   plotTraj(seal.ls[[i]])
#   dev.off()
# }
# 
# psi.df <- data.frame()
# for (i in 1:nrow(psi.save)) {
#   psi.df <- rbind(psi.df, cbind(psi.save[i, ], rep(i, ncol(psi.save))))
# }
# colnames(psi.df) <- c('psi', 'id')
# 
# library(ggplot2)
# gg <- ggplot(psi.df, aes(x = factor(id), y = psi)) +
#   geom_violin(aes(fill = factor(id))) +
#   labs(x = 'ID', y = expression(psi)) +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     legend.title = element_blank()) +
#   scale_fill_discrete(labels = id.ls)
# gg
# ggsave('violin.jpeg', gg)
# 
# seals <- data.frame()
# for (i in 1:length(seal.ls)) {
#   seals <- rbind(seals, seal.ls[[i]])
# }
# seals$dx <- mapply(function(lon, lat, lonpred, latpred) get_dx(c(lon, lonpred), c(lat, latpred)), 
#                    lon = seals$Longitude, lat = seals$Latitude, lonpred = seals$LonPred, latpred = seals$LatPred)
# seals$dy <- mapply(function(lon, lat, lonpred, latpred) get_dy(c(lon, lonpred), c(lat, latpred)), 
#                    lon = seals$Longitude, lat = seals$Latitude, lonpred = seals$LonPred, latpred = seals$LatPred)
# seals$col <- sapply(seals$DeployID, function(id) as.integer(which(id == id.ls)))
# save(seals, file = 'seals.Rdata')
# 
# library(dplyr)
# library(animation)
# from <- as.POSIXct(as.Date("1998-10-01"))
# to <- as.POSIXct(as.Date("1998-12-31"))
# saveHTML({
#   for (i in 1:(to - from)) {
#     selectedData <- seals %>% filter(Date > from + (i - 1)*24*3600) %>% filter(Date < from + i*24*3600)
#     par(mar=c(5.1, 4.1, 4.1, 8.1))
#     plot(selectedData$dx, selectedData$dy, xlim = c(-2*10^5, 2*10^5), ylim = c(-10^5, 10^5), col = selectedData$col, cex = 0.6,
#          xlab = expression(paste(Delta, 'x (m)')), ylab = expression(paste(Delta, 'y (m)')),
#          main = paste(from + (i - 1)*24*3600, 'to', from + i*24*3600))
#     abline(v = 0, col = 'blue')
#     abline(h = 0, col = 'blue')
#     legend('topright', inset = c(-0.3, 0), legend = id.ls, 
#            col = seq(1, 8), pch = rep(1, 8), cex = 0.75, xpd = TRUE)
#   }
# }, img.name = 'animation', htmlfile = 'aniLocDiff.html')