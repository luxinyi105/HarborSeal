setwd('/Users/luxinyi/Desktop/Research/HarborSeal/Script/EDA')

library(animation)

load('dfc.Rdata')
source('functions.R')
from <- as.POSIXct(as.Date('1998-06-01'))
to <- as.POSIXct(as.Date('1999-01-01'))
satellite <- 'D'
library(dplyr)
dfcs <- dfc %>% filter(Satellite == satellite) 

saveHTML({
  for (i in 1:(to - from)) {
    selectedData <- dfcs %>% filter(Date > from + (i - 1)*24*3600) %>% filter(Date < from + i*24*3600)
    lon <- selectedData$Longitude
    lat <- selectedData$Latitude
    k <- length(lon)
    dx <- rep(NA, k*(k - 1)/2)
    dy <- rep(NA, k*(k - 1)/2)
    ct <- 1
    for (p in 1:(k - 1)){
      for (q in (p + 1):k){
        hd <- get_hd(c(lon[p], lon[q]), c(lat[p], lat[q]))
        dx[ct] <- hd$dx
        dy[ct] <- hd$dy
        ct <- ct + 1
      }
    }
    
    plot(dx, dy, xlim = c(-10^5, 10^5), ylim = c(-10^5, 10^5), col = 'red', cex = 0.6,
         xlab = expression(paste(Delta, 'x (m)')), ylab = expression(paste(Delta, 'y (m)')),
         main = paste(from + (i - 1)*24*3600, 'to', from + i*24*3600))
    abline(v = 0, col = 'blue')
    abline(h = 0, col = 'blue')
  }
}, img.name = 'animation', htmlfile = 'aniLocDiff.html')




