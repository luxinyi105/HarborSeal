setwd("/Users/luxinyi/Desktop/Research/HarborSeal/Script/EDA")
rm(list = ls())

df <- read.csv("../../Data/1997-1999 Tugidak_081916-Argos.csv")

## Missing data
df$DeployID[df$DeployID == ''] <- NA
df$LocationQuality[df$LocationQuality == ''] <- NA
df$Satellite[df$Satellite == ''] <- NA
sum(is.na(df$MsgCount))
sum(is.na(df$Latitude))
sum(is.na(df$Longitude))
sum(is.na(df$LocationQuality))

## Complete cases, convert date to POSIX
dfc <- df[, c('DeployID', 'Date', 'LocationQuality', 'Longitude', 'Latitude', 'Satellite')]
dfc <- dfc[complete.cases(dfc), ]
dfc$Date <- as.POSIXct(dfc$Date, tz = 'UTC', '%m/%d/%y %H:%M')
save(dfc, file = 'dfc.Rdata')

## Count by Argos category
table(dfc$LocationQuality)

## Location difference in haversine distance
library(geosphere)
lon <- dfc$Longitude[1:100]
lat <- dfc$Latitude[1:100]

get_hd <- function(a, b){
  # a is a vector of 2 longitudes, b is a vector of 2 latitudes
  dx <- distm(c(a[1], b[1]), c(a[2], b[1]), fun = distHaversine)*(-1)^(a[1] < a[2])
  dy <- distm(c(a[1], b[1]), c(a[1], b[2]), fun = distHaversine)*(-1)^(b[1] < b[2])
  
  return (list('dx' = dx, 'dy' = dy))
}

k <- length(lon)
dx <- rep(NA, k*(k - 1)/2)
dy <- rep(NA, k*(k - 1)/2)
ct <- 1
for (i in 1:(k - 1)){
  for (j in (i + 1):k){
    hd <- get_hd(c(lon[i], lon[j]), c(lat[i], lat[j]))
    dx[ct] <- hd$dx
    dy[ct] <- hd$dy
    ct <- ct + 1
  }
}

plot(dx, dy, cex = 0.5)
abline(h = 0, col = 'blue')
abline(v = 0, col = 'blue')

## Map locations by seal ID
pal <- colorFactor(palette = topo.colors(28) , levels = levels(dfc$DeployID))
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = dfc, lng = ~Longitude, lat = ~Latitude, 
                   color = ~pal(DeployID), radius = 3, stroke = FALSE, fillOpacity = 0.8)

## Trajectory plot
load('dfc.Rdata')
seals <- levels(dfc$DeployID)
library(dplyr)
s1 <- dfc %>% filter(DeployID == seals[4])

library(ggplot2)
gg <- ggplot(s1, aes(x = Date, y = Longitude, shape = LocationQuality)) +
  geom_point() 
gg

ggplot(s1, aes(x = Date, y = Latitude, shape = LocationQuality)) +
  geom_point() 

