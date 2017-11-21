setwd("/Users/luxinyi/Desktop/Research/HarborSeal/Script/EDA")
rm(list = ls())
library(dplyr)
library(splines)

load('dfc.Rdata')

id <- 'PV98TUG13'
data <- dfc %>% filter(DeployID == id)
X <- scale(bs(data$Date, df = 30))
fit <- lm(Longitude ~ X, data = data)
par(mfrow = c(1, 1))
plot(data$Date, data$Longitude)
lines(data$Date, fit$fitted.values, col = 'red')

## Simulate data
N <- 50
plot(0, 0, type = "n")
S1 <- matrix(unlist(locator(n = N, type = "l", col = 1)), ncol = 2)
S2 <- jitter(S1, 1000)

layout(matrix(c(1, 1, 2, 3, 2, 3), 2, 3))
plot(S1, type = 'o', asp = TRUE)
lines(S2, type = 'o', col = 2)
plot(S1[, 1], type = 'l')
lines(S2[, 1], type = 'l', col = 2)
plot(S1[, 2], type = 'l')
lines(S2[, 2], type = 'l', col = 2)

## Fit bsplins to simulated data
df1 <- 5
df2 <- 10
df3 <- 20
N.pred <- 1000
times.pred <- seq(1, N.pred)
times <- seq(1, N)
tmp1.bs <- scale(bs(times.pred, df = df1, intercept = FALSE))
tmp2.bs <- scale(bs(times.pred, df = df2, intercept = FALSE))
tmp3.bs <- scale(bs(times.pred, df = df3, intercept = FALSE))
X.pred <- cbind(tmp1.bs, tmp2.bs, tmp3.bs)
tmp1.bs <- scale(bs(times, df = df1, intercept = FALSE), 
                 center = attributes(tmp1.bs)$`scaled:center`, 
                 scale = attributes(tmp1.bs)$`scaled:scale`)
tmp2.bs <- scale(bs(times, df = df2, intercept = FALSE),
                 center = attributes(tmp2.bs)$`scaled:center`, 
                 scale = attributes(tmp2.bs)$`scaled:scale`)
tmp3.bs <- scale(bs(times, df = df3, intercept = FALSE),
                 center = attributes(tmp3.bs)$`scaled:center`, 
                 scale = attributes(tmp3.bs)$`scaled:scale`)
X.full <- cbind(tmp1.bs, tmp2.bs, tmp3.bs)

matplot(X.pred, type = 'l', col = c(rep(1, df1), rep(2, df2), rep(3, df3)), lty = 1)
matplot(seq(1, 1000, length.out = 50), X.full, type = 'l', col = c(rep(1, df1), rep(2, df2), rep(3, df3)),
        lty = 2, add = TRUE)
abline(h = 0, col = 8, lwd = 2)