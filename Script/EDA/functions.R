get_hd <- function(a, b){
  # a is a vector of 2 longitudes, b is a vector of 2 latitudes
  library(geosphere)
  dx <- distm(c(a[1], b[1]), c(a[2], b[1]), fun = distHaversine)*(-1)^(a[1] < a[2])
  dy <- distm(c(a[1], b[1]), c(a[1], b[2]), fun = distHaversine)*(-1)^(b[1] < b[2])
  
  return (list('dx' = dx, 'dy' = dy))
}

get_dx <- function(a, b){
  library(geosphere)
  dx <- distm(c(a[1], b[1]), c(a[2], b[1]), fun = distHaversine)*(-1)^(a[1] < a[2])
  
  return (dx)
}

get_dy <- function(a, b){
  library(geosphere)
  dy <- distm(c(a[1], b[1]), c(a[1], b[2]), fun = distHaversine)*(-1)^(b[1] < b[2])
  
  return (dy)
}

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

plotTraj <- function(seal){
  par(mfrow = c(2, 1), mar = c(3.5, 3.5, 0.5, 0.5), mgp = c(2, 0.5, 0))
  plot(seal$Date, seal$Longitude, xlab = '', ylab = "Longitude",
       pch = seq(1, 7)[as.numeric(seal$LocationQuality)])
  legend('topleft', c('0', '1', '2', '3', 'A', 'B'), pch = seq(2, 7), cex = 0.5)
  lines(seal$Date, seal$LonPred, col = 'red', type = 'p', pch = 16, cex = 0.8)
  
  plot(seal$Date, seal$Latitude, xlab = "Time", ylab = "Latitude",
       pch = seq(1, 7)[as.numeric(seal$LocationQuality)])
  legend('topleft', c('0', '1', '2', '3', 'A', 'B'), pch = seq(2, 7), cex = 0.5)
  lines(seal$Date, seal$LatPred, col = 'red', type = 'p', pch = 16, cex = 0.8)
  
  library(ggplot2)
  ggplot(seal, aes(x = Date, y = Longitude, col = LocationQuality)) + 
    geom_point() + 
    theme(legend.title = element_blank())
    
}

getTbl <- function(seal.out){
  seal.out.tbl <- rbind(seal.out$beta0, seal.out$psi, seal.out$p, seal.out$c, seal.out$sigma2)
  post.mn <- apply(seal.out.tbl, 1, mean)
  post.ci <- apply(seal.out.tbl, 1, function(x) quantile(x, c(0.025, 0.975)))
  seal.out.tbl <- data.frame(rbind(post.mn, post.ci), row.names = c('mean', '2.5%', '97.5%'))
  colnames(seal.out.tbl) <- c('beta0lon', 'beta0lat', 'psi', 'p', 'c', 's1', 's2', 's3', 's4', 's5', 's6')
  
  return (seal.out.tbl)
}

# pdf('expectedW.pdf')
# par(mfrow = c(1, 1))
# plot(seal1$Date, apply(seal1.out$W, 1, mean), ylim = c(0, 1), xlab = 'Time (t)', ylab = 'Expected w(t)', cex = 0.8)
# lines(seal2$Date, apply(seal2.out$W, 1, mean), type = 'p', pch = 2, col = 'blue', cex = 0.8)
# lines(seal3$Date, apply(seal3.out$W, 1, mean), type = 'p', pch = 3, col = 'brown', cex = 0.8)
# abline(h = 0.5, col = 'red')
# legend('bottomright', c('PV98TUG13', 'PV98TUG14', 'PV98TUG18'), pch = c(1, 2, 3), col = c('black', 'blue', 'brown'))
# dev.off()