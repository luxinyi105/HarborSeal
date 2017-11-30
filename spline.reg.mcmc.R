get.full <- function(x){
  x.f <- rbind(c(x, rep(0, length(x))), c(rep(0, length(x)), x))
  return (x.f)
}

spline.reg.mcmc <- function(seal, n.iter, lambda){
  ####
  #### Setup Basis Expansion (X)
  ####
  
  times <- seal$Date
  N <- length(times)
  df1 <- 3
  df2 <- 3*4
  tmp1.bs <- scale(bs(times, df = df1, intercept = FALSE))
  tmp2.bs <- scale(bs(times, df = df2, intercept = FALSE))
  X <- cbind(tmp1.bs, tmp2.bs)
  
  ####
  #### Setup Variables
  ####
  
  S <- seal[, c("Longitude", "Latitude")]
  J <- 6
  M <- ncol(X)
  H <- matrix(c(1, 0, 0, -1), nrow = 2, byrow = TRUE)
  n.burn <- round(n.iter/10)
  beta0.save <- matrix(NA, nrow = 2, ncol = n.iter)
  beta.save <- matrix(NA, nrow = 2*M, ncol = n.iter)
  sigma2.save <- matrix(NA, nrow = J, ncol = n.iter)
  p.save <- rep(NA, n.iter)
  c.save <- rep(NA, n.iter)
  psi.save <- rep(NA, n.iter)
  W.save <- matrix(NA, nrow = N, ncol = n.iter)
  S.pred.save <- array(NA, c(2, N, n.iter))
  D.bar.save <- rep(NA, n.iter)
  mse.save <- rep(NA, n.iter)
  ind1 <- which(seal$LocationQuality == '0')
  ind2 <- which(seal$LocationQuality == '1')
  ind3 <- which(seal$LocationQuality == '2')
  ind4 <- which(seal$LocationQuality == '3')
  ind5 <- which(seal$LocationQuality == 'A')
  ind6 <- which(seal$LocationQuality == 'B')
  ind.ls <- list(ind1, ind2, ind3, ind4, ind5, ind6)
  
  ####
  #### Priors and Starting Values
  ####
  
  c <- 0.5
  p <- 0.5
  R <- matrix(c(1, sqrt(c)*p, sqrt(c)*p, c), nrow = 2, byrow = TRUE)
  psi <- 0.5
  mu0 <- c(mean(seal$Longitude), mean(seal$Latitude))
  Sigma0 <- 0.1*diag(2)
  Sigma.beta <- lambda*diag(2*M)
  a.1 <- 5
  a.2 <- 1 
  a.c <- 5
  b.c <- 5
  a.p <- 5
  b.p <- 5
  a.psi <- 5
  b.psi <- 5
  sig2.p.tune <- 0.05
  sig2.c.tune <- 0.05
  beta0 <- mu0
  beta <- rep(0, 2*ncol(X))
  sigma2 <- rep(1, J)
  sigma2.v <- rep(1, N)
  set.seed(1234)
  W <- rbinom(N, 1, 0.5)
  R.v <- lapply(W, function(w) R*w + (H%*%R%*%t(H))*(1 - w))
  
  ####
  #### MCMC
  ####
  
  set.seed(1234)
  for (k in 1:n.iter) {
    
    if(k%%100 == 0) cat(k, ' ')
    
    ### Sample beta0
    A <- apply(mapply(function(R, sigma2) solve(R)/sigma2, R = R.v, sigma2 = sigma2.v), 1, sum) +
      solve(Sigma0)
    b <- apply(mapply(function(s, R, sigma2, x) ((s - t(get.full(x)%*%beta))%*%solve(R))/sigma2, 
                      s = as.list(as.data.frame(t(S))), R = R.v, sigma2 = sigma2.v, x = as.list(as.data.frame(t(X)))), 1, sum) + 
      solve(Sigma0)%*%mu0
    
    beta0 <- mvrnorm(1, solve(A)%*%b, solve(A))
    beta0.save[, k] <- beta0
    
    ### Sample beta
    A <- apply(mapply(function(R, sigma2, x) t(get.full(x))%*%solve(R)%*%get.full(x)/sigma2, 
                      R = R.v, sigma2 = sigma2.v, x = as.list(as.data.frame(t(X)))), 1, sum) + solve(Sigma.beta)
    b <- apply(mapply(function(s, R, sigma2, x) t(get.full(x))%*%solve(R)%*%(s - beta0)/sigma2, 
                      s = as.list(as.data.frame(t(S))), R = R.v, sigma2 = sigma2.v, x = as.list(as.data.frame(t(X)))), 1, sum)
    beta <- mvrnorm(1, solve(A)%*%b, solve(A))
    beta.save[, k] <- beta
    
    ### Sample sigma_j^2
    for (j in 1:J){
      if (length(unlist(ind.ls[j])) == 0) {
        sigma2[j] <- 0 
      } else {
        if (length(unlist(ind.ls[j])) == 1) {
          s <- as.numeric(S[unlist(ind.ls[j]), ])
          x <- X[unlist(ind.ls[j]), ]
          R <- matrix(unlist(R.v[unlist(ind.ls[j])]), 2, 2, byrow = TRUE)
          a.1.tmp <- 1 + a.1
          a.2.tmp <- sum(t(s - beta0 - get.full(x)%*%beta)%*%solve(R)%*%(s - beta0 - get.full(x)%*%beta)) + a.2
        } else {
          s.j <- as.list(as.data.frame(t(S[unlist(ind.ls[j]), ])))
          x.j <- as.list(as.data.frame(t(X[unlist(ind.ls[j]), ])))
          R.j <- R.v[unlist(ind.ls[j])]
          a.1.tmp <- length(unlist(ind.ls[j])) + a.1
          a.2.tmp <- sum(mapply(function(s, R, x) t(s - beta0 - get.full(x)%*%beta)%*%solve(R)%*%(s - beta0 - get.full(x)%*%beta),
                                s = s.j, R = R.j, x = x.j)) + a.2
        }
        sigma2[j] <- 1/rgamma(1, shape = a.1.tmp, rate = a.2.tmp)
      }
      sigma2.v[unlist(ind.ls[j])] <- sigma2[j]
    }
    sigma2.save[, k] <- sigma2
    
    ### Sample psi
    a.psi.tmp <- sum(W) + a.psi
    b.psi.tmp <- sum(1 - W) + b.psi
    psi <- rbeta(1, a.psi.tmp, b.psi.tmp)
    psi.save[k] <- psi
    
    ### Sample W
    psi.tilde <- mapply(function(s, sigma2, x) 
      psi*dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R)/(psi*dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R) + (1 - psi)*dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*H%*%R%*%t(H))), 
      s = as.list(as.data.frame(t(S))), sigma2 = sigma2.v, x = as.list(as.data.frame(t(X))))
    W <- rbinom(N, 1, psi.tilde)
    W.save[, k] <- W
    R.v <- lapply(W, function(w) R*w + (H%*%R%*%t(H))*(1 - w))
    
    ### Sample p
    p.star <- rtruncnorm(1, a = 0, b = 1, mean = p, sd = sqrt(sig2.p.tune))
    R.star <- matrix(c(1, sqrt(c)*p.star, sqrt(c)*p.star, c), nrow = 2, byrow = TRUE)
    R.star.v <- lapply(W, function(w) R.star*w + (H%*%R.star%*%t(H))*(1 - w))
    mh1 <- sum(mapply(function(s, sigma2, R, x) log(dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R)),
                      s = as.list(as.data.frame(t(S))), sigma2 = sigma2.v, R = R.star.v, x = as.list(as.data.frame(t(X))))) +
      dbeta(p.star, a.p, b.p, log = TRUE) + log(dtruncnorm(p, a = 0, b = 1, mean = p.star, sd = sqrt(sig2.p.tune)))
    
    mh2 <- sum(mapply(function(s, sigma2, R, x) log(dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R)),
                      s = as.list(as.data.frame(t(S))), sigma2 = sigma2.v, R = R.v, x = as.list(as.data.frame(t(X))))) +
      dbeta(p, a.p, b.p, log = TRUE) + log(dtruncnorm(p.star, a = 0, b = 1, mean = p, sd = sqrt(sig2.p.tune)))
    
    mh <- exp(mh1 - mh2)
    if (runif(1) < mh) p <- p.star
    p.save[k] <- p
    
    ### Update the vector of R matrix at each time t
    R <- matrix(c(1, sqrt(c)*p, sqrt(c)*p, c), nrow = 2, byrow = TRUE)
    R.v <- lapply(W, function(w) R*w + (H%*%R%*%t(H))*(1 - w))
    
    ### Sample c
    c.star <- rtruncnorm(1, a = 0, b = 1, mean = c, sd = sqrt(sig2.c.tune))
    R.star <- matrix(c(1, sqrt(c.star)*p, sqrt(c.star)*p, c.star), nrow = 2, byrow = TRUE)
    R.star.v <- lapply(W, function(w) R.star*w + (H%*%R.star%*%t(H))*(1 - w))
    mh1 <- sum(mapply(function(s, sigma2, R, x) log(dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R)),
                      s = as.list(as.data.frame(t(S))), sigma2 = sigma2.v, R = R.star.v, x = as.list(as.data.frame(t(X))))) +
      dbeta(c.star, a.c, b.c, log = TRUE) + log(dtruncnorm(c, a = 0, b = 1, mean = c.star, sd = sqrt(sig2.c.tune)))
    
    mh2 <- sum(mapply(function(s, sigma2, R, x) log(dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R)),
                      s = as.list(as.data.frame(t(S))), sigma2 = sigma2.v, R = R.v, x = as.list(as.data.frame(t(X))))) +
      dbeta(c, a.c, b.c, log = TRUE) + log(dtruncnorm(c.star, a = 0, b = 1, mean = c, sd = sqrt(sig2.c.tune)))
    
    mh <- exp(mh1 - mh2)
    if (runif(1) < mh) c <- c.star
    c.save[k] <- c
    
    ### Update the vector of R matrix at each time t
    R <- matrix(c(1, sqrt(c)*p, sqrt(c)*p, c), nrow = 2, byrow = TRUE)
    R.v <- lapply(W, function(w) R*w + (H%*%R%*%t(H))*(1 - w))
    
    ### Prediction
    S.pred <- mapply(function(x, sigma2, R) mvrnorm(1, beta0 + get.full(x)%*%beta, sigma2*R),
                    x = as.list(as.data.frame(t(X))), sigma2 = sigma2.v, R = R.v)
    S.pred.save[, , k] <- S.pred
    
    ### Compute IC
    l1 <- mapply(function(s, x, sigma2) dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*R, log = TRUE),
           s = as.list(as.data.frame(t(S))), x = as.list(as.data.frame(t(X))), sigma2 = sigma2.v)
    l0 <- mapply(function(s, x, sigma2) dmvnorm(s, beta0 + get.full(x)%*%beta, sigma2*H%*%R%*%t(H), log = TRUE),
                 s = as.list(as.data.frame(t(S))), x = as.list(as.data.frame(t(X))), sigma2 = sigma2.v)
    D.bar.save[k] <- -2*(log(psi) + sum(l1) + log(1 - psi) + sum(l0))
    
  }
  cat('\n')
  
  ####
  #### Save samples
  ####
  beta0.save <- beta0.save[, -(1:n.burn)]
  beta.save <- beta.save[, -(1:n.burn)]
  psi.save <- psi.save[-(1:n.burn)]
  p.save <- p.save[-(1:n.burn)]
  c.save <- c.save[-(1:n.burn)]
  sigma2.save <- sigma2.save[, -(1:n.burn)]
  W.save <- W.save[, -(1:n.burn)]
  D.bar.save <- D.bar.save[-(1:n.burn)]
  mse.save <- mse.save[-(1:n.burn)]
  
  ####
  #### Compute IC
  ####
  beta0.post.mn <- mean(beta0.save)
  beta.post.mn <- apply(beta.save, 1, mean)
  psi.post.mn <- mean(psi.save)
  p.post.mn <- mean(p.save)
  c.post.mn <- mean(c.save)
  sigma2.post.mn <- apply(sigma2.save, 1, mean)
  R.post.mn <- matrix(c(1, sqrt(c.post.mn)*p.post.mn, sqrt(c.post.mn)*p.post.mn, c.post.mn), 
              nrow = 2, byrow = TRUE)
  for (j in 1:J) {
    sigma2.v[unlist(ind.ls[j])] <- sigma2.post.mn[j]
  }
  l1 <- mapply(function(s, x, sigma2) dmvnorm(s, beta0.post.mn + get.full(x)%*%beta.post.mn, sigma2*R.post.mn, log = TRUE),
               s = as.list(as.data.frame(t(S))), x = as.list(as.data.frame(t(X))), sigma2 = sigma2.v)
  l0 <- mapply(function(s, x, sigma2) dmvnorm(s, beta0.post.mn + get.full(x)%*%beta.post.mn, sigma2*H%*%R.post.mn%*%t(H), log = TRUE),
               s = as.list(as.data.frame(t(S))), x = as.list(as.data.frame(t(X))), sigma2 = sigma2.v)
  D.hat <- -2*(log(psi.post.mn) + sum(l1) + log(1 - psi.post.mn) + sum(l0))
  D.bar <- mean(D.bar.save)
  pD <- D.bar - D.hat
  DIC <- D.hat + 2*pD
  
  ####
  #### Write output
  ####
  mcmc.out <- list(beta0 = beta0.save, beta = beta.save, psi = psi.save, p = p.save, 
                   c = c.save, sigma2 = sigma2.save, W = W.save, S.pred = S.pred.save,
                   DIC = DIC)
  
  return (mcmc.out)
}