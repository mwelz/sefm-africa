# Some time series functions

ols <- function(X, y){
  X      <- as.matrix(X) ; y <- as.numeric(y)
  X      <- cbind(1, X)
  beta   <- as.numeric(solve(t(X) %*% X) %*% t(X) %*% y)
  resids <- y - X %*% beta
  y.hat  <- X %*% beta
  return(list(coeffs = beta, resids = as.numeric(resids), y.hat = as.numeric(y.hat)))
}

lag.p <- function(y, p){
  return(c(rep(NA, p), y)[1:length(y)])
}

d1 <- function(y){ 
  # takes 1st difference
  y - lag.p(y, 1)
}

autocorr.p <- function(y, p){
  n       <- length(y)
  gamma.0 <- var(y)
  mu      <- mean(y)
  gamma.p <- sum((y[-c(1:p)] - mu) * (y[-((n-p+1):n)] - mu)) / (n - 1)
  return(gamma.p / gamma.0)
}

get.Jmat <- function(data, J.set){
  J.mat <- sapply(1:length(J.set), function(j, J.set0=J.set, data0=data){
    lag.p(data0[,J.set0[j]])
  })
  return(J.mat)
}
