#' -------------------------------------------------------------------------------
#' Here we perform the Johansen cointegration tests to find cointegration relationships between African countries.
#' 
#' Author: mwelz
#' Last changed: Feb 19, 2021
#' -------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014") 

# load helper functions
source(paste0(getwd(), "/R/cointegration/cointegration-finder/helper-functions.R"))
  
### 0.1 Load & prepare data, being GDP index with 1960 = 100 ----
data           <- read.csv(paste0(getwd(), "/data/processed/africa_gdp_index.csv"))
data           <- data[1:57,] 
rownames(data) <- data[,1]

# take logs
data      <- log(data[,-1]) 
countries <- colnames(data)

# some helper onjects
N       <- ncol(data)
T       <- nrow(data)
RESULTS <- list()
N.set   <- 1:N

### 0.2 Hyperparameters ----
tau    <- 0.4 
tau.pc <- 0   # for the positive correlations
tau.nc <- 0   # for the negative correlations
K.pc   <- 3   # the _K_ strongest positive correlations to be considered
K.nc   <- 3   # the _K_ strongest negative correlations to be considered

i <-  which(countries == 'GHANA')

for(i in N.set){ 
  ### Step 1 ----
  # Retain countries for a country i
  crdw.i      <- rep(NA, N)
  cors        <- rep(NA, N)
  names(cors) <- N.set
  for(j in N.set){
    if(j == i) next
    resid <- ols(as.numeric(data[,j]), as.numeric(data[,i]))$resids
    rho   <- autocorr.p(resid, 1)
    crdw.i[j] <- 2 * (1 - rho)
    
    ## This is actually step 3 already:
    dy.i <- as.numeric(data[,i] - lag.p(data[,i],1))
    y.j.lag <- lag.p(as.numeric(data[,j]),1)
    dy.j <- y.j.lag - lag.p(y.j.lag,1)
    dy.i <- dy.i[-c(1,2)] # TODO: drop 2nd or last?
    dy.j <- dy.j[-c(1,2)]
    cors[j] <- cor(dy.i, dy.j)
  } # end j
  
  R.i.set <- c(i, which(crdw.i > tau)) # all retained countries (including i itself).
  #rm(resid, rho, crdw.i, j)
  
  ### Step 2 ----
  data.smp <- as.matrix(data[,R.i.set])
  
  johansen.try <- try(urca::ca.jo(data.smp), silent = TRUE)
  if(isTRUE(class(johansen.try) == "try-error")){
    txt <- paste0(countries[i],': No cointegration relationship found for these hyperparameters!')
    RESULTS[[i]] <- txt
    next
  } else{
    johansen <- johansen.try
  }
  coint <- as.numeric(data.smp %*% johansen@V[,1]) # 1st column is cointegration relationship
  eigvals <- as.numeric(johansen@lambda)
  J.i.set <- R.i.set[-1]
  J <- data[,J.i.set]
  
  ### Step 3 ----
  C.pos <- which(cors > tau.pc)
  C.neg <- which(cors < tau.nc)
  
  ### Step 4 ----
  pc.set <- as.integer(names(sort(cors[C.pos], decreasing = TRUE)))[1:K.pc]
  nc.set <- as.integer(names(sort(cors[C.neg], decreasing = FALSE)))[1:K.nc]
  P.mat <- data[,pc.set]
  N.mat <- data[,nc.set]
  
  ### Step 5 ----
  #J.i.set <- which(colnames(data) %in% c('CHAD', 'MADAGASCAR')) 
  #pc.set <- which(colnames(data) %in% c('TANZANIA', 'GUINEA', 'ETHIOPIA', 'CHAD')) 
  #nc.set <- which(colnames(data) %in% c('CONGOREPUB', 'SOUTHAFRICA', 'GABON', 'COMOROS')) 
  J.i.mat <- get.Jmat(data, J.i.set)
  J.len <- length(J.i.set)
  
  ### Organize everything in a list:
  pos.corr <- cors[pc.set] ; names(pos.corr) <- countries[pc.set]
  neg.corr <- cors[nc.set] ; names(neg.corr) <- countries[nc.set]
  joh      <- johansen@V[-1,1]  ; names(joh) <- countries[J.i.set]
  
  lst.out <- list(
    pos.corr = pos.corr,
    neg.corr = neg.corr,
    johansen = joh,
    johansen.eigenvalues = eigvals,
    coint.variable = stats::ts(coint, start = c(1960,1), end = c(2016,1), frequency = 1),
    hyperparams = list(tau = tau, tau.pc = tau.pc, tau.nc = tau.nc, K.pc = K.pc, K.nc = K.nc)
  )
  RESULTS[[i]] <- lst.out
}

names(RESULTS) <- countries

# save everything
save(RESULTS, file = paste0(getwd(), "/R/cointegration/cointegration-finder/cointegrations-raw.Rdata"))
