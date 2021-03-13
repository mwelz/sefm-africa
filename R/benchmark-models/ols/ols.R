#' -----------------------------------------------------------------------------
#' Here we use OLS models to do 1-step ahead forecasts (expanding window).
#' We use variable selection.
#' 
#' Author: mwelz
#' Last changed: Mar. 13, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load list "RESULTS" which was returned by "cointegration-finder.R"
load(paste0(getwd(), "/R/cointegration/cointegration-finder/cointegrations-raw.Rdata"))

# load helper functions
source(paste0(getwd(), "/R/cointegration/cointegration-finder/helper-functions.R"))

# Load & prepare data, being GDP index with 1960 = 100 ----
data           <- read.csv(paste0(getwd(), "/data/processed/africa_gdp_index.csv"))
data           <- data[1:57,] 
rownames(data) <- data[,1]
data           <- log(data[,-1]) # take logs and drop uninformative first column
countries      <- colnames(data)

# recover relevant hyperparameters
K.pc        <- RESULTS[[1]]$hyperparams$K.pc
K.nc        <- RESULTS[[1]]$hyperparams$K.nc
hyperparams <- RESULTS[[1]]$hyperparams

# the end years of the expanding window, one at a time
end.years    <- c(2010, 2011, 2012, 2013, 2014, 2015)
OLS.RESULTS  <- list()
significance <- 0.05 # significance level

# matrix for adjusted R^2
ols.adjr2 <- matrix(NA_real_, length(end.years), length(countries))
rownames(ols.adjr2) <- paste0("smpl1960to", end.years)
colnames(ols.adjr2) <- countries


for(i in 1:length(countries)){
  
  # skip country if no cointegration relationship was found
  if("STOP" %in% names(RESULTS[[i]])){
    OLS.RESULTS[[i]] <- RESULTS[[i]]$STOP
    next
  } # END IF
  
  
  # data for positively and negatively correlated countries
  countries.pc <- as.matrix(data[, names(RESULTS[[i]]$pos.corr)])
  countries.nc <- as.matrix(data[, names(RESULTS[[i]]$neg.corr)])
  
  # cointegration variable
  coint <- as.numeric(RESULTS[[i]]$coint.variable)
  
  ## get first difference at time t-1 for each regressor 
  # dependent variable
  y.d.lag2 <- lag.p(data[,i], 1) - lag.p(data[,i], 2) 
  
  # positively correlated countries
  pc.d.lag2 <- sapply(1:K.pc, function(j){
    lag.p(countries.pc[,j], 1) - lag.p(countries.pc[,j], 2)
  })
  colnames(pc.d.lag2) <- paste0(tolower(colnames(countries.pc)), ".d.lag2") 
  
  # negatively correlated countries
  nc.d.lag2 <- sapply(1:K.nc, function(j){
    lag.p(countries.nc[,j], 1) - lag.p(countries.nc[,j], 2)
  })
  colnames(nc.d.lag2) <- paste0(tolower(colnames(countries.nc)), ".d.lag2") 
  
  # cointegration variable
  coint.lag1 <- lag.p(coint, 1)
  
  # dependent variable
  y.d.lag1 <- data[,i] - lag.p(data[,i], 1)
  names(y.d.lag1) <- rownames(data)
  
  # gather regressors in matrix
  regressors <- cbind(coint.lag1, y.d.lag2, pc.d.lag2, nc.d.lag2)
  rownames(regressors) <- rownames(data)
  
  # adjust for OLS: to ensure balanced panel, we lose the first two periods due to previous lagging
  regressors <- regressors[-c(1,2), ]
  y.d.lag1   <- y.d.lag1[-c(1,2)]
  
  # initialize list to save in
  ols.i <- list()
  
  # run OLS as expanding winsoe
  for(t in end.years){
    
    # window period
    period <- 1:which(rownames(regressors) == t)
    
    # estimate OLS and apply t-test with White SEs
    set.seed(1)
    ols.obj0     <- lm(y.d.lag1 ~., data.frame(y.d.lag1 = y.d.lag1[period], regressors[period,]))
    ttest.pvals  <- lmtest::coeftest(ols.obj0, vcov = sandwich::vcovHC(ols.obj0, type = "HC0"))[,"Pr(>|t|)"]
    
    # model building
    retained.regressors <- ttest.pvals < significance
    retained.regressors <- retained.regressors[-1] # ignore intercept
    
    if(any(retained.regressors)){
      
      # case 1: we retained at least 1 regressor
      regressors.temp <- regressors[period, retained.regressors]
      ols.obj <- lm(y.d.lag1 ~., data.frame(y.d.lag1 = y.d.lag1[period], 
                                            regressors[period, retained.regressors]))
    } else{
      
      # case 2: no regressors were retained. Continue with original model
      retained.regressors <- rep(TRUE, length(retained.regressors))
      ols.obj <- ols.obj0
    }
    

    # predictions (recall that we lost the first two periods due to lagging)
    yhat <- c(NA_real_, NA_real_, ols.obj$fitted.values)
    names(yhat) <- 1960:t
    
    # residuals
    resids <- c(NA_real_, NA_real_, ols.obj$residuals)
    names(resids) <- 1960:t
    
    ## 1-step-ahead forecast
    regressors.hat <- regressors[which(rownames(regressors) == t + 1), retained.regressors]
    onestepfcast   <- sum(ols.obj$coefficients * c(1, regressors.hat))
    true.value     <- as.numeric(y.d.lag1[(names(y.d.lag1) == t + 1)])
    
    # store results
    ols.i[[paste0("smpl1960to", t)]][["in-sample-forecasts"]]  <- yhat
    ols.i[[paste0("smpl1960to", t)]][["residuals"]]            <- resids
    ols.i[[paste0("smpl1960to", t)]][["one-step-ahead-fcast"]] <- 
      c(fcast = onestepfcast, truth = true.value)
    
    # store adjusted R^2
    ols.adjr2[paste0("smpl1960to", t), i] <- summary(ols.obj)$adj.r.squared
    
  } # END end.years
  
  # add the hyperparams
  ols.i$hyperparams <- hyperparams
  
  # store result
  OLS.RESULTS[[i]] <- ols.i
  
} # END countries

# name the list and store it
names(OLS.RESULTS) <- countries
save(OLS.RESULTS, file = paste0(getwd(), "/R/benchmark-models/ols/ols-results.Rdata"))
save(ols.adjr2, file = paste0(getwd(), "/R/benchmark-models/ols/ols-adjr2.Rdata"))
