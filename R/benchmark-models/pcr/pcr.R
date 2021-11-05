#' -----------------------------------------------------------------------------
#' Here we use principal component regression to do 1-step ahead forecasts (expanding window).
#' 
#' Author: mwelz
#' Last changed: Nov 5, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load helper functions
source(paste0(getwd(), "/R/cointegration/cointegration-finder/helper-functions.R"))

# load & prepare data, being GDP index with 1960 = 100 ----
data           <- read.csv(paste0(getwd(), "/data/processed/africa_gdp_index.csv"))
data           <- data[1:57,] 
rownames(data) <- data[,1]

# take logs
data.log       <- log(data[,-1]) # drop uninformative 1st column
countries      <- colnames(data.log)

# the end years of the expanding window, one at a time
end.years <- c(2010, 2011, 2012, 2013, 2014, 2015)

# initialize list to store everything
pcr.results <- list()

# country indices
country.idxs <- 1:length(countries)

# prepare regressor matrix X.lag that contains lagged growth rates
X <- sapply(country.idxs, function(j)  data.log[,j] - lag.p(data.log[,j], 1) )
rownames(X) <- rownames(data.log)
colnames(X) <- colnames(data.log)
X.lag <- sapply(country.idxs, function(j) lag.p(X[,j], p = 1))
colnames(X.lag) <- colnames(X)
rownames(X.lag) <- rownames(X)



for(i in country.idxs){
  
  # variable of interest
  y.d.lag1 <- data.log[,i] - lag.p(data.log[,i], 1)
  names(y.d.lag1) <- rownames(data.log)
  
  # initialize
  out <- matrix(NA_real_, length(end.years), ncol(data.log) + 3)
  rownames(out) <- paste0("(1960, ", end.years, ")")
  colnames(out) <- c(paste0("coef_",colnames(data.log), "_lag"),
                     "adjR2", "one.step.ahead.fcast", "true.value")
  
  for(j in 1:length(end.years)){
    
    # prepare data
    t       <- end.years[j]
    idx.t   <- which(names(y.d.lag1) == t)
    subset  <- 1:idx.t
    
    # the regressor matrix also contains first lag of the dependent variable 
    data.t  <- data.frame(y = y.d.lag1, X.lag)[subset,] 
    colnames(data.t)[-1] = paste0(colnames(data.t)[-1], "_lag")
    
    # remove the two years lost to lagging
    data.t  <- data.t[-c(1:2),]

    # perform PCR
    pcr.mod <- pls::pcr(y~., data = data.t, center = TRUE, scale = TRUE, validation = "none")
    
    # it's unclear how we can extract eigenvalues from pcr object, so compute them separately
    eigvalues <- eigen(cor(data.t[,-1]))$values # drop dependent variable
   
    # retain variables according to the Kaiser criterion
    num.retained <- max(which(eigvalues >= 1))
    
    # calculate adjusted R^2
    val <- pls::mvrValstats(pcr.mod, estimate = "train", ncomp = num.retained)
    r2  <- as.numeric(1 - val$SSE[,,2] / val$SST)
    r2adj <- 1 - (1 - r2) * (nrow(data.t) - 1) / (nrow(data.t) - num.retained)
    
    # predict y for period t+1 by only using information of period t
    pcr.pred <- predict(pcr.mod, X.lag[idx.t,,drop = FALSE], ncomp = num.retained)[,,1]
    
    # evaluate the forecast
    true.value <- y.d.lag1[idx.t + 1]
    
    # extract the coefficients
    cf <- coefficients(pcr.mod, ncomp = num.retained)[,,1]

    # store results
    out[j, country.idxs]           <- unname(cf)
    out[j, "adjR2"]                <- r2adj
    out[j, "one.step.ahead.fcast"] <- unname(pcr.pred)
    out[j, "true.value"]           <- unname(true.value)
    
  } # END end.years
  
  pcr.results[[i]] <- out
  
} # END countries

# store results
names(pcr.results) <- countries
save(pcr.results, 
     file = paste0(getwd(),
                   "/R/benchmark-models/pcr/pcr-results.Rdata"))
