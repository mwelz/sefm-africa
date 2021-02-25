#' -----------------------------------------------------------------------------
#' Here we use AR(1) models to do 1-step ahead forecasts (expanding window).
#' Estimation is done by OLS.
#' 
#' Author: mwelz
#' Last changed: Feb. 25, 2021.
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
ar1.results <- list()

for(i in 1:length(countries)){
  
  # variable of interest
  y.d.lag1 <- data.log[,i] - lag.p(data.log[,i], 1)
  names(y.d.lag1) <- rownames(data.log)

  # initialize
  out <- matrix(NA_real_, length(end.years), 4)
  rownames(out) <- paste0("(1960, ", end.years, ")")
  colnames(out) <- c("AR1.intercept", "AR1.coefficient", "one.step.ahead.fcast",
                     "true.value")

  for(j in 1:length(end.years)){
    t               <- end.years[j]
    y.d.lag1.subset <- y.d.lag1[1:which(names(y.d.lag1) == t)]

    # fit AR(1) model with intercept by OLS (first observation is NA due to lagging, so drop it)
    ar.mod        <- ar.ols(y.d.lag1.subset[-1], order.max = 1, intercept = TRUE,
                            demean = FALSE, aic = FALSE)

    ar.intercept  <- ar.mod$x.intercept
    ar.coef       <- as.numeric(ar.mod$ar)
    
    # perform 1-step ahead forecast
    ar.1stepfcast <- predict(ar.mod)$pred
      
    # evaluate the forecast
    true.value <- y.d.lag1[which(names(y.d.lag1) == t + 1)]
    
    # store results
    out[j, "AR1.intercept"]        <- ar.intercept
    out[j, "AR1.coefficient"]      <- ar.coef
    out[j, "one.step.ahead.fcast"] <- as.numeric(ar.1stepfcast)
    out[j, "true.value"]           <- as.numeric(true.value)

  } # END end.years
  
  ar1.results[[i]] <- out
  
} # END countries

# store results
names(ar1.results) <- countries
save(ar1.results, 
     file = paste0(getwd(),
                   "/R/benchmark-models/ar1/ar1-results.Rdata"))
