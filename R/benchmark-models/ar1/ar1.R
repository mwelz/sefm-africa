#' -----------------------------------------------------------------------------
#' Here we use AR(1) models to do 1-step ahead forecasts (expanding window).
#' Estimation is done by OLS.
#' 
#' Author: mwelz
#' Last changed: Feb. 19, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

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
  series <- stats::ts(data.log[,i], start = c(1960,1), end = c(2016,1), frequency = 1)
  
  # initialize
  out <- matrix(NA_real_, length(end.years), 5)
  rownames(out) <- paste0("(1960, ", end.years, ")")
  colnames(out) <- c("AR1.intercept", "AR1.coefficient", "one.step.ahead.fcast",
                     "true.value", "squared.fcast.error")

  for(j in 1:length(end.years)){
    series.subset <- stats::window(series, end = c(end.years[j],1))
    
    # fit AR(1) model with intercept by OLS
    ar.mod        <- ar.ols(series.subset, order.max = 1, intercept = TRUE)
    ar.intercept  <- ar.mod$x.intercept
    ar.coef       <- as.numeric(ar.mod$ar)
    
    # perform 1-step ahead forecast
    ar.1stepfcast <- ar.intercept +
      ar.coef * series.subset[length(series.subset)]
    
    # evaluate the forecast
    true.value <- as.numeric(stats::window(series, 
                                           start = c(end.years[j] + 1, 1), 
                                           end   = c(end.years[j] + 1, 1)))
    
    # store results
    out[j, "AR1.intercept"]     <- ar.intercept
    out[j, "AR1.coefficient"]   <- ar.coef
    out[j, "one.step.ahead.fcast"]  <- ar.1stepfcast
    out[j, "true.value"]          <- true.value
    out[j, "squared.fcast.error"] <- (ar.1stepfcast - true.value)^2
    
  } # END end.years
  
  ar1.results[[i]] <- out
  
} # END countries

# store results
names(ar1.results) <- countries
save(ar1.results, 
     file = paste0(getwd(),
                   "/R/benchmark-models/ar1/ar1-results.Rdata"))
