#' -----------------------------------------------------------------------------
#' Here we use IMA models (which are MA(1) models to do 1-step ahead forecasts (expanding window).
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
ma1.results <- list()

for(i in 1:length(countries)){
  
  # variable of interest
  y.d.lag1 <- data.log[,i] - lag.p(data.log[,i], 1)
  names(y.d.lag1) <- rownames(data.log)
  
  # initialize
  out <- matrix(NA_real_, length(end.years), 4)
  rownames(out) <- paste0("(1960, ", end.years, ")")
  colnames(out) <- c("MA1.intercept", "MA1.coefficient", "one.step.ahead.fcast",
                     "true.value")
  
  for(j in 1:length(end.years)){
    t               <- end.years[j]
    y.d.lag1.subset <- y.d.lag1[1:which(names(y.d.lag1) == t)]
    
    set.seed(713254)
    ma.obj <- stats::arima(y.d.lag1.subset,
                           order = c(0, 0, 1), include.mean = TRUE)
    
    ma.intercept  <- ma.obj$coef["intercept"]
    ma.coef       <- ma.obj$coef["ma1"]
    
    # perform 1-step ahead forecast
    ma.1stepfcast <- predict(ma.obj)$pred
    
    # evaluate the forecast
    true.value <- y.d.lag1[which(names(y.d.lag1) == t + 1)]
    
    # store results
    out[j, "MA1.intercept"]        <- ma.intercept
    out[j, "MA1.coefficient"]      <- ma.coef
    out[j, "one.step.ahead.fcast"] <- as.numeric(ma.1stepfcast)
    out[j, "true.value"]           <- as.numeric(true.value)
    
  } # END end.years
  
  ma1.results[[i]] <- out
  
} # END countries

# store results
names(ma1.results) <- countries
save(ma1.results, 
     file = paste0(getwd(),
                   "/R/benchmark-models/ima/ima-results.Rdata"))
