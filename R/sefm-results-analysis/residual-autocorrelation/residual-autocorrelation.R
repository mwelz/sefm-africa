#' --------------------------------------------------------------------
#' We evaluate the autocorrelation of the time-series-residuals of African countries, obtained from the SEFM models (and the OLS models in case of non-convergence). Concretely, these are the models with sample period 1960 to 2015. All countries have NA for 1960 and some also for 1961, due to lagging (in some, only the 1st lag was excluded in the model building process). Data obtained from "africa_gdp_index.wf1". Note: since some SEFM models use 2nd lags, some residual series also have NAs for 1962.
#' 
#' Last changed: Feb 23, 2021.
#' Author: mwelz
#' -------------------------------------------------------------------
rm(list = ls()) ; cat("\014")


### 0.1 initialize irregular SEFM models ----
# if no Jansen cointegration relationship was found:
nls2.arr <- c("CAR", "CONGODR", "EQGUINEA", "ETHIOPIA", "LIBERIA", "LIBYA", "ZIMBABWE")

# if NLS did not converge, we use OLS instead:
ols.arr <- c("ANGOLA", "CABOVERDE", "ERITREA", "GHANA", "MAURITANIA", "MAURITIUS", "SENEGAL", "SUDAN", "TOGO", "UGANDA")

# load the OLS residuals: OLS.RESULTS
load(paste0(getwd(), "/R/benchmark-models/ols/ols-results.Rdata"))
countries <- names(OLS.RESULTS)
n <- length(OLS.RESULTS)



### 0.2 initialize regular SEFM models ----
# load the time-series residuals of the SEFM models
resids.nls.raw       <- read.csv(file = paste0(getwd(), "/EViews/summaries/sefm-residuals.csv"), 
                             header = TRUE)
resids.nls           <- resids.nls.raw
rownames(resids.nls) <- resids.nls[, "yr"]
resids.nls           <- resids.nls[,-1]
colnames(resids.nls) <- toupper(colnames(resids.nls))


### 1. autocorrelation analysis ----

# prepare autocorrelation analysis
autocorr.mat <- matrix(NA_real_, 5, n)
colnames(autocorr.mat) <- toupper(countries)
rownames(autocorr.mat) <- c("Estimation type", "Ljung-Box statistic with lag order 1", "Ljung-Box p-value", 
                            "Reject H0 at 95% confidence?", "Number time series observations")
autocorr.mat <- as.data.frame(autocorr.mat)

for(j in 1:n){
  
  # country of interest
  country <- countries[j]
  
  if(country %in% ols.arr){
    
    # Case 1: estimation via OLS
    resids <- OLS.RESULTS[[country]]$smpl1960to2015$residuals
    resids <- resids[-1] # drop 1st observation to have consistent start points with NLS (NA anyway)
    type   <- "OLS"
    
  } else if(country %in% nls2.arr){
    
    #  Case 2: estimation via NLS, but without Johansen coefficients
    resids <- resids.nls[, country]
    type   <- "NLS-2"
    
  } else{
    
    # Case 3: regular estimation via NLS, so with Johansen coefficients
    resids <- resids.nls[, country]
    type   <- "NLS-1"
    
  } # END iF
  
  # make residuals of given country a time series object
  resids.ts <- ts(resids, 
    start = c(1961, 1), end = c(2015, 1), frequency = 1)
  
  # test for autocorrelation of lag order 1 by using the Ljung-Box test
  ljung.box.test     <- stats::Box.test(resids.ts, lag = 1, type = "Ljung-Box")
  autocorr.mat[1, j] <- type
  autocorr.mat[2, j] <- ljung.box.test$statistic
  autocorr.mat[3, j] <- ljung.box.test$p.value
  autocorr.mat[4, j] <- ljung.box.test$p.value <= 0.05
  autocorr.mat[5, j] <- length(na.omit(resids.ts))
  
} # FOR

# save the autocorrelation tests
write.csv(t(autocorr.mat), 
          file =  paste0(getwd(), 
                         "/R/sefm-results-analysis/residual-autocorrelation/residual-autocorrelation.csv"))
