#' --------------------------------------------------------------------
#' We evaluate the autocorrelation of the time-series-residuals of African countries, obtained from the SEFM models. Concretely, these are the models with sample period 1960 to 2015. All countries have NA for 1960 and some also for 1961, due to lagging (in some, only the 1st lag was excluded in the model building process). Data obtained from "africa_gdp_index.wf1". Note: since some SEFM models use 2nd lags, some residual series also have NAs for 1962.
#' 
#' TODO: Angola and Botswana are incorrect, redo them.
#' Last changed: Feb 18, 2021.
#' Author: mwelz
#' -------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load the time-series residuals of the SEFM models
resids.raw       <- read.csv(file = paste0(getwd(), "/EViews/summaries/sefm-residuals.csv"), 
                             header = TRUE)
resids           <- resids.raw
rownames(resids) <- resids[, "yr"]
resids           <- resids[,-1]

# drop the currently erroneous Angola and Botswana
resids <- resids[,-which(colnames(resids) == "angola")]
resids <- resids[,-which(colnames(resids) == "botswana")]

# prepare autocorrelation analysis
p <- ncol(resids) # number of countries
autocorr.mat <- matrix(NA_real_, 4, p)
colnames(autocorr.mat) <- toupper(colnames(resids))
rownames(autocorr.mat) <- c("Ljung-Box statistic with lag order 1", "Ljung-Box p-value", 
                            "Reject H0 at 95% confidence?", "Number time series observations")
autocorr.mat <- as.data.frame(autocorr.mat)

for(j in 1:p){
  
  # make residuals of given country a time series object
  resids.ts <- ts(resids[,j], 
    start = c(1961, 1), end = c(2015, 1), frequency = 1)
  
  # test for autocorrelation of lag order 1 by using the Ljung-Box test
  ljung.box.test     <- stats::Box.test(resids.ts, lag = 1, type = "Ljung-Box")
  autocorr.mat[1, j] <- ljung.box.test$statistic
  autocorr.mat[2, j] <- ljung.box.test$p.value
  autocorr.mat[3, j] <- ljung.box.test$p.value <= 0.05
  autocorr.mat[4, j] <- length(na.omit(resids.ts))
  
} # FOR

# save the autocorrelation tests
write.csv(t(autocorr.mat), 
          file =  paste0(getwd(), 
                         "/R/sefm-results-analysis/residual-autocorrelation/residual-autocorrelation.csv"))
