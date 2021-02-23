#' ---------------------------------------------------------------------------------
#' We evaluate the cross correlations of the time-series-residuals of African countries, obtained from the SEFM models (and the OLS models in case of non-convergence). Concretely, these are the models with sample period 1960 to 2015. All countries have NA for 1960 and some also for 1961, due to lagging (in some, only the 1st lag was excluded in the model building process). Data obtained from "africa_gdp_index.wf1".
#' 
#' Last changed: Feb 23, 2021.
#' Author: mwelz
#' ---------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")


### 0.1 initialize irregular SEFM models ----
# if no Jansen cointegration relationship was found:
nls2.arr <- c("CAR", "CONGODR", "EQGUINEA", "ETHIOPIA", "LIBERIA", "LIBYA", "ZIMBABWE")

# if NLS did not converge, we use OLS instead:
ols.arr <- c("ANGOLA", "CABOVERDE", "ERITREA", "GHANA", "MAURITANIA", "MAURITIUS", "SENEGAL", "SUDAN", "TOGO", "UGANDA")

# load the OLS residuals: OLS.RESULTS
load(paste0(getwd(), "/R/benchmark-models/ols/ols-results.Rdata"))
countries  <- names(OLS.RESULTS)
n          <- length(OLS.RESULTS)
resids.ols <- sapply(ols.arr, function(x){
  OLS.RESULTS[[x]]$smpl1960to2015$residuals
})
resids.ols <- resids.ols[-1,] # drop 1st observation to have consistent start points with NLS (NA anyway)


### 0.2 initialize regular SEFM models ----
# load the time-series residuals of the SEFM models
resids.nls.raw       <- read.csv(file = paste0(getwd(), "/EViews/summaries/sefm-residuals.csv"), 
                                 header = TRUE)
resids.nls           <- resids.nls.raw
rownames(resids.nls) <- resids.nls[, "yr"]
resids.nls           <- resids.nls[,-1]
colnames(resids.nls) <- toupper(colnames(resids.nls))


### 1. prepare input of correlation matrix ----
# document type of model
types        <- rep("NLS-1", n)
names(types) <- countries
types[countries %in% ols.arr]  <- "OLS"
types[countries %in% nls2.arr] <- "NLS-2"

# combine residuals into one matrix
resids <- as.matrix(cbind(resids.nls, resids.ols))
resids <- resids[, countries] # make it ordered alphabetically

# alter column names to reflect model that was used for estimation
colnames(resids) <- paste0(countries, " (", types, ")")

# make it a balanced panel by dropping 1961
resids <- na.omit(resids) 

# save the resulting correlation matrix
write.csv(cor(resids), file = paste0(getwd(), "/R/sefm-results-analysis/residual-cross-correlation/residual-correlation-matrix.csv"))
