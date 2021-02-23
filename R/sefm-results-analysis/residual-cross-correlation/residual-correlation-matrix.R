#' ---------------------------------------------------------------------------------
#' We evaluate the cross correlations of the time-series-residuals of African countries, obtained from the SEFM models. Concretely, these are the models with sample period 1960 to 2015. All countries have NA for 1960 and some also for 1961, due to lagging (in some, only the 1st lag was excluded in the model building process). Data obtained from "africa_gdp_index.wf1".
#' 
#' Last changed: Feb 23, 2021.
#' Author: mwelz
#' ---------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load the time-series residuals of the SEFM models
resids.raw       <- read.csv(file = paste0(getwd(), "/EViews/summaries/sefm-residuals.csv"), 
                             header = TRUE)
resids           <- resids.raw
rownames(resids) <- resids[, "yr"]
resids           <- resids[,-1]
colnames(resids) <- toupper(colnames(resids))

# make it a balanced panel by dropping 1961
resids <- na.omit(resids) 

# save the resulting correlation matrix
write.csv(cor(resids), file = paste0(getwd(), "/R/sefm-results-analysis/residual-cross-correlation/residual-correlation-matrix.csv"))
