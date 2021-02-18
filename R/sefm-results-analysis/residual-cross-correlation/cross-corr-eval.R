#' ---------------------------------------------------------------------------------
#' We evaluate the cross correlations of the time-series-residuals of African countries, obtained from the SEFM models. Concretely, these are the models with sample period 1960 to 2015. All countries have NA for 1960 and some also for 1961, due to lagging (in some, only the 1st lag was excluded in the model building process). Data obtained from "africa_gdp_index.wf1".
#' 
#' TODO: Angola and Botswana are incorrect, redo them.
#' Last changed: Feb 8, 2021.
#' Author: mwelz
#' ---------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load the time-series residuals
resids.raw <- read.csv(file = "/Users/mwelz/Documents/work/ra_franses/2019/wk33/sefm-eviews/ijf-revision-1/resid-cross-correlation/residstable.csv", header = TRUE)
resids           <- resids.raw
rownames(resids) <- resids[, "yr"]
resids           <- resids[,-1]

# drop the currently erroneous Angola and Botswana
resids <- resids[,-which(colnames(resids) == "angola")]
resids <- resids[,-which(colnames(resids) == "botswana")]
resids <- na.omit(resids) # make it a balanced panel by dropping 1961

# evaluate correlation
cross.corr <- cor(resids)
foo=abs(cross.corr) > 0.4
rowSums(foo)

write.csv(cor(resids), file = "/Users/mwelz/Documents/work/ra_franses/2019/wk33/sefm-eviews/ijf-revision-1/resid-cross-correlation/resids-crosscorr.csv")
