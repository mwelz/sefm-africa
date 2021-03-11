rm(list = ls()) ; cat("\014")

# load results of the benchmark models
load(paste0(getwd(), "/R/benchmark-models/ar1/ar1-results.Rdata"))
load(paste0(getwd(), "/R/benchmark-models/ima/ima-results.Rdata"))
load(paste0(getwd(), "/R/benchmark-models/ols/ols-results.Rdata"))
load(paste0(getwd(), "/R/benchmark-models/rw/rw-results.Rdata"))

# adjust names
AR1.RESULTS <- ar1.results ; rm(ar1.results)
MA1.RESULTS <- ma1.results ; rm(ma1.results)

# load SEFM results (the 2 warnings come from the unused EViews objects C and resid)
SEFM.RESULTS <- hexView::readEViews(paste0(getwd(), "/EViews/africa_gdp_index.wf1"))

# list of all countries
countries <- names(AR1.RESULTS)

# if no Jansen cointegration relationship was found:
nls2.arr <- c("CAR", "CONGODR", "EQGUINEA", "ETHIOPIA", "LIBERIA", "LIBYA", "ZIMBABWE")

# if NLS did not converge, we use OLS instead:
ols.arr <- c("ANGOLA", "CABOVERDE", "ERITREA", "GHANA", "MAURITANIA", "MAURITIUS", "SENEGAL", "SUDAN", "TOGO", "UGANDA")

# the forecast years
years.fcast <- c(2011, 2012, 2013, 2014, 2015, 2016)

models <- c("SEFM", "OLS", "AR1", "MA1", "RW")
FCASTS.ls <- list()
winner.mat <- matrix(0, length(countries), length(models))
rownames(winner.mat) <- countries
colnames(winner.mat) <- models
perf.eval <- as.data.frame(matrix(NA_real_, length(countries), 11))
models.me <- paste0(models, "_ME")
models.mae <- paste0(models, "_MAE")
colnames(perf.eval) <- c("Estimation_Type", models.me, models.mae)
rownames(perf.eval) <- countries

for(i in 1:length(countries)){
  
  # load the forecasts
  ar1 <- AR1.RESULTS[[i]]
  ma1 <- MA1.RESULTS[[i]]
  
  # OLS forecasts
  if(is.list(OLS.RESULTS[[i]])){
    
    # OLS could be performed since there were cointegration relationships
    ols <- sapply(2010:2015, function(yr){
      OLS.RESULTS[[i]][[paste0("smpl1960to", yr)]]$`one-step-ahead-fcast`
    })
    
  } else {
    
    # set to NA if no OLS could be performed due to lack of cointegration relationships
    ols <- matrix(NA_real_, 2, 6)
    rownames(ols) <- c("fcast", "truth")
  }
  ols <- t(ols)
  rownames(ols) <- years.fcast
  
  # RW forecast
  rw <- sapply(2010:2015, function(yr){
    RW.RESULTS[[i]][[paste0("smpl1960to", yr)]]$`one-step-ahead-fcast`
  })
  rw <- t(rw)
  rownames(rw) <- years.fcast
  
  # prepare object to store results in
  out.i <- matrix(NA_real_, length(years.fcast), 6)
  rownames(out.i) <- years.fcast
  colnames(out.i) <- c("Truth", models) 
  
  # SEFM
  if(countries[i] %in% ols.arr){
    
    # case 1: SEFM did not converge, so we use OLS model instead
    out.i[, "SEFM"]   <- ols[,"fcast"]
    
  } else{
    
    # case 2: SEFM converged
    sefm <- SEFM.RESULTS[[paste0("SEFM_", countries[i], "_1STEP")]] 
    names(sefm) <- 1960:2020
    out.i[, "SEFM"]  <- sefm[names(sefm) %in% years.fcast]
    
  } # IF
  
  out.i[, "OLS"]   <- ols[,"fcast"]
  out.i[, "Truth"] <- rw[,"truth"]
  out.i[, "AR1"]   <- ar1[,"one.step.ahead.fcast"]
  out.i[, "MA1"]   <- ma1[,"one.step.ahead.fcast"]
  out.i[, "RW"]    <- rw[,"fcast"]
  
  # get root mean squared prediction error
  RMSE <- sapply(models, function(x){
    sqrt(mean((out.i[, "Truth"] - out.i[, x])^2)) 
  })
  
  # determine the winner:
  winner <- names(sort(RMSE, decreasing = FALSE, na.last = NA)[1])
  
  if((countries[i] %in% ols.arr) & (winner %in% c("SEFM", "OLS"))){
    
    # handling ties that can happen if SEFM = OLS
    winner.mat[i, "SEFM"] <- 1
    
  } else{
    
    # assign 1 to the winning model
    winner.mat[i, winner] <- 1
    
  } # END IF
  
  # include zero in RMSE to account for the true value
  RMSE <- c(0, RMSE)
  
  # assign results
  FCASTS.ls[[i]] <- rbind(out.i, RMSE)
  
  ## other performance measures
  # MAE (multiply by 100 to make it growth rates)
  mae <- sapply(models, function(x){
      mean(abs(100 * out.i[, "Truth"] - 100 * out.i[, x])) 
  })
  
  # Mean Error (ME)
  me <- sapply(models, function(x){
    mean(100 * out.i[, "Truth"] - 100 * out.i[, x]) 
  })
  
  # assign the MAE and ME 
  perf.eval[i, models.mae] <- mae
  perf.eval[i, models.me]  <- me
  
  if(countries[i] %in% ols.arr){
    type <- "OLS"
  } else if(countries[i] %in% nls2.arr){
    type <- "NLS w/o relations"
  } else{
    type <- "NLS"
  }
  perf.eval[i, "Estimation_Type"] <- type
  
} # FOR

names(FCASTS.ls) <- countries

# overview of the winners
winner.overview <- rbind(winner.mat, PROPORTION.WIN = colMeans(winner.mat))

# declare the spreadsheet
file <- paste0(getwd(), "/R/summary/summary.xlsx")

# Initial sheet
readme <- "Here we used several models to perform 1-step ahead forecasts (expanding window). In the next tab you find a summary of the winning frequency of each model. That is, the model with the lowest RMSE across the six 1-step-ahead forecasts for 2011 to 2016."

# readme
xlsx::write.xlsx(readme, file = file, sheetName = "README",
                 append = FALSE, row.names = FALSE, col.names = FALSE)

# winners
xlsx::write.xlsx(winner.overview, file = file, sheetName = "WINNERS",
                 append = TRUE, row.names = TRUE, col.names = TRUE)

# gradually append the spreadsheet
for(i in 1:length(countries)){
  
  xlsx:: write.xlsx(FCASTS.ls[[i]], 
                    file = file, 
                    sheetName = countries[i], 
                    append = TRUE, 
                    row.names = TRUE, 
                    col.names = TRUE)
  
}

# prepare the CSV for the performance evaluations
perf.eval.out <- perf.eval[, c("Estimation_Type", "SEFM_ME", "SEFM_MAE", "MA1_ME", "MA1_MAE", "AR1_ME", "AR1_MAE", "OLS_ME", "OLS_MAE", "RW_ME", "RW_MAE")]

write.csv(perf.eval.out, file = paste0(getwd(), "/R/summary/performances.csv"))
