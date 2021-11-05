#' -----------------------------------------------------------------------------
#' Here we use a random walk to do 1-step ahead forecasts (expanding window).
#' 
#' Author: mwelz
#' Last changed: Nov 5, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load helper functions
source(paste0(getwd(), "/R/cointegration/cointegration-finder/helper-functions.R"))

# Load & prepare data, being GDP index with 1960 = 100 ----
data           <- read.csv(paste0(getwd(), "/data/processed/africa_gdp_index.csv"))
data           <- data[1:57,] 
rownames(data) <- data[,1]
data           <- log(data[,-1]) # take logs and drop uninformative first column
countries      <- colnames(data)
num.times      <- nrow(data)

# the end years of the expanding window, one at a time
end.years    <- c(2010, 2011, 2012, 2013, 2014, 2015)
RW.RESULTS   <- list()

# set seed for N(0,1) noise terms
set.seed(632718273)

for(i in 1:length(countries)){
  
  # initialize RW (note y[1] = log(100) everywhere)
  y    <- rep(NA_real_, num.times)
  y[1] <- data[1, i] 
  eps  <- rnorm(num.times, 0, 1)
  
  # create RW
  for(t in 2:num.times){
    y[t] <- y[t-1] + eps[t]
  }
  names(y) <- rownames(data)
  
  # RW dependent variable
  y.d.lag1 <- y - lag.p(y, 1)
  
  # true series
  y.d.lag1.true <- data[,i] - lag.p(data[,i], 1)
  names(y.d.lag1.true) <- rownames(data)
  
  # RW forecasting
  rw.i <- list()
  for(t in end.years){
    
    # index of time period t
    t.idx <- which(names(y.d.lag1) == t)
    
    # attempt to calculate R^2 in RW; not meaningful!
    #tss <- mean((y.d.lag1.true[3:t.idx] - mean(y.d.lag1.true[3:t.idx]))^2)
    #rss <- mean((y.d.lag1.true[3:t.idx] - lag.p(y.d.lag1, 1)[3:t.idx])^2) # RW forecast for T+1 is T 
    #r2 <- 1 - rss / tss

    # 1-step-ahead forecast
    onestepfcast <- as.numeric(y.d.lag1[t.idx]) # RW forecast for T+1 is T
    true.value   <- as.numeric(y.d.lag1.true[t.idx + 1])
    
    rw.i[[paste0("smpl1960to", t)]][["one-step-ahead-fcast"]] <- 
      c(fcast = onestepfcast, truth = true.value)
    
  } # END end.years
  
  RW.RESULTS[[i]] <- rw.i
  
} # END countries

# store it
names(RW.RESULTS) <- countries
save(RW.RESULTS, file = paste0(getwd(), "/R/benchmark-models/rw/rw-results.Rdata"))
