#' ------------------------------------------------------------------------------------------------
#' Input: The original dataset of annual GDP level growth in Africa
#' Output: A csv file of annual GDP growth, but as index, where 1960 is taken as base year.
#'        That is, for each country, the GDP growth of 1960 is set to 100.
#'        Please note: the years 2017-2020 (inclusive) are uninformative, because they are all equal
#'        
#' Author: mwelz
#' Last changed: February 23, 2021 
#' ------------------------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# read raw data
df <- read.csv(paste0(getwd(), "/data/raw/africa_gdp_growth.csv"), 
               header = TRUE) # Ghana is shifted by one row

# drop uninformative first column
df <- df[, -1]
rownames(df) <- as.character(1961:(1961 + nrow(df) - 1))

# make index data with 1960 = 100
levels <- matrix(NA, dim(df)[1], dim(df)[2])
g_rates <- 1 + df / 100 
lev <- 100

for(i in 1:(dim(df)[1])){
  lev <- as.numeric(lev * g_rates[i,])
  levels[i,] <- lev
}

levels <- rbind(100, levels)
rownames(levels) <- c('1960', rownames(df))
colnames(levels) <- colnames(df)
levels <- as.data.frame(levels)

# save
write.csv(levels, file = paste0(getwd(), "/data/processed/africa_gdp_index.csv"))
