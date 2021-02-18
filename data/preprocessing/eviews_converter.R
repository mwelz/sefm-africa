#install.packages('hexView')
library(hexView)

setwd('/Users/mwelz/Documents/work/ra_franses/2019/wk17/data')
filename <- 'africa gdp with imputed data.wf1'

df <- readEViews(filename, time.stamp = TRUE, as.data.frame = TRUE)
rownames(df) <- df[,1]
df <- df[,-1]
options(scipen=999) # no scientific notation
df <- round(df, 4)

# Make level data
df <- read.csv('/Users/mwelz/Documents/work/ra_franses/2019/wk17/data/africa_gdp_growth.csv',header = TRUE) # Ghana is shifted by one row
df <- df[,-1]


levels <- matrix(NA, dim(df)[1], dim(df)[2])
g_rates <- 1 + df / 100 
lev <- 100

for(i in 1:(dim(df)[1])){
  lev <- as.numeric(lev * g_rates[i,])
  levels[i,] <- lev
}

levels <- rbind(100, levels)
rownames(levels) <- c('t0', rownames(df))
colnames(levels) <- colnames(df)
levels <- as.data.frame(levels)

#write.csv(df, "africa_gdp_growth.csv")
write.csv(levels, "africa_gdp_index.csv")
