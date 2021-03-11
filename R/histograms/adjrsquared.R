# Returns a histogram of the adjusted R^2 for the SEFM models with estimation sample 1960-2015 (Figure 3 in the paper)
rm(list = ls()) ; cat("\014")

# load adjusted R^2 data for OLS (ols.adjr2)
load(paste0(getwd(), "/R/benchmark-models/ols/ols-adjr2.Rdata"))

# the countries
countries <- colnames(ols.adjr2)

# we only consider the period 1960 to 2015
ols.adjr2 <- ols.adjr2["smpl1960to2015",]

# load adjusted R^2 data for SEFM
adjr2.sefm.df <- read.csv(file = paste0(getwd(), "/EViews/summaries/sefm-adjrsquared.csv"))
adjr2.sefm <- as.numeric(adjr2.sefm.df)
names(adjr2.sefm) <- toupper(colnames(adjr2.sefm.df))
rm(adjr2.sefm.df)

# countries for which the SEFM is an OLS model
ols.arr <- c("ANGOLA", "CABOVERDE", "ERITREA", "GHANA", "MAURITANIA", "MAURITIUS", "SENEGAL", "SUDAN", "TOGO", "UGANDA")

# initialize final array
adjr2 <- rep(NA_real_, length(countries))
names(adjr2) <- countries

# countries for which the SEFM is the OLS model
adjr2[ols.arr] <- ols.adjr2[ols.arr]

# countries for which the SEFM is an NLS model
adjr2[names(adjr2.sefm)] <- adjr2.sefm

# make a histogram
library(ggplot2)

pdf(file = paste0(getwd(), "/R/histograms/plots/SEFM_adjustedRsquared.pdf"))
gg <- ggplot(data.frame(adjr2 = adjr2), aes(x = adjr2)) + 
  geom_histogram(color = "black", fill = "gray", bins = 10) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14))
print(gg)
dev.off()

# some summary statistics
sort(adjr2) # max RWANDA (0.7095), min SOMALIA (0.0385)
mean(adjr2) # 0.353
