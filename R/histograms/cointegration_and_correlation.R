rm(list = ls()) ; cat("\014")

# load information on cointegration relationships ("RESULTS")
load(paste0(getwd(), "/R/cointegration/cointegration-finder/cointegrations-raw.Rdata"))

countries <- names(RESULTS)

# initialize counter matrices
coint.mat <- matrix(0, length(countries), length(countries))
rownames(coint.mat) <- colnames(coint.mat) <- countries
corrs.mat <- coint.mat
coint.overview <- data.frame(relations = rep(NA_character_, length(countries)))
rownames(coint.overview) <- countries
corr.overview <- data.frame(PosCorr = rep(NA_character_, length(countries)),
                            NegCorr = rep(NA_character_, length(countries)))
rownames(corr.overview) <- countries

for(i in 1:length(countries)){
  
  # autocorrelations
  countries.corr  <- c(names(RESULTS[[i]]$pos.corr), names(RESULTS[[i]]$neg.corr))
  corrs.mat[i, countries %in% countries.corr]  <- 1
  corr.overview[i, "PosCorr"] <- paste(names(RESULTS[[i]]$pos.corr), collapse = ", ")
  corr.overview[i, "NegCorr"] <- paste(names(RESULTS[[i]]$neg.corr), collapse = ", ")
  
  # loop stops here if there are no cointegration relationships
  if("STOP" %in% names(RESULTS[[i]])) next
  
  # cointegration relations
  countries.coint <- names(RESULTS[[i]]$johansen)
  coint.mat[i, countries %in% countries.coint] <- 1
  coint.overview[i,] <- paste(countries.coint, collapse = ", ")
}

# how often does a country appear in a cointregration relation?
coint.appearance <- colSums(coint.mat)
sort(coint.appearance) # min: Congo DR, Ethiopia, Liberia, Libya, Sao Tome, Zimbabwe (all 0). Max: CAR (38)

# make a histogram
library(ggplot2)
pdf(file = paste0(getwd(), "/R/histograms/plots/cointegration_appearance.pdf"))
gg <- ggplot(data.frame(coint.appearance = coint.appearance), aes(x = coint.appearance)) + 
  geom_histogram(color = "black", fill = "gray", bins = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) 
print(gg)
dev.off()

# how often was a country's autocorrelation among the K.nc or K.pc?
corrs.appearance <- colSums(corrs.mat)
sort(corrs.appearance) # Libya = NAMIBIA = NIGERIA = 1, Botswana = 15

pdf(file = paste0(getwd(), "/R/histograms/plots/correlationn_appearance.pdf"))
gg <- ggplot(data.frame(corrs.appearance = corrs.appearance), aes(x = corrs.appearance)) + 
  geom_histogram(color = "black", fill = "gray", bins = 15) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14)) 
print(gg)
dev.off()

# csv
write.csv(coint.overview, file = paste0(getwd(), "/R/histograms/plots/cointegraton-table.csv"))
write.csv(corr.overview, file = paste0(getwd(), "/R/histograms/plots/autocorrelations-table.csv"))
