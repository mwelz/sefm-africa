#' ------------------------------------------------------------------------------------------
#' Makes a nice xlsx file that summarizes the Johansen cointegration relationships that were found in "cointegration-finder.R".
#' 
#' Author: mwelz 
#' Last changed: Mar 13, 2021
#' -------------------------------------------------------------------------------------------
rm(list = ls()) ; cat('\014')
library(xlsx)

# load list "RESULTS" which was returned by "cointegration-finder.R"
load(paste0(getwd(), "/R/cointegration/cointegration-finder/cointegrations-raw.Rdata"))

# declare file path
file <- paste0(getwd(), "/R/cointegration/summaries/cointegration-summary.xlsx")

# the countries
countries <- names(RESULTS)

## Initial sheet
readme <- unlist(RESULTS[[1]]$hyperparams)
readme <- rbind(names(readme), readme)
readme <- cbind(c("Hyperparameters", ""), readme)
rownames(readme) <- colnames(readme) <- NULL
write.xlsx(readme, file = file, sheetName = "README",
           append = FALSE, row.names = FALSE, col.names = FALSE)

#i = 9 # i=9 isCAR, where no Johansen

for(i in 1:length(countries)){ 
  results.i <- RESULTS[[i]]
  
  # If no Johansen cointegration relationship was found:
  if("STOP" %in% names(results.i)){
    
    RES <- matrix("", 10, max(results.i$hyperparams$K.pc, results.i$hyperparams$K.nc ) + 1)
    RES[1,1] <- results.i$STOP
    RES[4,] <- c("Positive Correlations", names(results.i$pos.corr))
    RES[5,] <- c("", round(results.i$pos.corr, 4))
    
    RES[8,] <- c("Negative Correlations", names(results.i$neg.corr))
    RES[9,] <- c("", round(results.i$neg.corr, 4))
    
    write.xlsx(RES, file=file, sheetName=countries[i], append=TRUE, row.names=FALSE, col.names=FALSE)
    next
  }
  
  # Johansen coeffs
  joh <- results.i$johansen
  joh <- rbind(names(joh), round(joh,4)) ; rownames(joh) <- NULL ; colnames(joh) <- NULL
  joh <- cbind(c('Johansen Cointegration Relationships', 'Coefficient'), joh)
  
  # correlations
  pos.cor <- results.i$pos.corr
  pos.cor <- rbind(names(pos.cor), round(pos.cor,4)) ; rownames(pos.cor) <- NULL ; colnames(pos.cor) <- NULL
  pos.cor <- cbind(c('Positive Correlations', ''), pos.cor)
  neg.cor <- results.i$neg.corr
  neg.cor <- rbind(names(neg.cor), round(neg.cor,4)) ; rownames(neg.cor) <- NULL ; colnames(neg.cor) <- NULL
  neg.cor <- cbind(c('Negative Correlations', ''), neg.cor)
 
  # hyperparams
  hyperparameters <- unlist(results.i$hyperparams)
  hyperparameters <- rbind(names(hyperparameters), hyperparameters)
  rownames(hyperparameters) <- colnames(hyperparameters) <- NULL
  hyperparameters <- cbind(c('Hyperparameters', ''), hyperparameters)
  
  # Johansen eigenvalues
  eigvals <- results.i$johansen.eigenvalues
  eigvals <- c("Johansen Eigenvalues", eigvals)
  
  # put it all together:
  n.row <- nrow(joh) + nrow(pos.cor) + nrow(neg.cor) + 10
  n.col <- max(ncol(joh), ncol(pos.cor), ncol(neg.cor), length(eigvals))
  
  RES <- matrix('', n.row, n.col)
  RES[1:nrow(joh), c(1:ncol(joh))] <- joh 
  ct <- length(1:nrow(joh))
  RES[ct+3, 1:length(eigvals)] <- eigvals
  ct <- ct + 3
  RES[(ct+3):(ct+3+nrow(pos.cor)-1), c(1:ncol(pos.cor))] <- pos.cor
  ct <- (ct+3+nrow(pos.cor)-1)
  RES[(ct+3):(ct+3+nrow(neg.cor)-1), c(1:ncol(neg.cor))] <- neg.cor
 
  # save it
  write.xlsx(RES, file=file, sheetName=countries[i], append=TRUE, row.names=FALSE, col.names=FALSE)
}
