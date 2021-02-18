#' ------------------------------------------------------------------------------------------
#' Makes a nice xlsx file that summarizes the Johansen cointegration relationships that were found in "cointegration-finder.R".
#' 
#' Author: mwelz 
#' Last changed: Feb 18, 2021
#' -------------------------------------------------------------------------------------------
rm(list = ls()) ; cat('\014')
library(xlsx)

# load list "RESULTS" which was returned by "cointegration-finder.R"
load(paste0(getwd(), "/R/cointegration/cointegration-finder/cointegrations-raw.Rdata"))

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
  if(!is.list(results.i)){
    write.xlsx(results.i, file=file, sheetName=countries[i], append=TRUE, row.names=FALSE, col.names=FALSE)
    next
  }
  
  # Johansen coeffs
  joh <- results.i$johansen
  joh <- rbind(names(joh), round(joh,4)) ; rownames(joh) <- NULL ; colnames(joh) <- NULL
  joh <- cbind(c('Johansen', ''), joh)
  
  # correlations
  pos.cor <- results.i$pos.corr
  pos.cor <- rbind(names(pos.cor), round(pos.cor,4)) ; rownames(pos.cor) <- NULL ; colnames(pos.cor) <- NULL
  pos.cor <- cbind(c('Positive Correlations', ''), pos.cor)
  neg.cor <- results.i$neg.corr
  neg.cor <- rbind(names(neg.cor), round(neg.cor,4)) ; rownames(neg.cor) <- NULL ; colnames(neg.cor) <- NULL
  neg.cor <- cbind(c('Negative Correlations', ''), neg.cor)
  
  # convergence diagnostics
  convergence <- matrix(NA, 3, 4)
  convergence[1,] <- c('', 'SSR', 'Iterations', 'Converged?')
  convergence[,1] <- c('', 'BFGS', 'Gauss-Newton')
  convergence[c(2:3),2] <- c(results.i$ssr.bfgs, results.i$ssr.gm)
  convergence[c(2:3),3] <- c(results.i$iters$bfgs, results.i$iters$gm)
  convergence[c(2:3),4] <- c(results.i$convergence.bfgs, results.i$convergence.gm)
  
  # hyperparams
  hyperparameters <- unlist(results.i$hyperparams)
  hyperparameters <- rbind(names(hyperparameters), hyperparameters)
  rownames(hyperparameters) <- colnames(hyperparameters) <- NULL
  hyperparameters <- cbind(c('Hyperparameters', ''), hyperparameters)
  
  
  # estimation results
  res.bfgs <- round(results.i$results.bfgs, 6)
  res.bfgs <- cbind(rownames(res.bfgs), res.bfgs)
  res.bfgs <- rbind(colnames(res.bfgs), res.bfgs)
  res.bfgs[1,1] <- 'BFGS Results'
  colnames(res.bfgs) <- rownames(res.bfgs) <- NULL
  
  res.gn <- round(results.i$results.gm, 6)
  res.gn <- cbind(rownames(res.gn), res.gn)
  res.gn <- rbind(colnames(res.gn), res.gn)
  res.gn[1,1] <- 'Gauss-Newton Results'
  colnames(res.gn) <- rownames(res.gn) <- NULL
  
  # put it all together:
  n.row <- nrow(joh) + nrow(pos.cor) + nrow(neg.cor) + nrow(convergence) + nrow(res.bfgs) + nrow(res.bfgs) + 10
  n.col <- max(ncol(joh), ncol(pos.cor), ncol(neg.cor), ncol(convergence), ncol(convergence), ncol(res.bfgs))
  
  RES <- matrix('', n.row, n.col)
  RES[1:nrow(joh), c(1:ncol(joh))] <- joh 
  ct <- length(1:nrow(joh))
  RES[(ct+3):(ct+3+nrow(pos.cor)-1), c(1:ncol(pos.cor))] <- pos.cor
  ct <- (ct+3+nrow(pos.cor)-1)
  RES[(ct+3):(ct+3+nrow(neg.cor)-1), c(1:ncol(neg.cor))] <- neg.cor
  ct <- (ct+3+nrow(neg.cor)-1)
  RES[(ct+3):(ct+3+nrow(convergence)-1), c(1:ncol(convergence))] <- convergence
  ct <- (ct+3+nrow(convergence)-1)
  RES[(ct+3):(ct+3+nrow(res.bfgs)-1), c(1:ncol(res.bfgs))] <- res.bfgs
  ct <- (ct+3+nrow(res.bfgs)-1)
  RES[(ct+3):(ct+3+nrow(res.gn)-1), c(1:ncol(res.gn))] <- res.gn
  ct <- (ct+3+nrow(res.gn)-1)
  
  write.xlsx(RES, file=file, sheetName=countries[i], append=TRUE, row.names=FALSE, col.names=FALSE)
}


