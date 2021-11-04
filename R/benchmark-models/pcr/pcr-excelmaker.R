#' -----------------------------------------------------------------------------
#' Returns a spreadsheet of the results of "pcr.R"
#' 
#' Author: mwelz
#' Last changed: Nov. 4, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load list of results (a list called "pcr.results")
load(paste0(getwd(), "/R/benchmark-models/pcr/pcr-results.Rdata"))
countries <- names(pcr.results)

# declare the spreadsheet
file <- paste0(getwd(), "/R/benchmark-models/pcr/pcr-results.xlsx")

# Initial sheet
readme <- "Here we used Principal Component Regression (PCR) to perform 1-step ahead forecasts (expanding window). The first column of each sheet contains the sample that was used for estimation. The following columns contain the PCR coefficent estimates and the last two columns contain the one step ahead forecast and the true value of the forecast period, respectively."
xlsx::write.xlsx(readme, file = file, sheetName = "README",
                 append = FALSE, row.names = FALSE, col.names = FALSE)

# gradually append the spreadsheet
for(i in 1:length(countries)){
  
  xlsx:: write.xlsx(pcr.results[[i]], 
                    file = file, 
                    sheetName = countries[i], 
                    append = TRUE, 
                    row.names = TRUE, 
                    col.names = TRUE)
  
}
