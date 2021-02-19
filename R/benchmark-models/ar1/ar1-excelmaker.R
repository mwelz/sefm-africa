#' -----------------------------------------------------------------------------
#' Returns a spreadsheet of the results of "ar1.R
#' 
#' Author: mwelz
#' Last changed: Feb. 19, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load list of results (a list called "ar1.results")
load(paste0(getwd(), "/R/benchmark-models/ar1/ar1-results.Rdata"))
countries <- names(ar1.results)

# declare the spreadsheet
file <- paste0(getwd(), "/R/benchmark-models/ar1/ar1-results.xlsx")

# Initial sheet
readme <- "Here we used AR(1) models to perform 1-step ahead forecasts (expanding window). Estimation is done by OLS and an intercept is used. The first column of each sheet contains the sample that was used for estimation."
xlsx::write.xlsx(readme, file = file, sheetName = "README",
                 append = FALSE, row.names = FALSE, col.names = FALSE)

# gradually append the spreadsheet
for(i in 1:length(countries)){
  
  xlsx:: write.xlsx(ar1.results[[i]], 
                    file = file, 
                    sheetName = countries[i], 
                    append = TRUE, 
                    row.names = TRUE, 
                    col.names = TRUE)
  
}
