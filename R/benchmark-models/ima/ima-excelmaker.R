#' -----------------------------------------------------------------------------
#' Returns a spreadsheet of the results of "ima.R"
#' 
#' Author: mwelz
#' Last changed: Feb. 25, 2021.
#' ------------------------------------------------------------------------------
rm(list = ls()) ; cat("\014")

# load list of results (a list called "ar1.results")
load(paste0(getwd(), "/R/benchmark-models/ima/ima-results.Rdata"))
countries <- names(ma1.results)

# declare the spreadsheet
file <- paste0(getwd(), "/R/benchmark-models/ima/ima-results.xlsx")

# Initial sheet
readme <- "Here we used MA(1) models to perform 1-step ahead forecasts (expanding window). An intercept is used. The first column of each sheet contains the sample that was used for estimation."
xlsx::write.xlsx(readme, file = file, sheetName = "README",
                 append = FALSE, row.names = FALSE, col.names = FALSE)

# gradually append the spreadsheet
for(i in 1:length(countries)){
  
  xlsx:: write.xlsx(ma1.results[[i]], 
                    file = file, 
                    sheetName = countries[i], 
                    append = TRUE, 
                    row.names = TRUE, 
                    col.names = TRUE)
  
}
