# Install and load necessary packages
if (!require(quantmod)) install.packages("quantmod")
if (!require(xts)) install.packages("xts")
library(quantmod)
library(xts)

# Define FRED symbols
symbols <- c("UNRATE", "GDP", "CPALTT01USM657N", "FEDFUNDS")

# Fetch and store data in a list
data_list <- list()
for (symbol in symbols) {
  data_list[[symbol]] <- getSymbols(
    symbol,
    src = "FRED",
    from = "1990-01-01",
    to = "2005-12-31",
    auto.assign = FALSE
  )
}

# Convert data to quarterly with actual quarter-end dates
quarterly_data_list <- lapply(data_list, function(x) {
  # Convert to quarterly
  quarterly_xts <- to.quarterly(x, OHLC = FALSE)
  # Adjust index to use quarter-end dates
  index(quarterly_xts) <- as.Date(index(quarterly_xts))
  return(quarterly_xts)
})

# Merge all quarterly data into a single object
quarterly_data <- do.call(merge, quarterly_data_list)

# View the data
head(quarterly_data)

# Optionally, save to a CSV file
write.csv(as.data.frame(quarterly_data), "fred_quarterly_data_with_dates.csv")
data <-quarterly_data

#1st differentiating 
# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

library(ggplot2)
library(gridExtra)
library(forecast)
library(tseries)

# Define the analyze_stationarity function
analyze_stationarity <- function(time_series, title) {
  # Convert to time series if not already
  if (!inherits(time_series, "ts")) {
    time_series <- ts(time_series)
  }
  
  # ADF test for stationarity
  adf_test <- adf.test(time_series)
  
  # Use the known critical value (-2.85 for 5% significance level)
  adf_stat <- round(adf_test$statistic, 2)
  critical_value <- -2.85
  
  # Calculate Standard Deviation in Levels and First Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)
  diff_ts <- diff(time_series)
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Original Time Series
  p1 <- autoplot(ts(time_series)) + 
    ggtitle(paste("Original Data -", title)) + 
    ylab("Values") + xlab("Time")
  
  # Plot ACF
  p2 <- ggAcf(time_series, lag.max = 24) + 
    ggtitle(paste("ACF of", title))
  
  # Plot PACF
  p3 <- ggPacf(time_series, lag.max = 24) + 
    ggtitle(paste("PACF of", title))
  
  # Display ADF test results, SD in levels, and SD in differences as a text plot
  adf_plot <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = adf_result, size = 5, hjust = 0.5) + 
    ggtitle(paste("ADF Test Result -", title)) + 
    theme_void()  # Blank plot with text
  
  # Combine all plots into one page
  grid.arrange(p1, p2, p3, adf_plot, ncol = 2)
}

# Assuming 'data' is your data frame containing the time series
# Convert it to a time series object if necessary
data <- ts(data, start = c(1990, 1), frequency = 4)  # Quarterly data starting in 1990

# Iterate over each column in the data and apply the function
for (col_name in colnames(data)) {
  cat("\nAnalyzing stationarity for:", col_name, "\n")
  analyze_stationarity(data[, col_name], col_name)
}
#1ST Differentiating
# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

library(ggplot2)
library(gridExtra)
library(forecast)
library(tseries)

# Define the analyze_stationarity function
analyze_stationarity <- function(time_series, title) {
  # Convert to time series if not already
  if (!inherits(time_series, "ts")) {
    time_series <- ts(time_series)
  }
  
  # ADF test for stationarity
  adf_test <- adf.test(time_series)
  
  # Use the known critical value (-2.85 for 5% significance level)
  adf_stat <- round(adf_test$statistic, 2)
  critical_value <- -2.85
  
  # Calculate Standard Deviation in Levels and First Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)
  diff_ts <- diff(time_series)
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Original Time Series
  p1 <- autoplot(ts(time_series)) + 
    ggtitle(paste("Original Data -", title)) + 
    ylab("Values") + xlab("Time")
  
  # Plot ACF
  p2 <- ggAcf(time_series, lag.max = 24) + 
    ggtitle(paste("ACF of", title))
  
  # Plot PACF
  p3 <- ggPacf(time_series, lag.max = 24) + 
    ggtitle(paste("PACF of", title))
  
  # Display ADF test results, SD in levels, and SD in differences as a text plot
  adf_plot <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = adf_result, size = 5, hjust = 0.5) + 
    ggtitle(paste("ADF Test Result -", title)) + 
    theme_void()  # Blank plot with text
  
  # Combine all plots into one page
  grid.arrange(p1, p2, p3, adf_plot, ncol = 2)
}

# Assuming `data` is your dataset containing UNRATE, GDP, and FEDFUNDS columns
# Convert them into time series objects
unrate_ts <- ts(data[,"UNRATE"], start = c(1990, 1), frequency = 4)
gdp_ts <- ts(data[,"GDP"], start = c(1990, 1), frequency = 4)
fedfunds_ts <- ts(data[,"FEDFUNDS"], start = c(1990, 1), frequency = 4)

# Apply first differencing to each series
diff_unrate <- diff(unrate_ts)  # First differencing for UNRATE
diff_gdp <- diff(gdp_ts)        # First differencing for GDP
diff_fedfunds <- diff(fedfunds_ts)  # First differencing for FEDFUNDS

# Analyze stationarity after first differencing
cat("\nAnalyzing stationarity for: Differenced UNRATE\n")
analyze_stationarity(diff_unrate, "First Differenced UNRATE")

cat("\nAnalyzing stationarity for: Differenced GDP\n")
analyze_stationarity(diff_gdp, "First Differenced GDP")

cat("\nAnalyzing stationarity for: Differenced FEDFUNDS\n")
analyze_stationarity(diff_fedfunds, "First Differenced FEDFUNDS")

# Optionally save the transformed series to a new dataset
transformed_data <- cbind(
  Differenced_UNRATE = diff_unrate,
  Differenced_GDP = diff_gdp,
  Differenced_FEDFUNDS = diff_fedfunds
)

# Save to CSV for future analysis
write.csv(as.data.frame(transformed_data), "first_differenced_data.csv")
#2nd differentiation of GDP
# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(forecast)) install.packages("forecast")
if (!require(tseries)) install.packages("tseries")

library(ggplot2)
library(gridExtra)
library(forecast)
library(tseries)

# Define the analyze_stationarity function
analyze_stationarity <- function(time_series, title) {
  # Convert to time series if not already
  if (!inherits(time_series, "ts")) {
    time_series <- ts(time_series)
  }
  
  # ADF test for stationarity
  adf_test <- adf.test(time_series)
  
  # Use the known critical value (-2.85 for 5% significance level)
  adf_stat <- round(adf_test$statistic, 2)
  critical_value <- -2.85
  
  # Calculate Standard Deviation in Levels and Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)
  diff_ts <- diff(time_series)
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Original Time Series
  p1 <- autoplot(ts(time_series)) + 
    ggtitle(paste("Original Data -", title)) + 
    ylab("Values") + xlab("Time")
  
  # Plot ACF
  p2 <- ggAcf(time_series, lag.max = 24) + 
    ggtitle(paste("ACF of", title))
  
  # Plot PACF
  p3 <- ggPacf(time_series, lag.max = 24) + 
    ggtitle(paste("PACF of", title))
  
  # Display ADF test results, SD in levels, and SD in differences as a text plot
  adf_plot <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = adf_result, size = 5, hjust = 0.5) + 
    ggtitle(paste("ADF Test Result -", title)) + 
    theme_void()  # Blank plot with text
  
  # Combine all plots into one page
  grid.arrange(p1, p2, p3, adf_plot, ncol = 2)
}

# Assuming `data` is your dataset containing the GDP column
# Convert GDP to a time series object
gdp_ts <- ts(data[,"GDP"], start = c(1990, 1), frequency = 4)

# Apply second differencing to the GDP series
second_diff_gdp <- diff(diff(gdp_ts))  # First differencing twice

# Analyze stationarity after second differencing
cat("\nAnalyzing stationarity for: Second Differenced GDP\n")
analyze_stationarity(second_diff_gdp, "Second Differenced GDP")

# Optionally save the second differenced series to a dataset
transformed_data <- data.frame(Second_Differenced_GDP = second_diff_gdp)

#Qn4
# Install and load necessary packages
if (!require(vars)) install.packages("vars")
if (!require(knitr)) install.packages("knitr")
if (!require(kableExtra)) install.packages("kableExtra")

library(vars)
library(knitr)
library(kableExtra)

# Prepare the data
stationary_data <- cbind(
  GDP = diff(diff(data[, "GDP"])),               # Second-differenced GDP
  FEDFUNDS = diff(data[, "FEDFUNDS"]),           # First-differenced FEDFUNDS
  UNRATE = diff(data[, "UNRATE"]),               # First-differenced UNRATE
  CPALTT01USM657N = data[, "CPALTT01USM657N"]    # Original stationary series
)

# Remove NAs introduced by differencing
stationary_data <- na.omit(stationary_data)

# Determine the optimal lag order using AIC with a max lag of 5
lag_selection <- VARselect(stationary_data, lag.max = 5, type = "const")
best_lag <- lag_selection$selection["AIC(n)"]

cat("Best lag order based on AIC (Max Lag = 5):", best_lag, "\n")

# Fit the VAR model with the best lag order
var_model <- VAR(stationary_data, p = best_lag, type = "const")

# Extract coefficients and statistics into a user-friendly table
coefficients_table <- do.call(rbind, lapply(names(var_model$varresult), function(dv) {
  res <- var_model$varresult[[dv]]  # Get regression result for dependent variable
  coef_summary <- coef(summary(res))  # Extract coefficients and their statistics
  data.frame(
    DependentVariable = rep(dv, nrow(coef_summary)),  # Add dependent variable info
    Variable = rownames(coef_summary),               # Independent variables
    Estimate = round(coef_summary[, "Estimate"], 4),
    StdError = round(coef_summary[, "Std. Error"], 4),
    tValue = round(coef_summary[, "t value"], 4),
    PrT = signif(coef_summary[, "Pr(>|t|)"], 4)
  )
}))

# Add a row for the AIC value in the report
aic_row <- data.frame(
  DependentVariable = "Overall Model",
  Variable = "AIC",
  Estimate = round(AIC(var_model), 4),
  StdError = NA,
  tValue = NA,
  PrT = NA
)
coefficients_table <- rbind(coefficients_table, aic_row)

# Display coefficients and statistics as a user-friendly table
coefficients_table %>%
  kbl(caption = "VAR Model Coefficients and Statistics (Including AIC, Max Lag = 5)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

# Print AIC value of the selected model in the console for reference
cat("AIC of the best VAR model (Max Lag = 5):", AIC(var_model), "\n")

# Optional: Save the table to a CSV file
write.csv(coefficients_table, "var_model_coefficients_with_aic_maxlag5.csv", row.names = FALSE)


#QN6
# Generate impulse response functions for shocks to CPI and GDP
irf_results <- irf(
  var_model,
  impulse = c("CPALTT01USM657N", "GDP"),  # Shocks: CPI and GDP
  response = c("FEDFUNDS", "UNRATE"),     # Responses: Fed Funds and Unemployment
  n.ahead = 12,                           # Forecast horizon: 12 periods
  boot = TRUE,                            # Bootstrap confidence intervals
  ci = 0.95                               # 95% confidence interval
)

# Set up a 2x2 plot layout
par(mfrow = c(2, 2))

# Plot each impulse-response function with confidence intervals
plot(irf_results, 
     main = "Fed Funds Rate Response to CPI Shock",
     impulse = "CPALTT01USM657N", response = "FEDFUNDS")

plot(irf_results, 
     main = "Unemployment Rate Response to CPI Shock",
     impulse = "CPALTT01USM657N", response = "UNRATE")

plot(irf_results, 
     main = "Fed Funds Rate Response to GDP Shock",
     impulse = "GDP", response = "FEDFUNDS")

plot(irf_results, 
     main = "Unemployment Rate Response to GDP Shock",
     impulse = "GDP", response = "UNRATE")

