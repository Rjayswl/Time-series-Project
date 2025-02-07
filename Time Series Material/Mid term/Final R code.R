install.packages("fredr")
# Load the fredr package
library(fredr)

# Set your FRED API key
fredr_set_key("df2cfabf9e43a37548ae8c31df89b533")

# Fetch the data for each series

# 1. Consumer Price Index (CPI) - Monthly
cpi_data <- fredr(series_id = "CPIAUCSL",
                  observation_start = as.Date("2015-01-01"),  # Adjust to your time range
                  frequency = "m")  # Monthly

# 2. Mortgage Interest Rates - Monthly
mortgage_data <- fredr(series_id = "MORTGAGE30US",
                       observation_start = as.Date("2015-01-01"),  # Adjust as needed
                       frequency = "m")  # Monthly

# 3. Housing Price Index (HPI) - Monthly
hpi_data <- fredr(series_id = "CSUSHPINSA",
                  observation_start = as.Date("2015-01-01"),  # Adjust as needed
                  frequency = "m")  # Monthly

# 4. Gross Domestic Product (GDP) - Quarterly (interpolate later)
gdp_data <- fredr(series_id = "GDP",
                  observation_start = as.Date("2015-01-01"),  # Adjust as needed
                  frequency = "q")  # Quarterly data

# Convert quarterly GDP data to monthly using linear interpolation
gdp_monthly <- approx(gdp_data$date, gdp_data$value, 
                      xout = seq(min(gdp_data$date), max(gdp_data$date), by = "month"))  # Linear interpolation

# Convert interpolated GDP into a data frame
gdp_monthly_df <- data.frame(date = as.Date(gdp_monthly$x), gdp = gdp_monthly$y)

# View the data
head(cpi_data)
head(mortgage_data)
head(hpi_data)
head(gdp_monthly_df)
View(gdp_monthly_df)
# Save data to CSV files if needed
write.csv(cpi_data, "cpi_data.csv", row.names = FALSE)
write.csv(mortgage_data, "mortgage_data.csv", row.names = FALSE)
write.csv(hpi_data, "hpi_data.csv", row.names = FALSE)
write.csv(gdp_monthly_df, "gdp_monthly.csv", row.names = FALSE)
# Install required libraries if you don't have them

install.packages("pacman")
pacman::p_load(ggplot2, forecast, tseries, gridExtra)

# Function to check stationarity, calculate SD, and plot ACF, PACF
analyze_stationarity <- function(time_series, title) {
  # ADF test for stationarity
  adf_test <- adf.test(time_series)
  
  # Use the known critical value (-2.85)
  adf_stat <- round(adf_test$statistic, 2)
  critical_value <- -2.85  # Known value for 5% level
  
  # Calculate Standard Deviation in Levels and First Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)
  diff_ts <- diff(time_series)
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Simulated Data (Original Time Series)
  p1 <- autoplot(time_series) +
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

# Example usage for each of your datasets

# 1. CPI Data - Replace 'value' with the correct column in your data
cpi_ts <- ts(cpi_data$value, frequency = 12)  # Assuming monthly data
analyze_stationarity(cpi_ts, "CPI Data")

# 2. Mortgage Data - Replace 'value' with the correct column in your data
mortgage_ts <- ts(mortgage_data$value, frequency = 12)  # Assuming monthly data
analyze_stationarity(mortgage_ts, "Mortgage Data")

# 3. HPI Data - Replace 'value' with the correct column in your data
hpi_ts <- ts(hpi_data$value, frequency = 12)  # Assuming monthly data
analyze_stationarity(hpi_ts, "HPI Data")

# Step 1: Install and load the necessary libraries
pacman::p_load(dplyr, ggplot2, forecast, tseries, gridExtra)

# Step 2: Define the function to check stationarity, calculate SD, and plot ACF, PACF
analyze_stationarity <- function(time_series, title) {
  
  # ADF test for stationarity
  adf_test <- adf.test(time_series)  # Perform the Augmented Dickey-Fuller test
  
  # Use the known critical value (-2.85)
  adf_stat <- round(adf_test$statistic, 2)  # Extract and round the ADF test statistic
  critical_value <- -2.85  # Known critical value for 5% significance level
  
  # Calculate Standard Deviation in Levels and First Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)  # SD of original time series
  diff_ts <- diff(time_series)  # First-differenced time series
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)  # SD of first differences
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Simulated Data (Original Time Series)
  p1 <- autoplot(time_series) +
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

# Step 3: Convert the 'gdp' column to a time series object
# Ensure the 'gdp' column contains valid data
gdp_ts <- ts(gdp_monthly_df$gdp, start = c(2015, 1), frequency = 12)  # Start at Jan 2015, monthly data

# Step 4: Apply the function to analyze stationarity
analyze_stationarity(gdp_ts, "GDP Monthly Data")

-Differentiating time series data -------
  # Step 1: Install and load the necessary libraries
  pacman::p_load(dplyr, ggplot2, forecast, tseries, gridExtra)

# Step 2: Define the function to check stationarity, calculate SD, and plot ACF, PACF
analyze_stationarity <- function(time_series, title) {
  
  # ADF test for stationarity
  adf_test <- adf.test(time_series)  # Perform the Augmented Dickey-Fuller test
  
  # Use the known critical value (-2.85)
  adf_stat <- round(adf_test$statistic, 2)  # Extract and round the ADF test statistic
  critical_value <- -2.85  # Known critical value for 5% significance level
  
  # Calculate Standard Deviation in Levels and First Differences
  sd_levels <- round(sd(time_series, na.rm = TRUE), 2)  # SD of original time series
  diff_ts <- diff(time_series)  # First-differenced time series
  sd_diff <- round(sd(diff_ts, na.rm = TRUE), 2)  # SD of first differences
  
  # Display ADF result with SDs
  adf_result <- paste("ADF Statistic:", adf_stat,
                      "\nCritical Value (5%):", critical_value,
                      "\nSD in Levels:", sd_levels,
                      "\nSD in Differences:", sd_diff)
  
  # Plot Simulated Data (Original Time Series)
  p1 <- autoplot(time_series) +
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

# Step 3: Apply first differencing to all your datasets

# 1. CPI Data
# Convert the 'value' column to a time series object
cpi_ts <- ts(cpi_data$value, start = c(2015, 1), frequency = 12)  # Assuming monthly data
cpi_diff <- diff(cpi_ts)  # Apply first differencing
analyze_stationarity(cpi_diff, "Differenced CPI Data")  # Analyze the differenced data

# 2. Mortgage Data
# Convert the 'value' column to a time series object
mortgage_ts <- ts(mortgage_data$value, start = c(2015, 1), frequency = 12)  # Assuming monthly data
mortgage_diff <- diff(mortgage_ts)  # Apply first differencing
analyze_stationarity(mortgage_diff, "Differenced Mortgage Data")  # Analyze the differenced data

# 3. HPI Data
# Convert the 'value' column to a time series object
hpi_ts <- ts(hpi_data$value, start = c(2015, 1), frequency = 12)  # Assuming monthly data
hpi_diff <- diff(hpi_ts)  # Apply first differencing
analyze_stationarity(hpi_diff, "Differenced HPI Data")  # Analyze the differenced data

# 4. GDP Monthly Data
# Convert the 'gdp' column to a time series object
gdp_ts <- ts(gdp_monthly_df$gdp, start = c(2015, 1), frequency = 12)  # Assuming monthly data
gdp_diff <- diff(gdp_ts)  # Apply first differencing
analyze_stationarity(gdp_diff, "Differenced GDP Data")  # Analyze the differenced data

  QN6.2nd Try
  # Load necessary libraries
  library(forecast)  # for ARIMA modeling and forecasting
  library(tseries)   # for statistical tests
  
  # Assuming `gdp_diff` is the time series of differenced GDP
  # Get the length of the time series
  T <- length(gdp_diff)
  
  # Split the data into training set (first T-6 observations) and test set (last 5 observations)
  train_data <- gdp_diff[1:(T-6)]   # Training data from 1 to T-6
  test_data <- gdp_diff[(T-5):T]    # Test data from T-5 to T
  # Plot ACF (Autocorrelation Function) and PACF (Partial Autocorrelation Function)
  acf(train_data, main = "ACF of Training Data GDP")    # Helps in determining 'q' (MA terms)
  pacf(train_data, main = "PACF of Training Data GDP")  # Helps in determining 'p' (AR terms)

  Fit ARIMA(0,1,2) model
  arima_012 <- Arima(train_data, order = c(0, 1, 2))
  
  # Summary to inspect the model
  summary(arima_012)
  print(arima(012))
  arima_013 <- Arima(train_data, order = c(0, 1, 3))
  print(arima_013)
  arima_014 <- Arima(train_data, order = c(0, 1, 4))
  print(arima_014)
# Assuming your fitted models are arima_012, arima_013, and arima_014
  #Table code
  # Load necessary libraries
  library(forecast)
  library(broom)
  library(dplyr)
  library(knitr)
  library(kableExtra)
  
  # Fit ARIMA models
  arima_012 <- Arima(train_data, order = c(0, 1, 2))
  arima_013 <- Arima(train_data, order = c(0, 1, 3))
  arima_014 <- Arima(train_data, order = c(0, 1, 4))
  
  # Function to extract necessary data from each model
  extract_arima_metrics <- function(model) {
    model_metrics <- glance(model)
    
    # Extract only the numeric part of the coefficients without extra labels
    theta1 <- coef(model)["ma1"]
    theta2 <- ifelse("ma2" %in% names(coef(model)), coef(model)["ma2"], NA)
    theta3 <- ifelse("ma3" %in% names(coef(model)), coef(model)["ma3"], NA)
    theta4 <- ifelse("ma4" %in% names(coef(model)), coef(model)["ma4"], NA)
    
    # Create a named vector with the extracted values
    c(
      AIC = round(model_metrics$AIC, 2),
      BIC = round(model_metrics$BIC, 2),
      RMSE = round(accuracy(model)[, "RMSE"], 4),
      MAE = round(accuracy(model)[, "MAE"], 4),
      MAPE = round(accuracy(model)[, "MAPE"], 4),
      `θ₁ (MA term)` = round(theta1, 4),
      `θ₂ (MA term)` = round(theta2, 4),
      `θ₃ (MA term)` = round(theta3, 4),
      `θ₄ (MA term)` = round(theta4, 4)
    )
  }
  
  # Extract metrics for each model
  arima_012_metrics <- extract_arima_metrics(arima_012)
  arima_013_metrics <- extract_arima_metrics(arima_013)
  arima_014_metrics <- extract_arima_metrics(arima_014)
  
  # Combine the results into a single data frame with models as columns
  results <- data.frame(
    Parameter = c("AIC", "BIC", "RMSE", "MAE", "MAPE", "θ₁ (MA term)", "θ₂ (MA term)", "θ₃ (MA term)", "θ₄ (MA term)"),
    `ARIMA(0,1,2)` = arima_012_metrics,
    `ARIMA(0,1,3)` = arima_013_metrics,
    `ARIMA(0,1,4)` = arima_014_metrics
  )
  
  # Display the results in a clean table format with models as columns
  kable(results, caption = "ARIMA Model Comparison", align = "c") %>%
    kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
    column_spec(1, bold = T, width = "10em") %>%
    column_spec(2:ncol(results), width = "8em")
  
  
# Extract residuals from each model
residuals_012 <- residuals(arima_012)
residuals_013 <- residuals(arima_013)
residuals_014 <- residuals(arima_014)
Perform Ljung-Box test on the residuals of ARIMA(0,1,2)
ljung_box_012 <- Box.test(residuals_414, lag = 4, type = "Ljung-Box")
print(ljung_box_012)

# Perform Ljung-Box test on the residuals of ARIMA(0,1,3)
ljung_box_013 <- Box.test(residuals_314, lag = 5, type = "Ljung-Box")
print(ljung_box_013)

# Perform Ljung-Box test on the residuals of ARIMA(0,1,4)
ljung_box_014 <- Box.test(residuals_213, lag = 6, type = "Ljung-Box")
print(ljung_box_014)  
# Perform Ljung-Box test on the residuals of ARIMA(0,1,3)
ljung_box_013 <- Box.test(residuals_013, lag = 5, type = "Ljung-Box")

# Create a data frame with the Ljung-Box test results for ARIMA(0,1,3)
ljung_test_table <- data.frame(
  Model = "ARIMA(0,1,3)",
  Statistic = round(ljung_box_013$statistic, 4),
  df = ljung_box_013$parameter,
  p_value = round(ljung_box_013$p.value, 4)
)

# Display the Ljung-Box test result in a nice table format
library(knitr)
library(kableExtra)

ljung_test_table %>%
  kable(col.names = c("Model", "Statistic", "df", "p-value"), 
        caption = "Ljung-Box Test Result for ARIMA(0,1,3)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = F, 
                position = "center")


residuals_013 <- arima_013$residuals  
ljung_box_013 <- Box.test(residuals_013, lag = 5, type = "Ljung-Box")
plot(residuals_013, type = "o", main = "Residuals of ARIMA(0,1,3)", 
     ylab = "Residuals", xlab = "Time", col = "blue")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at zero for reference


# ACF plot of the residuals
acf(residuals_013, main = "ACF of Residuals", lag.max = 5)
print(ljung_box_013)
tsdiag(arima_013)
   
#Qn8

# Load necessary libraries
library(forecast)
library(knitr)
library(kableExtra)

# Assuming your differenced GDP data is already available as `gdp_diff`
T <- length(gdp_diff)  # Total length of the time series

# Split the data into training and testing sets
train_data <- gdp_diff[1:(T-6)]   # Use first T-6 observations for training
test_data <- gdp_diff[(T-5):T]    # Use last 6 observations for testing

# Step 1: Forecast the next 6 observations using each ARIMA model
forecast_012 <- forecast(arima_012, h = 6)  # ARIMA(0,1,2)
forecast_013 <- forecast(arima_013, h = 6)  # ARIMA(0,1,3)
forecast_014 <- forecast(arima_014, h = 6)  # ARIMA(0,1,4)

# Step 2: Define the loss function (MAE)
evaluate_forecast_mae <- function(predicted, actual) {
  mae <- mean(abs(predicted - actual))  # Mean Absolute Error
  return(mae)
}

# Step 3: Evaluate each forecast using MAE
mae_012 <- evaluate_forecast_mae(forecast_012$mean, test_data)
mae_013 <- evaluate_forecast_mae(forecast_013$mean, test_data)
mae_014 <- evaluate_forecast_mae(forecast_014$mean, test_data)

# Step 4: Create a table summarizing the MAE results
mae_table <- data.frame(
  Model = c("ARIMA(0,1,2)", "ARIMA(0,1,3) (Best Model)", "ARIMA(0,1,4)"),
  MAE = round(c(mae_012, mae_013, mae_014), 2)
)

# Print the table in a nice report-friendly format
mae_table %>%
  kable(col.names = c("Model", "Mean Absolute Error (MAE)"), 
        align = c("l", "c"), caption = "MAE Comparison for ARIMA Models") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center")

# Step 5: Determine the best model based on the lowest MAE
best_model <- ifelse(mae_013 < mae_012 & mae_013 < mae_014, "ARIMA(0,1,3)", 
                     ifelse(mae_012 < mae_014, "ARIMA(0,1,2)", "ARIMA(0,1,4)"))
cat("Best model based on MAE is:", best_model)

# Create a data frame with the forecasted values and MAE
forecast_table <- data.frame(
  Model = c("ARIMA(0,1,2)", "ARIMA(0,1,3) (Best Model)", "ARIMA(0,1,4)"),
  Forecast_1 = c(forecast_012$mean[1], forecast_013$mean[1], forecast_014$mean[1]),
  Forecast_2 = c(forecast_012$mean[2], forecast_013$mean[2], forecast_014$mean[2]),
  Forecast_3 = c(forecast_012$mean[3], forecast_013$mean[3], forecast_014$mean[3]),
  Forecast_4 = c(forecast_012$mean[4], forecast_013$mean[4], forecast_014$mean[4]),
  Forecast_5 = c(forecast_012$mean[5], forecast_013$mean[5], forecast_014$mean[5]),
  Forecast_6 = c(forecast_012$mean[6], forecast_013$mean[6], forecast_014$mean[6]),
  MAE = round(c(mae_012, mae_013, mae_014), 2)  # Include rounded MAE values
)

# Print the table in a user-friendly format
forecast_table %>%
  kable(col.names = c("Model", "Forecast 1", "Forecast 2", "Forecast 3", 
                      "Forecast 4", "Forecast 5", "Forecast 6", "Mean Absolute Error (MAE)"),
        align = c("l", "c", "c", "c", "c", "c", "c", "c"), 
        caption = "Forecasted Values and MAE for ARIMA Models") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center")

# Step 4: Visualize the forecasts along with actual values

# Plot for ARIMA(0,1,2)
autoplot(forecast_012) +
  autolayer(test_ts, series = "Actual", PI = FALSE) +
  ggtitle("Forecast vs Actual: ARIMA(0,1,2)") +
  xlab("Time") + ylab("GDP Diff") +
  theme_minimal()

# Plot for ARIMA(0,1,3)
autoplot(forecast_013) +
  autolayer(test_ts, series = "Actual", PI = FALSE) +
  ggtitle("Forecast vs Actual: ARIMA(0,1,3)") +
  xlab("Time") + ylab("GDP Diff") +
  theme_minimal()

# Plot for ARIMA(0,1,4)
autoplot(forecast_014) +
  autolayer(test_ts, series = "Actual", PI = FALSE) +
  ggtitle("Forecast vs Actual: ARIMA(0,1,4)") +
  xlab("Time") + ylab("GDP Diff") +
  theme_minimal()

# Assuming forecast_012, forecast_013, and forecast_014 are already created
# Step 1: Create the combined forecast as the simple average of ARIMA(0,1,2) and ARIMA(0,1,4)
combined_forecast <- (forecast_012$mean + forecast_014$mean) / 2

# Define a function to calculate only MAE
evaluate_forecast_mae <- function(predicted, actual) {
  mae <- mean(abs(predicted - actual))  # Mean Absolute Error
  return(mae)
}

# Calculate MAE for each forecast and the combined forecast
mae_012 <- evaluate_forecast_mae(forecast_012$mean, test_data)
mae_013 <- evaluate_forecast_mae(forecast_013$mean, test_data)  # Best model based on MAE
mae_014 <- evaluate_forecast_mae(forecast_014$mean, test_data)
mae_combined <- evaluate_forecast_mae(combined_forecast, test_data)

# Print the MAE results for each model and the combined forecast
cat("MAE for ARIMA(0,1,2):", mae_012, "\n")
cat("MAE for ARIMA(0,1,3) (Best Model):", mae_013, "\n")
cat("MAE for ARIMA(0,1,4):", mae_014, "\n")
cat("MAE for Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4)):", mae_combined, "\n")
# Create a data frame with the forecasted values and MAE for both the best model and combined forecast
comparison_table <- data.frame(
  Model = c("ARIMA(0,1,3) (Best Model)", "Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4))"),
  Forecast_1 = c(forecast_013$mean[1], combined_forecast[1]),
  Forecast_2 = c(forecast_013$mean[2], combined_forecast[2]),
  Forecast_3 = c(forecast_013$mean[3], combined_forecast[3]),
  Forecast_4 = c(forecast_013$mean[4], combined_forecast[4]),
  Forecast_5 = c(forecast_013$mean[5], combined_forecast[5]),
  Forecast_6 = c(forecast_013$mean[6], combined_forecast[6]),
  MAE = round(c(mae_best_model, mae_combined), 2)  # Include rounded MAE values
)

# Display the table in a report-friendly format
library(knitr)
library(kableExtra)

comparison_table %>%
  kable(col.names = c("Model", "Forecast 1", "Forecast 2", "Forecast 3", 
                      "Forecast 4", "Forecast 5", "Forecast 6", "Mean Absolute Error (MAE)"),
        align = c("l", "c", "c", "c", "c", "c", "c", "c"), 
        caption = "Table IV Best Model and Combined Forecast Using MAE") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center")

# Step 2: Plot the Combined Forecast vs Actual values
library(ggplot2)
autoplot(ts(combined_forecast, start = (T - 5), frequency = 12)) + 
  autolayer(test_ts, series = "Actual", PI = FALSE) + 
  ggtitle("Forecast vs Actual: Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4))") + 
  xlab("Time") + 
  ylab("GDP Diff") + 
  theme_minimal()
---------------------------------------------------------------------------------
# Forecast values
cat("Forecasted values for ARIMA(0,1,2):\n")
print(forecast_012$mean)

cat("Forecasted values for ARIMA(0,1,3) (Best Model):\n")
print(forecast_013$mean)

cat("Forecasted values for ARIMA(0,1,4):\n")
print(forecast_014$mean)

# Print the Combined Forecast
cat("Combined Forecast (Simple Average of ARIMA(0,1,2) and ARIMA(0,1,4)):\n")
print(combined_forecast)

View(gdp_diff_df)

QN10
# Forecast the next 6 observations for each model
forecast_012 <- forecast(arima_012, h = 6)  # ARIMA(0,1,2)
forecast_013 <- forecast(arima_013, h = 6)  # ARIMA(0,1,3) - Best Model
forecast_014 <- forecast(arima_014, h = 6)  # ARIMA(0,1,4)
# Simple average (combined forecast) for the next 6 observations
combined_forecast <- (forecast_012$mean + forecast_014$mean) / 2
# Define the MAE function
evaluate_forecast_mae <- function(predicted, actual) {
  mae <- mean(abs(predicted - actual))  # Mean Absolute Error
  return(mae)
}
# Create a data frame with the forecasted values and MAE
comparison_table <- data.frame(
  Model = c("ARIMA(0,1,3) (Best Model)", "Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4))"),
  Forecast_1 = c(forecast_013$mean[1], combined_forecast[1]),
  Forecast_2 = c(forecast_013$mean[2], combined_forecast[2]),
  Forecast_3 = c(forecast_013$mean[3], combined_forecast[3]),
  Forecast_4 = c(forecast_013$mean[4], combined_forecast[4]),
  Forecast_5 = c(forecast_013$mean[5], combined_forecast[5]),
  Forecast_6 = c(forecast_013$mean[6], combined_forecast[6]),
  MAE = round(c(mae_best_model, mae_combined), 2)
)

# Display the table in a report-friendly format
# Create a data frame with the forecasted values and MAE for both the best model and combined forecast
comparison_table <- data.frame(
  Model = c("ARIMA(0,1,3) (Best Model)", "Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4))"),
  Forecast_1 = c(forecast_013$mean[1], combined_forecast[1]),
  Forecast_2 = c(forecast_013$mean[2], combined_forecast[2]),
  Forecast_3 = c(forecast_013$mean[3], combined_forecast[3]),
  Forecast_4 = c(forecast_013$mean[4], combined_forecast[4]),
  Forecast_5 = c(forecast_013$mean[5], combined_forecast[5]),
  Forecast_6 = c(forecast_013$mean[6], combined_forecast[6]),
  MAE = round(c(mae_best_model, mae_combined), 2)  # Include rounded MAE values
)

# Display the table in a report-friendly format
library(knitr)
library(kableExtra)

comparison_table %>%
  kable(col.names = c("Model", "Forecast 1", "Forecast 2", "Forecast 3", 
                      "Forecast 4", "Forecast 5", "Forecast 6", "Mean Absolute Error (MAE)"),
        align = c("l", "c", "c", "c", "c", "c", "c", "c"), 
        caption = "Comparison of Best Model and Combined Forecast Using MAE") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center")

# Evaluate MAE for the best model (ARIMA(0,1,3))
mae_best_model <- evaluate_forecast_mae(forecast_013$mean, test_data)

# Evaluate MAE for the combined forecast
mae_combined <- evaluate_forecast_mae(combined_forecast, test_data)

# Print the MAE results for comparison
cat("MAE for ARIMA(0,1,3) (Best Model):", mae_best_model, "\n")
cat("MAE for Combined Forecast (ARIMA(0,1,2) + ARIMA(0,1,4)):", mae_combined, "\n")

#
# table
mae_table %>%
  kable(col.names = c("Model", "Mean Absolute Error (MAE)"), 
        align = c("l", "c"), caption = "MAE Comparison for ARIMA Models") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, position = "center")