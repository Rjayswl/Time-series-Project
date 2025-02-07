#qn2
install.packages("dplyr")
library(urca)
library(dplyr)
library(lubridate)
library(purrr)  # Ensure this is installed
install.packages("purrr")

# Step 1: Preprocess each dataset
# Convert `date` to Date format and align all datasets
cpi_data <- cpi_data %>%
  mutate(date = mdy(date)) %>%
  select(date, value) %>%
  rename(cpi = value)

gdp_monthly <- gdp_monthly %>%
  mutate(date = mdy(date)) %>%
  select(date, gdp)

hpi_data <- hpi_data %>%
  mutate(date = mdy(date)) %>%
  select(date, value) %>%
  rename(hpi = value)

mortgage_data <- mortgage_data %>%
  mutate(date = mdy(date)) %>%
  select(date, value) %>%
  rename(mortgage = value)



# Display the cleaned datasets
head(cpi_data)
head(mortgage_data)
head(hpi_data)
head(gdp_monthly)

# Step 2: Merge datasets on the date column
merged_data <- reduce(list(cpi_data, gdp_monthly, hpi_data, mortgage_data), full_join, by = "date")



# Step 4: Convert to time series
# Use the first date in the dataset to set the start time
start_year <- year(min(merged_data$date))
start_month <- month(min(merged_data$date))

# Create a time series object (monthly frequency)
ts_data <- ts(merged_data[, -1], start = c(start_year, start_month), frequency = 12)


#Limiting to 112 rows
# Load necessary libraries
library(urca)
library(dplyr)
library(knitr)
library(kableExtra)

# Step 1: Limit the dataset to 112 rows
merged_data <- merged_data[1:112, ]

# Step 2: Convert to time series
start_year <- year(min(merged_data$date, na.rm = TRUE))
start_month <- month(min(merged_data$date, na.rm = TRUE))

# Create a time series object (exclude the 'date' column)
ts_data <- ts(merged_data[, -1], start = c(start_year, start_month), frequency = 12)


# Load necessary libraries
library(tseries)
library(dplyr)
library(knitr)
library(kableExtra)

# Helper function to perform Engle-Granger test
engle_granger_test <- function(data, dependent_var, independent_var) {
  # Step 1: OLS regression
  formula <- as.formula(paste(dependent_var, "~", independent_var))
  ols_model <- lm(formula, data = data)
  
  # Step 2: Extract residuals
  residuals <- residuals(ols_model)
  
  # Step 3: Test residuals for stationarity (ADF test)
  adf_result <- adf.test(residuals)
  
  # Step 4: Summarize results
  data.frame(
    Dependent = dependent_var,
    Independent = independent_var,
    ADF_Statistic = adf_result$statistic,
    P_Value = adf_result$p.value,
    Decision = ifelse(adf_result$p.value < 0.05, "Co-integrated", "Not Co-integrated")
  )
}

# Variables in the dataset
variables <- c("gdp", "cpi", "mortgage")  # Independent variables to test

# Step 1: Pairwise Testing with hpi as the dependent variable
pairwise_results <- do.call(rbind, lapply(variables, function(var) {
  engle_granger_test(merged_data, "hpi", var)
}))

# Step 2: Display Results
kable(pairwise_results, caption = "Engle-Granger Co-integration Test Results (HPI as Dependent Variable)") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

#QN3
library(dplyr)

# Step 1: Align datasets by the `date` column and limit rows to 112
aligned_data <- gdp_monthly %>%
  inner_join(hpi_data, by = "date") %>%
  inner_join(mortgage_data, by = "date") %>%
  inner_join(cpi_data, by = "date") %>%
  slice(1:112)  # Limit rows to 112

# differencing using base R
stationary_data <- aligned_data
# First differences
stationary_data$gdp_diff <- c(NA, diff(stationary_data$gdp))
stationary_data$hpi_diff <- c(NA, diff(stationary_data$hpi))
stationary_data$mortgage_diff <- c(NA, diff(stationary_data$mortgage))
# First difference for CPI
stationary_data$cpi_diff <- c(NA, diff(stationary_data$cpi))

# Filter out rows with NA
stationary_data <- stationary_data[complete.cases(stationary_data[, c("gdp_diff", "hpi_diff", "mortgage_diff", "cpi_diff")]), ]

# Select relevant columns
stationary_data <- stationary_data[, c("date", "gdp_diff", "hpi_diff", "mortgage_diff", "cpi_diff")]
# Verify the updated column names
print("Updated Column Names in Stationary Data:")
print(colnames(stationary_data))
# View the resulting dataset
head(stationary_data)
# Determine the total number of rows in the dataset
T <- nrow(stationary_data)

# Load necessary libraries
library(vars)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Step 1: Split the dataset into training and test sets
T <- nrow(stationary_data)
training_set <- stationary_data[1:(T - 6), ]  # Observations 1 to (T-6)
testing_set <- stationary_data[(T - 5):T, ]      # Observations (T-5) to T

# Step 2: Prepare the training data for VAR modeling
var_data <- training_set %>%
  dplyr::select(gdp_diff, hpi_diff, mortgage_diff, cpi_diff)

# Verify the prepared data
print("Prepared Training Data for VAR Modeling:")
print(head(var_data))

# Step 3: Select optimal lag order using the training set
lag_selection <- VARselect(var_data, lag.max = 5, type = "const")
print("Optimal Lag Selection Criteria:")
print(lag_selection$selection)  # Print lag order selection criteria (AIC, HQ, SC, FPE)

# Extract the optimal lag based on AIC
optimal_lag <- lag_selection$selection["AIC(n)"]
cat("Optimal Lag (AIC):", optimal_lag, "\n")

# Step 4: Fit the VAR model using the selected lag
var_model <- VAR(var_data, p = optimal_lag, type = "const")  # Fit VAR model

# Step 5: Summarize the VAR model
cat("VAR Model Summary:\n")
summary(var_model)

# Step 6: Extract coefficients and statistics from the VAR model
coefficients_table <- lapply(names(var_model$varresult), function(dep_var) {
  model <- var_model$varresult[[dep_var]]  # Get the model for each dependent variable
  coef_summary <- summary(model)$coefficients  # Extract coefficient summary
  
  # Convert coefficient summary to a data frame
  data.frame(
    Variable = rownames(coef_summary),
    Estimate = coef_summary[, 1],
    Std_Error = coef_summary[, 2],
    t_value = coef_summary[, 3],
    P_Value = coef_summary[, 4],
    Dependent = dep_var  # Add dependent variable name
  )
}) %>% 
  bind_rows()  # Combine all data frames into one

# Pivot the table to make variables as columns
coefficients_table_pivot <- coefficients_table %>%
  select(Variable, Dependent, Estimate, t_value, P_Value) %>%
  pivot_wider(
    names_from = Dependent,
    values_from = c(Estimate, t_value, P_Value),
    names_glue = "{Dependent}_{.value}" # Custom column names
  )

# Display the table in a formatted way
coefficients_table_pivot %>%
  kable(caption = "VAR Model Coefficients and Statistics with Variables as Columns") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

# Step 7: Calculate AIC and BIC for the VAR model
log_likelihood <- logLik(var_model)  # Extract log-likelihood
num_params <- length(coef(var_model))  # Total number of parameters in the model
num_obs <- nrow(var_data) - optimal_lag  # Number of observations after considering lags

aic_value <- -2 * as.numeric(log_likelihood) + 2 * num_params  # Calculate AIC
bic_value <- -2 * as.numeric(log_likelihood) + log(num_obs) * num_params  # Calculate BIC

# Create a friendly table for AIC and BIC
aic_bic_table <- data.frame(
  Metric = c("AIC", "BIC"),
  Value = c(aic_value, bic_value)
)

# Display the AIC and BIC table
kable(aic_bic_table, caption = "AIC and BIC for the VAR Model") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

# Step 8: Generate IRF for HPI as the response variable
# Ensure that the VAR model from the training set is used
irf_hpi <- irf(var_model, 
               impulse = c("gdp_diff", "mortgage_diff", "cpi_diff"),  # Impulse variables
               response = "hpi_diff",  # Response variable
               n.ahead = 10,  # Forecast horizon
               boot = TRUE)  # Use bootstrap for confidence intervals

# Step 9: Plot the IRFs for HPI
plot(irf_hpi)


# Step 10: Extract IRF values for detailed analysis

irf_values_hpi <- as.data.frame(irf_hpi$irf)
print("Extracted IRF Values for HPI:")
print(head(irf_values_hpi))  # Display the first few rows of the extracted values

# Step 11: Compute FEVD
# Compute Forecast Error Variance Decomposition for the VAR model
fevd_hpi <- fevd(var_model, n.ahead = 10)  # FEVD results for all variables

# Step 12: Extract FEVD results for HPI
fevd_hpi_table <- data.frame(
  Horizon = 1:10,  # Forecast horizons
  GDP = fevd_hpi$gdp_diff[, "hpi_diff"],          # Contribution from GDP
  Mortgage = fevd_hpi$mortgage_diff[, "hpi_diff"],  # Contribution from Mortgage Rates
  CPI = fevd_hpi$cpi_diff[, "hpi_diff"],            # Contribution from CPI
  HPI_Self = fevd_hpi$hpi_diff[, "hpi_diff"]        # Contribution from HPI itself
)

# Step 13: Normalize rows to ensure they sum to 1
fevd_hpi_table <- fevd_hpi_table %>%
  mutate(across(-Horizon, ~ . / rowSums(across(-Horizon))))

# Step 14: Round values to 3 decimal places
fevd_hpi_table <- fevd_hpi_table %>%
  mutate(across(-Horizon, ~ round(., 3)))

# Step 15: Print friendly table with limited decimals
kable(fevd_hpi_table, caption = "Normalized FEVD: Contributions to Housing Price Variance (Rounded to 3 Decimal Places)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")  # Highlight header row

# Step 16: Reshape data to long format for plotting
fevd_long <- melt(fevd_hpi_table, id.vars = "Horizon", variable.name = "Variable", value.name = "Contribution")

# Step 17: Plot FEVD results using ggplot2
fevd_plot <- ggplot(fevd_long, aes(x = Horizon, y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Forecast Error Variance Decomposition (HPI)",
    x = "Horizon (Periods Ahead)",
    y = "Contribution to Variance (%)",
    fill = "Variable"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Display the plot
print(fevd_plot)


#VII
# Load necessary libraries
library(forecast)
library(tseries)
library(dplyr)
library(vars)
library(kableExtra)

# Step 1: Prepare the Data
# Ensure differencing is applied
gdp_diff <- diff(gdp_monthly$gdp)  # First differencing for stationarity
T <- length(gdp_diff)

# Split into training and test sets
train_data <- gdp_diff[1:(T-6)]   # Training data: 1 to T-6
test_data <- gdp_diff[(T-5):T]    # Test data: Last 6 observations

# Step 2: ARIMA(0,1,3) Model
# Fit ARIMA model
arima_013 <- Arima(train_data, order = c(0, 1, 3))
print(arima_013)

# Forecast for the next 6 periods
arima_forecasts <- forecast(arima_013, h = 6)

# Evaluate ARIMA using MAE
arima_mae <- mean(abs(test_data - arima_forecasts$mean))

# Create ARIMA results table
arima_results_table <- data.frame(
  Period = 1:6,
  Actual = test_data,
  Forecast = arima_forecasts$mean,
  Error = test_data - arima_forecasts$mean,
  Absolute_Error = abs(test_data - arima_forecasts$mean),
  MAE = arima_mae
)

# Display ARIMA results table
kable(arima_results_table, caption = "ARIMA(0,1,3) Model Forecasts and Errors") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

# Step 3: VAR Model
# Forecast using the VAR model
var_forecast <- predict(var_model, n.ahead = 6)

# Extract actual test data for VAR (handle as list or data frame)
if (is.list(var_data)) {
  test_actual <- as.numeric(var_data$gdp_diff[(length(var_data$gdp_diff) - 5):length(var_data$gdp_diff)])
} else {
  test_actual <- as.numeric(var_data[(nrow(var_data) - 5):nrow(var_data), "gdp_diff"])
}

# Extract VAR forecasts
gdp_var_forecast <- as.numeric(var_forecast$fcst$gdp_diff[, 1])

# Calculate MAE for VAR forecasts
var_mae <- mean(abs(test_actual - gdp_var_forecast))

# Create VAR results table
var_results_table <- data.frame(
  Period = 1:6,
  Actual = test_actual,
  Forecast = gdp_var_forecast,
  Error = test_actual - gdp_var_forecast,
  Absolute_Error = abs(test_actual - gdp_var_forecast),
  MAE = var_mae
)

# Display VAR results table
kable(var_results_table, caption = "VAR Model Forecasts and Errors") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

# Step 4: Comparison of VAR and ARIMA
# Create comparison table for MAE
comparison_table <- data.frame(
  Model = c("VAR", "ARIMA"),
  MAE = c(var_mae, arima_mae)
)

# Display comparison table
kable(comparison_table, caption = "MAE Comparison Between VAR and ARIMA Models") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(0, bold = TRUE, background = "#D3D3D3")

ggplot(forecast_comparison, aes(x = Period)) +
  geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = ARIMA_Forecast, color = "ARIMA Forecast"), linewidth = 1, linetype = "dashed") +
  geom_line(aes(y = VAR_Forecast, color = "VAR Forecast"), linewidth = 1, linetype = "dotted") +
  labs(title = "Comparison of Actual Values and Forecasts",
       x = "Period",
       y = "gdp_diff",
       color = "Legend") +
  theme_minimal()

